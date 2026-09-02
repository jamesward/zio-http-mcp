package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.client.*
import zio.*
import zio.http.*
import zio.json.ast.Json
import zio.test.*
import zio.test.TestAspect.*

object McpSkillsInteropSpec extends ZIOSpecDefault:
  private val digestText = "sha256:" + "b" * 64

  private final case class Source(entry: McpSkillEntry) extends McpSkillsSource[Any]:
    def list(
      params: McpSkillsListParams,
      ctx: McpRequestContext,
    ): IO[McpSkillsSourceError, McpSkillsListResult] =
      val _ = ctx
      params.cursor match
        case None          => ZIO.succeed(McpSkillsListResult(Chunk(entry), Some("done")))
        case Some("done") => ZIO.succeed(McpSkillsListResult(Chunk.empty))
        case Some(other)   => ZIO.fail(McpSkillsSourceError.InvalidParams(s"Unknown cursor: $other"))

    def get(
      uri: McpSkillUri,
      ctx: McpRequestContext,
    ): IO[McpSkillsSourceError, McpSkillEntry] =
      val _ = ctx
      if uri == entry.uri then ZIO.succeed(entry)
      else ZIO.fail(McpSkillsSourceError.InvalidParams(s"Unknown skill: ${uri.value}"))

  private final case class Directory(child: ResourceDefinition) extends McpSkillsDirectorySource[Any]:
    def read(
      params: ResourceDirectoryReadParams,
      ctx: McpRequestContext,
    ): IO[McpSkillsSourceError, ResourcesListResult] =
      val _ = ctx
      if params.uri == "skill://refunds/references" then
        ZIO.succeed(ResourcesListResult(Chunk(child), Some("directory-next")))
      else ZIO.fail(McpSkillsSourceError.InvalidParams(s"Not a directory resource: ${params.uri}"))

  private final case class Fixture(server: McpServer[Any], entry: McpSkillEntry)

  private def fixture: ZIO[Any, Any, Fixture] =
    for
      uri         <- ZIO.fromEither(McpSkillUri.parse("skill://refunds/SKILL.md"))
      resourceUri <- ZIO.fromEither(McpSkillResourceUri.parse(uri.value))
      digest      <- ZIO.fromEither(McpSkillDigest.parse(digestText))
      size        <- ZIO.fromEither(McpSkillSize.parse(42))
      entry       <- ZIO.fromEither(McpSkillEntry.static(
                       uri,
                       McpSkillFrontmatter(Json.Obj(
                         "name" -> Json.Str("refunds"),
                         "description" -> Json.Str("Process refunds"),
                         "metadata" -> Json.Obj("version" -> Json.Str("1")),
                       )),
                       NonEmptyChunk(McpSkillResource(resourceUri, digest, size)),
                     ))
      child        = ResourceDefinition(
                       "skill://refunds/references/policy.md",
                       "policy.md",
                       mimeType = Some("text/markdown"),
                     )
      registry     = McpSkills.withDirectory(Source(entry), Directory(child))
      resource     = McpResource(uri.value, "refunds")
                       .mimeType("text/markdown")
                       .read(requested => ZIO.succeed(Chunk(ResourceContents(
                         requested,
                         mimeType = Some("text/markdown"),
                         text = Some("# Refunds"),
                       ))))
      server       = McpServer("skills-server", "1.0.0")
                       .withExtensions(registry)
                       .resource(resource)
    yield Fixture(server, entry)

  private def config(port: Int, version: ProtocolVersion): McpClientConfig =
    McpClientConfig(
      s"http://localhost:$port/mcp",
      preferredVersion = version,
      clientInfo = Implementation("skills-client", "1.0.0"),
    )

  private def errorCode(error: McpClientError): Option[Int] = error match
    case McpClientError.JsonRpc(code, _, _) => Some(code)
    case McpClientError.Transport(_, _)      => None
    case McpClientError.Protocol(_)          => None
    case McpClientError.Decode(_)            => None
    case McpClientError.Auth(_)              => None
    case McpClientError.ToolFailed(_)        => None

  private def exercise(version: ProtocolVersion): ZIO[Server & Client & McpServer.State, Any, TestResult] =
    ZIO.scoped:
      for
        f       <- fixture
        port    <- Server.install(f.server.routes)
        client  <- McpClient.connect(config(port, version), McpClientExtensions.empty)
        skills  <- ZIO.fromEither(McpSkillsClient.from(client))
        page    <- skills.list()
        next    <- skills.list(Some("done"))
        fetched <- skills.get(f.entry.uri)
        contents <- skills.readSkill(f.entry.uri)
        directory <- skills.readDirectory("skill://refunds/references")
        rawList <- client.requestRaw(McpSkills.ListMethod, Json.Obj())
        rawGet <- client.requestRaw(
                    McpSkills.GetMethod,
                    Json.Obj("uri" -> Json.Str(f.entry.uri.value)),
                  )
        rawDirectory <- client.requestRaw(
                          McpSkills.DirectoryReadMethod,
                          Json.Obj("uri" -> Json.Str("skill://refunds/references")),
                        )
        unknownUri <- ZIO.fromEither(McpSkillUri.parse("skill://missing/SKILL.md"))
        unknown <- skills.get(unknownUri).flip
        invalid <- client.requestRaw(
                     McpSkills.GetMethod,
                     Json.Obj("uri" -> Json.Str("skill://refunds/README.md")),
                   ).flip
        invalidDirectory <- skills.readDirectory("skill://refunds/missing").flip
      yield
        val raw = rawList.asObject
        val getWire = rawGet.asObject
        val directoryWire = rawDirectory.asObject
        assertTrue(
          page.skills == Chunk(f.entry),
          page.nextCursor.contains("done"),
          next.skills.isEmpty,
          fetched == f.entry,
          contents.headOption.flatMap(_.text).contains("# Refunds"),
          directory.resources.map(_.name) == Chunk("policy.md"),
          directory.nextCursor.contains("directory-next"),
          errorCode(unknown).contains(ErrorCode.InvalidParams.code),
          errorCode(invalid).contains(ErrorCode.InvalidParams.code),
          errorCode(invalidDirectory).contains(ErrorCode.InvalidParams.code),
          raw.flatMap(_.get("resultType")).flatMap(_.asString).contains("complete") == version.isStateless,
          raw.flatMap(_.get("ttlMs")).isDefined == version.isStateless,
          raw.flatMap(_.get("cacheScope")).isDefined == version.isStateless,
          getWire.flatMap(_.get("resultType")).flatMap(_.asString).contains("complete") == version.isStateless,
          getWire.flatMap(_.get("ttlMs")).isEmpty,
          getWire.flatMap(_.get("cacheScope")).isEmpty,
          directoryWire.flatMap(_.get("resultType")).flatMap(_.asString).contains("complete") == version.isStateless,
          directoryWire.flatMap(_.get("ttlMs")).isEmpty,
          directoryWire.flatMap(_.get("cacheScope")).isEmpty,
        )

  override def spec =
    suite("Skills extension loopback")(
      test("legacy list/get/directory/errors and core resource read"):
        exercise(ProtocolVersion.V2025_11_25)
      ,
      test("modern list/get/directory/errors use server-owned cache envelope"):
        exercise(ProtocolVersion.V2026_07_28)
      ,
      test("README Skills client helper connects and reads through core resources/read"):
        ZIO.scoped:
          for
            f      <- fixture
            port   <- Server.install(f.server.routes)
            skills <- McpSkillsClient.connect(config(port, ProtocolVersion.V2026_07_28))
            page   <- skills.list()
            listed <- ZIO.fromOption(page.skills.headOption).orElseFail("No listed skills")
            entry  <- skills.get(listed.uri)
            body   <- skills.readSkill(entry.uri)
          yield assertTrue(
            entry == f.entry,
            body.headOption.flatMap(_.text).contains("# Refunds"),
          )
      ,
      test("public operation codecs round-trip Skills proxy payloads"):
        for
          f           <- fixture
          listResult   = McpSkillsListResult(Chunk(f.entry), Some("next"))
          listJson    <- ZIO.fromEither(McpSkills.listOperation.encodeResult(listResult))
          decodedList <- ZIO.fromEither(McpSkills.listOperation.decodeResult(listJson))
          getResult    = McpSkillsGetResult(f.entry)
          getJson     <- ZIO.fromEither(McpSkills.getOperation.encodeResult(getResult))
          decodedGet  <- ZIO.fromEither(McpSkills.getOperation.decodeResult(getJson))
        yield assertTrue(
          decodedList == listResult,
          decodedGet == getResult,
        )
      ,
      test("constructor without directory source advertises no directory support and client refuses the call"):
        ZIO.scoped:
          for
            f         <- fixture
            registry   = McpSkills(Source(f.entry))
            settings  <- registry.settings(McpRequestContext(ProtocolVersion.V2026_07_28))
            server     = McpServer("skills-server", "1.0.0").withExtensions(registry)
            port      <- Server.install(server.routes)
            client    <- McpClient.connect(config(port, ProtocolVersion.V2026_07_28), McpClientExtensions.empty)
            skills    <- ZIO.fromEither(McpSkillsClient.from(client))
            refusal   <- skills.readDirectory("skill://refunds/references").flip
          yield assertTrue(
            registry.values.flatMap(_.operations).map(_.operation.method).toSet == Set(McpSkills.ListMethod, McpSkills.GetMethod),
            settings.get(McpSkills.Id).contains(Json.Obj()),
            refusal == McpClientError.Protocol("Server did not advertise Skills directoryRead support"),
          )
      ,
    ).provide(
      Server.defaultWith(_.onAnyOpenPort),
      Client.default,
      McpServer.State.default,
    ) @@ withLiveClock @@ timeout(1.minute) @@ sequential
