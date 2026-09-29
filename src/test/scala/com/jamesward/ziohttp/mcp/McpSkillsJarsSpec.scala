package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.client.*
import zio.*
import zio.http.*
import zio.json.ast.Json
import zio.test.*
import zio.test.TestAspect.*

import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.security.MessageDigest

// Exercises McpSkillsJars against real SkillsJars pulled in as test-scope dependencies
// (see build.sbt): com.skillsjars:anthropics__skills__pdf and
// com.skillsjars:anthropics__skills__brand-guidelines.
object McpSkillsJarsSpec extends ZIOSpecDefault:
  private val pdfUri = "skill://anthropics/skills/pdf/SKILL.md"
  private val brandUri = "skill://anthropics/skills/brand-guidelines/SKILL.md"

  private def classpathBytes(path: String): Array[Byte] =
    val stream = getClass.getClassLoader.getResourceAsStream(path)
    try stream.readAllBytes()
    finally stream.close()

  private def sha256(bytes: Array[Byte]): String =
    "sha256:" + MessageDigest.getInstance("SHA-256").digest(bytes).map(b => f"${b & 0xff}%02x").mkString

  private def staticResources(entry: McpSkillEntry): Chunk[McpSkillResource] = entry.resources match
    case McpSkillResources.Static(values) => values.toChunk
    case McpSkillResources.Dynamic        => Chunk.empty

  private def entry(jars: McpSkillsJars, uri: String): IO[String, McpSkillEntry] =
    ZIO.fromOption(jars.entries.find(_.uri.value == uri)).orElseFail(s"Missing $uri")

  private def config(port: Int, version: ProtocolVersion): McpClientConfig =
    McpClientConfig(
      s"http://localhost:$port/mcp",
      preferredVersion = version,
      clientInfo = Implementation("skillsjars-client", "1.0.0"),
    )

  private def errorCode(error: McpClientError): Option[Int] = error match
    case McpClientError.JsonRpc(code, _, _) => Some(code)
    case _                                  => None

  private def exercise(version: ProtocolVersion): ZIO[Server & Client & McpServer.State, Any, TestResult] =
    ZIO.scoped:
      for
        jars      <- McpSkillsJars.load
        server     = McpServer("skillsjars", "1.0.0")
                       .withExtensions(jars.extensions)
                       .resourceSource(jars.resources)
        port      <- Server.install(server.routes)
        client    <- McpClient.connect(config(port, version), McpClientExtensions.empty)
        skills    <- ZIO.fromEither(McpSkillsClient.from(client))
        page      <- skills.list()
        uri       <- ZIO.fromEither(McpSkillUri.parse(pdfUri))
        fetched   <- skills.get(uri)
        body      <- skills.readSkill(uri)
        root      <- skills.readDirectory("skill://anthropics/skills/pdf")
        scripts   <- skills.readDirectory("skill://anthropics/skills/pdf/scripts")
        scriptUri <- ZIO.fromEither(McpSkillResourceUri.parse(
                       "skill://anthropics/skills/pdf/scripts/check_fillable_fields.py"
                     ))
        script    <- skills.readResource(scriptUri)
        listed    <- client.listResources
        missing   <- ZIO.fromEither(McpSkillUri.parse("skill://anthropics/skills/missing/SKILL.md"))
        unknown   <- skills.get(missing).flip
        notDir    <- skills.readDirectory("skill://anthropics/skills/pdf/forms.md").flip
      yield
        val bodyText = body.headOption.flatMap(_.text).getOrElse("")
        assertTrue(
          page.skills.map(_.uri.value).toSet == Set(pdfUri, brandUri),
          page.nextCursor.isEmpty,
          fetched == jars.entries.find(_.uri.value == pdfUri).get,
          bodyText.startsWith("---"),
          bodyText.contains("name: pdf"),
          body.headOption.flatMap(_.mimeType).contains("text/markdown"),
          root.resources.find(_.name == "scripts").flatMap(_.mimeType).contains("inode/directory"),
          root.resources.find(_.name == "SKILL.md").map(_.uri).contains(pdfUri),
          root.resources.exists(_.name == "forms.md"),
          scripts.resources.nonEmpty,
          scripts.resources.forall(_.name.endsWith(".py")),
          scripts.resources.exists(_.uri == scriptUri.value),
          script.headOption.flatMap(_.text).exists(_.nonEmpty),
          script.headOption.flatMap(_.mimeType).contains("text/x-python"),
          listed.map(_.uri).toSet == Set(pdfUri, brandUri),
          listed.find(_.uri == pdfUri).flatMap(_.description).exists(_.contains("PDF")),
          errorCode(unknown).contains(ErrorCode.InvalidParams.code),
          errorCode(notDir).contains(ErrorCode.InvalidParams.code),
        )

  private def writeSkill(root: Path, path: String, content: String): Task[Unit] =
    ZIO.attemptBlocking:
      val file = root.resolve(path)
      Files.createDirectories(file.getParent)
      Files.writeString(file, content)
      ()

  override def spec =
    suite("McpSkillsJars")(
      test("loads the SkillsJars on the test classpath"):
        for
          jars <- McpSkillsJars.load
          pdf  <- entry(jars, pdfUri)
          _    <- entry(jars, brandUri)
        yield
          val resources = staticResources(pdf)
          assertTrue(
            jars.entries.map(_.uri.value).toSet == Set(pdfUri, brandUri),
            jars.skipped.isEmpty,
            pdf.frontmatter.value.get("name").contains(Json.Str("pdf")),
            pdf.frontmatter.value.get("license").flatMap(_.asString).exists(_.contains("LICENSE.txt")),
            resources.head.uri.value == pdfUri,
            resources.exists(_.uri.value == "skill://anthropics/skills/pdf/scripts/check_fillable_fields.py"),
          )
      ,
      test("resource digests and sizes match the jar contents"):
        for
          jars <- McpSkillsJars.load
          pdf  <- entry(jars, pdfUri)
        yield
          val checks = staticResources(pdf).map: resource =>
            val path = "META-INF/skills/" + resource.uri.value.stripPrefix("skill://")
            val bytes = classpathBytes(path)
            resource.digest.value == sha256(bytes) && resource.size.bytes == bytes.length.toLong
          assertTrue(checks.size > 1, checks.forall(ok => ok))
      ,
      test("legacy loopback: list/get/read/directory over the Skills extension"):
        exercise(ProtocolVersion.V2025_11_25)
      ,
      test("modern loopback: list/get/read/directory over the Skills extension"):
        exercise(ProtocolVersion.V2026_07_28)
      ,
      test("README SkillsJars example serves classpath skills"):
        ZIO.scoped:
          for
            jars   <- McpSkillsJars.load
            server  = McpServer("skills", "1.0.0")
                        .withExtensions(jars.extensions)
                        .resourceSource(jars.resources)
            port   <- Server.install(server.routes)
            skills <- McpSkillsClient.connect(config(port, ProtocolVersion.V2026_07_28))
            page   <- skills.list()
          yield assertTrue(page.skills.map(_.uri.name.value).toSet == Set("pdf", "brand-guidelines"))
      ,
      test("directory classpath entries load, and invalid skills are skipped"):
        ZIO.scoped:
          for
            dir <- ZIO.acquireRelease(ZIO.attemptBlocking(Files.createTempDirectory("skillsjars")))(dir =>
                     ZIO.attemptBlocking(
                       Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete(_))
                     ).orDie
                   )
            _   <- writeSkill(dir, "META-INF/skills/acme/tools/good-skill/SKILL.md",
                     """---
                       |name: good-skill
                       |description: >
                       |  Folded multi-line
                       |  description.
                       |metadata:
                       |  version: "2"
                       |---
                       |# Good
                       |""".stripMargin)
            _   <- writeSkill(dir, "META-INF/skills/acme/tools/good-skill/refs/notes.md", "notes")
            _   <- writeSkill(dir, "META-INF/skills/acme/tools/bad-skill/SKILL.md",
                     "---\nname: other-name\ndescription: mismatch\n---\n")
            _   <- writeSkill(dir, "META-INF/skills/acme/tools/no-frontmatter/SKILL.md", "# nothing\n")
            loader = new URLClassLoader(Array(dir.toUri.toURL), null)
            jars <- McpSkillsJars.load(loader)
            good <- entry(jars, "skill://acme/tools/good-skill/SKILL.md")
            ctx   = McpRequestContext(ProtocolVersion.V2026_07_28)
            refs <- jars.directory.read(ResourceDirectoryReadParams("skill://acme/tools/good-skill/refs"), ctx)
          yield assertTrue(
            jars.entries.size == 1,
            good.frontmatter.value.get("description").contains(Json.Str("Folded multi-line description.\n")),
            good.frontmatter.value.get("metadata").contains(Json.Obj("version" -> Json.Str("2"))),
            staticResources(good).map(_.uri.value) == Chunk(
              "skill://acme/tools/good-skill/SKILL.md",
              "skill://acme/tools/good-skill/refs/notes.md",
            ),
            staticResources(good).last.digest.value == sha256("notes".getBytes(StandardCharsets.UTF_8)),
            refs.resources.map(_.uri) == Chunk("skill://acme/tools/good-skill/refs/notes.md"),
            jars.skipped.map(_.path).toSet == Set(
              "META-INF/skills/acme/tools/bad-skill/SKILL.md",
              "META-INF/skills/acme/tools/no-frontmatter/SKILL.md",
            ),
          )
      ,
      test("frontmatter parsing rejects missing and unterminated blocks"):
        assertTrue(
          McpSkillsJars.parseFrontmatter("# no frontmatter").isLeft,
          McpSkillsJars.parseFrontmatter("---\nname: x\n").isLeft,
          McpSkillsJars.parseFrontmatter("---\n- a\n- b\n---\n").isLeft,
          McpSkillsJars.parseFrontmatter("﻿---\r\nname: x\r\n---\r\nbody").contains(
            Json.Obj("name" -> Json.Str("x"))
          ),
        )
      ,
    ).provide(
      Server.defaultWith(_.onAnyOpenPort),
      Client.default,
      McpServer.State.default,
    ) @@ withLiveClock @@ timeout(2.minutes) @@ sequential
