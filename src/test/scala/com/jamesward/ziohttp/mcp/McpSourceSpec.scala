package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.client.*
import zio.*
import zio.http.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*
import zio.test.TestAspect.*

/**
 * Exercises the MCP_SPEC step-0 `zio-http-mcp` prerequisites against our own server over
 * loopback HTTP:
 *
 *   - (d) `ToolDefinition` / `ResourceDefinition` round-tripping arbitrary `_meta`.
 *   - (b) the dynamic [[McpToolSource]] / [[McpResourceSource]] hooks merging into
 *     `tools/list`, `tools/call`, `resources/list`, `resources/templates/list`,
 *     `resources/read`, and `completion/complete`.
 *   - (c) path-parameterised mounting (`mountedAtParam`) exposing the captured segment
 *     to the sources via `ctx.pathParams`.
 *   - (a) the client `listResourceTemplates` / `complete` calls and static auth headers
 *     (`McpClient.streamableHttp`).
 */
object McpSourceSpec extends ZIOSpecDefault:

  private val uiMeta: Json.Obj =
    Json.Obj(Chunk("ui" -> Json.Obj(Chunk("resourceUri" -> Json.Str("ui://widget")))))

  // A dynamic tool source whose tool name encodes the mount slug, carrying `_meta`.
  private val toolSource: McpToolSource[Any] = new McpToolSource[Any]:
    def listTools(ctx: McpToolContext): ZIO[Any, Nothing, Chunk[ToolDefinition]] =
      val slug = ctx.pathParams.getOrElse("slug", "none")
      ZIO.succeed(Chunk(
        ToolDefinition(
          name = ToolName(s"echo_$slug"),
          description = Some("dynamic echo"),
          inputSchema = Json.Obj(Chunk("type" -> Json.Str("object"))),
          meta = Some(uiMeta),
        )
      ))
    def callTool(name: ToolName, args: Option[Json.Obj], ctx: McpToolContext): ZIO[Any, Nothing, CallToolResult] =
      val slug = ctx.pathParams.getOrElse("slug", "?")
      if name.value.startsWith("echo_") then
        ZIO.succeed(CallToolResult(content = Chunk(ToolContent.text(s"called=${name.value} slug=$slug"))))
      else
        ZIO.succeed(CallToolResult(content = Chunk(ToolContent.text(s"no such tool ${name.value}")), isError = Some(true)))

  private val resourceSource: McpResourceSource[Any] = new McpResourceSource[Any]:
    def listResources(ctx: McpToolContext): ZIO[Any, Nothing, Chunk[ResourceDefinition]] =
      ZIO.succeed(Chunk(ResourceDefinition(uri = "up://res", name = "res")))
    def listResourceTemplates(ctx: McpToolContext): ZIO[Any, Nothing, Chunk[ResourceTemplateDefinition]] =
      ZIO.succeed(Chunk(ResourceTemplateDefinition(uriTemplate = "up://{id}", name = "tmpl")))
    def readResource(uri: String, ctx: McpToolContext): ZIO[Any, ToolError, Chunk[ResourceContents]] =
      ZIO.succeed(Chunk(ResourceContents(uri = uri, text = Some(s"body of $uri"))))
    override def complete(ref: CompletionRef, argument: CompletionArgument, ctx: McpToolContext): ZIO[Any, Nothing, CompletionResult] =
      ZIO.succeed(CompletionResult(CompletionValues(values = Chunk("alpha", "beta"))))

  // A metadata provider that brands the mount per captured slug: title + a
  // SEP-973 icon whose URL embeds the slug (a client MAY surface it).
  private val serverInfoSource: ServerInfoSource[Any] = ServerInfoSource { ctx =>
    val slug = ctx.pathParams.getOrElse("slug", "none")
    ZIO.succeed(Implementation(
      name        = "dyn",
      version     = "0.1.0",
      title       = Some(s"Toolbook $slug"),
      icons       = List(Icon(
        src = s"https://example.test/$slug/icon.png",
        mimeType = Some("image/png"),
        sizes = List("48x48", "96x96"),
      )),
      websiteUrl  = Some(s"https://example.test/$slug"),
      description = Some(s"Dynamic server for $slug"),
    ))
  }

  // Per-toolbook style mount: one server serving `/<slug>` for any slug.
  private val dynServer = McpServer("dyn", "0.1.0")
    .toolSource(toolSource)
    .resourceSource(resourceSource)
    .serverInfo(serverInfoSource)
    .mountedAtParam("slug")

  // A minimal fake upstream that records the inbound Authorization header and answers
  // just enough JSON-RPC for the client to initialize and list tools.
  private def fakeUpstream(authRef: Ref[Option[String]]): Routes[Any, Response] =
    val h = handler { (req: Request) =>
      for
        _    <- authRef.update(prev => prev.orElse(req.rawHeader("authorization")))
        body <- req.body.asString.orElseSucceed("{}")
      yield
        val obj    = body.fromJson[Json.Obj].toOption.getOrElse(Json.Obj())
        val idJson = obj.get("id")
        val method = obj.get("method").flatMap(_.asString).getOrElse("")
        def reply(result: Json): Response =
          Response.json(Json.Obj(Chunk(
            "jsonrpc" -> Json.Str("2.0"),
            "id"      -> idJson.getOrElse(Json.Null),
            "result"  -> result,
          )).toJson)
        method match
          case "initialize" =>
            reply(Json.Obj(Chunk(
              "protocolVersion" -> Json.Str(McpProtocol.Version),
              "capabilities"    -> Json.Obj(),
              "serverInfo"      -> Json.Obj(Chunk("name" -> Json.Str("fake"), "version" -> Json.Str("1.0"))),
            )))
          case "tools/list" =>
            reply(Json.Obj(Chunk("tools" -> Json.Arr())))
          case _ =>
            // notifications carry no id
            idJson match
              case None    => Response.status(Status.Accepted)
              case Some(_) => reply(Json.Obj())
    }
    Routes(Method.POST / "up" -> h)

  override def spec =
    suite("McpSource / path-param mount / _meta / client additions")(

      test("(d) ToolDefinition round-trips arbitrary _meta as `_meta`"):
        val td = ToolDefinition(
          name = ToolName("x"),
          inputSchema = Json.Obj(Chunk("type" -> Json.Str("object"))),
          meta = Some(uiMeta),
        )
        val json = td.toJson
        val back = json.fromJson[ToolDefinition]
        val rd = ResourceDefinition(uri = "ui://w", name = "w", meta = Some(uiMeta))
        val rdBack = rd.toJson.fromJson[ResourceDefinition]
        ZIO.succeed(assertTrue(
          json.contains("\"_meta\""),
          !json.contains("\"meta\""),
          back == Right(td),
          rdBack == Right(rd),
        ))
      ,

      test("Implementation and Icon codecs emit the MCP schema and accept 0.6.0 scalar sizes"):
        val minimal = Implementation("plain", "1.0.0")
        val full = Implementation(
          name = "full",
          version = "2.0.0",
          title = Some("Full Server"),
          icons = List(Icon(
            "https://example.test/icon.png",
            mimeType = Some("image/png"),
            sizes = List("48x48", "96x96"),
          )),
          websiteUrl = Some("https://example.test"),
          description = Some("A fully described server"),
        )
        val fullJson = full.toJson
        val fullAst = fullJson.fromJson[Json.Obj].toOption
        val sizes = fullAst.flatMap(_.get("icons")).flatMap(_.asArray).flatMap(_.headOption)
          .flatMap(_.asObject).flatMap(_.get("sizes")).flatMap(_.asArray)
          .map(_.flatMap(_.asString).toList)
        val legacy =
          """{"src":"https://example.test/legacy.png","mimeType":"image/png","sizes":"48x48"}"""
            .fromJson[Icon]
        ZIO.succeed(assertTrue(
          minimal.toJson == """{"name":"plain","version":"1.0.0"}""",
          fullJson.fromJson[Implementation] == Right(full),
          fullJson.contains("\"description\":\"A fully described server\""),
          sizes.contains(List("48x48", "96x96")),
          legacy.exists(_.sizes == List("48x48")),
        ))
      ,

      test("(c)+(b)+(d) dynamic tools carry the slug + _meta through tools/list and tools/call"):
        ZIO.scoped:
          for
            port   <- Server.install(dynServer.statelessRoutes)
            client <- McpClient.connect(s"http://localhost:$port/abc123")
            tools  <- client.listTools
            ok     <- client.callTool("echo_abc123", Json.Obj())
            bad    <- client.callTool("nope")
          yield
            val tool = tools.headOption
            assertTrue(
              tools.map(_.name.value) == Chunk("echo_abc123"),
              tool.flatMap(_.meta).contains(uiMeta),
              ok.isError.forall(!_),
              ok.content.collectFirst { case ToolContent.Text(t, _) => t }.exists(_.contains("slug=abc123")),
              bad.isError.contains(true),
            )
      ,

      test("(b) resources, templates, read, and completion delegate to the resource source"):
        ZIO.scoped:
          for
            port      <- Server.install(dynServer.statelessRoutes)
            client    <- McpClient.connect(s"http://localhost:$port/tb")
            resources <- client.listResources
            templates <- client.listResourceTemplates
            contents  <- client.readResource("up://anything")
            completed <- client.complete(CompletionRef(CompletionRefType.Resource, uri = Some("up://{id}")), CompletionArgument("id", "a"))
          yield assertTrue(
            resources.map(_.uri) == Chunk("up://res"),
            templates.map(_.uriTemplate) == Chunk("up://{id}"),
            contents.headOption.flatMap(_.text).contains("body of up://anything"),
            completed.completion.values == Chunk("alpha", "beta"),
          )
      ,

      test("(c) serverInfo metadata provider brands the mount with all implementation metadata"):
        ZIO.scoped:
          for
            port   <- Server.install(dynServer.statelessRoutes)
            client <- McpClient.connect(s"http://localhost:$port/xyz789")
          yield
            val info = client.serverInfo
            assertTrue(
              info.title.contains("Toolbook xyz789"),
              info.description.contains("Dynamic server for xyz789"),
              info.websiteUrl.contains("https://example.test/xyz789"),
              info.icons.map(_.src) == List("https://example.test/xyz789/icon.png"),
              info.icons.headOption.flatMap(_.mimeType).contains("image/png"),
              info.icons.headOption.map(_.sizes).contains(List("48x48", "96x96")),
            )
      ,

      test("(a) streamableHttp attaches the configured static auth header to every request"):
        ZIO.scoped:
          for
            authRef <- Ref.make(Option.empty[String])
            port    <- Server.install(fakeUpstream(authRef))
            url     <- ZIO.fromEither(URL.decode(s"http://localhost:$port/up")).orElseFail(McpClientError.Transport("bad url"))
            client  <- McpClient.streamableHttp(url, Headers(Header.Authorization.Bearer("sekret")))
            _       <- client.listTools
            seen    <- authRef.get
          yield assertTrue(seen.contains("Bearer sekret"))
      ,

    ).provide(
      Server.defaultWith(_.onAnyOpenPort),
      Client.default,
    ) @@ withLiveClock @@ timeout(1.minute) @@ sequential
