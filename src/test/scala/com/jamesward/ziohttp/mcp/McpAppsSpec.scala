package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.client.*
import zio.*
import zio.http.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*
import zio.test.TestAspect.*

object McpAppsSpec extends ZIOSpecDefault:
  private def uiObject(meta: Option[Json.Obj]): Option[Json.Obj] =
    meta.flatMap(_.get("ui")).flatMap(_.asObject)

  private def text(result: CallToolResult): Option[String] =
    result.content.collectFirst:
      case ToolContent.Text(value, _) => value

  override def spec =
    suite("MCP Apps")(
      test("capabilities use the stable extension ID, empty server settings, and nonempty client MIME settings"):
        for
          serverSettings <- McpApps.serverExtension.settings.resolve(
                              McpRequestContext(ProtocolVersion.V2026_07_28)
                            )
        yield
          val client = McpApps.clientExtension(McpAppsClientSettings.Html)
          val mimeTypes = client.settings.asObject.flatMap(_.get("mimeTypes")).flatMap(_.asArray)
          assertTrue(
            McpApps.Id.value == "io.modelcontextprotocol/ui",
            serverSettings.asObject.exists(_.fields.isEmpty),
            client.id == McpApps.Id,
            mimeTypes.exists(_.flatMap(_.asString) == Chunk(McpApps.HtmlMimeType)),
          )
      ,
      test("UI URI is parsed and HTML MIME is constant"):
        assertTrue(
          McpUiUri.parse("ui://weather/dashboard").isRight,
          McpUiUri.parse("https://weather/dashboard").isLeft,
          McpUiUri.parse("ui:weather").isLeft,
          McpUiUri.parse("ui:").isLeft,
          McpUiUri.parse("ui://?theme=dark").isLeft,
          McpUiUri.parse("ui:///").isLeft,
          McpApps.HtmlMimeType == "text/html;profile=mcp-app",
        )
      ,
      test("typed tool metadata is nested, preserves unknown metadata, and never emits deprecated flat form"):
        for
          uri <- ZIO.fromEither(McpUiUri.parse("ui://weather/dashboard"))
          base = ToolDefinition(
            ToolName("weather"),
            inputSchema = Json.Obj(),
            meta = Some(Json.Obj(
              "vendor" -> Json.Str("kept"),
              "ui" -> Json.Obj("future" -> Json.Bool(true)),
            )),
          )
          decorated = McpApps.withToolMetadata(
            base,
            McpAppsToolMeta(uri, NonEmptyChunk(McpAppsVisibility.App)),
          )
          ui = uiObject(decorated.meta)
        yield assertTrue(
          decorated.meta.flatMap(_.get("vendor")).contains(Json.Str("kept")),
          decorated.meta.flatMap(_.get("ui/resourceUri")).isEmpty,
          ui.flatMap(_.get("future")).contains(Json.Bool(true)),
          ui.flatMap(_.get("resourceUri")).flatMap(_.asString).contains(uri.value),
          ui.flatMap(_.get("visibility")).flatMap(_.asArray)
            .exists(_.flatMap(_.asString) == Chunk("app")),
        )
      ,
      test("typed Apps resource definition and contents preserve unknown metadata at both locations"):
        for
          uri <- ZIO.fromEither(McpUiUri.parse("ui://weather/dashboard"))
          metadata = McpAppsResourceMeta(domain = Some("weather.example.com"))
          baseMeta = Some(Json.Obj(
            "vendor" -> Json.Str("kept"),
            "ui" -> Json.Obj("future" -> Json.Arr(Json.Num(1), Json.Str("two"))),
          ))
          definition = McpApps.withResourceMetadata(
            ResourceDefinition(uri.value, "weather", meta = baseMeta),
            metadata,
          )
          contents = McpApps.withResourceMetadata(
            ResourceContents(uri.value, text = Some("<!doctype html>"), meta = baseMeta),
            metadata,
          )
          definitionUi = uiObject(definition.meta)
          contentsUi = uiObject(contents.meta)
        yield assertTrue(
          definition.meta.flatMap(_.get("vendor")).contains(Json.Str("kept")),
          contents.meta.flatMap(_.get("vendor")).contains(Json.Str("kept")),
          definitionUi.flatMap(_.get("future")).contains(Json.Arr(Json.Num(1), Json.Str("two"))),
          contentsUi.flatMap(_.get("future")).contains(Json.Arr(Json.Num(1), Json.Str("two"))),
          definitionUi.flatMap(_.get("domain")).flatMap(_.asString).contains("weather.example.com"),
          contentsUi.flatMap(_.get("domain")).flatMap(_.asString).contains("weather.example.com"),
        )
      ,
      test("typed Apps resource definition and contents preserve CSP, permissions, domain, and border metadata"):
        for
          uri <- ZIO.fromEither(McpUiUri.parse("ui://weather/dashboard"))
          metadata = McpAppsResourceMeta(
            csp = Some(McpAppsCsp(
              connectDomains = Chunk("https://api.example.com"),
              resourceDomains = Chunk("https://cdn.example.com"),
              frameDomains = Chunk("https://video.example.com"),
              baseUriDomains = Chunk("https://base.example.com"),
            )),
            permissions = Set(McpAppsPermission.Camera, McpAppsPermission.ClipboardWrite),
            domain = Some("weather.example.com"),
            prefersBorder = Some(true),
          )
          definition = McpApps.resource(uri, "weather", metadata)
          contents = McpApps.contents(uri, "<!doctype html><h1>Weather</h1>", metadata)
          encoded <- ZIO.fromEither(contents.toJsonAST)
          decoded <- ZIO.fromEither(encoded.as[ResourceContents])
          ui = uiObject(decoded.meta)
          csp = ui.flatMap(_.get("csp")).flatMap(_.asObject)
          permissions = ui.flatMap(_.get("permissions")).flatMap(_.asObject)
        yield assertTrue(
          definition.mimeType.contains(McpApps.HtmlMimeType),
          decoded == contents,
          csp.flatMap(_.get("connectDomains")).flatMap(_.asArray)
            .exists(_.flatMap(_.asString) == Chunk("https://api.example.com")),
          permissions.flatMap(_.get("camera")).flatMap(_.asObject).isDefined,
          permissions.flatMap(_.get("clipboardWrite")).flatMap(_.asObject).isDefined,
          ui.flatMap(_.get("domain")).flatMap(_.asString).contains("weather.example.com"),
          ui.flatMap(_.get("prefersBorder")).flatMap(_.asBoolean).contains(true),
        )
      ,
      test("CallToolResult metadata round-trips losslessly with meaningful core text fallback"):
        for
          fallback <- ZIO.fromEither(McpAppsFallbackText.parse("Weather is 72°F and sunny"))
          result = McpApps.result(
            fallback,
            structuredContent = Some(Json.Obj("temperature" -> Json.Num(72))),
            metadata = Json.Obj(
              "timestamp" -> Json.Str("2026-01-26T00:00:00Z"),
              "unknown" -> Json.Arr(Json.Num(1), Json.Str("two")),
            ),
          )
          encoded <- ZIO.fromEither(result.toJsonAST)
          decoded <- ZIO.fromEither(encoded.as[CallToolResult])
        yield assertTrue(
          decoded == result,
          text(decoded).contains("Weather is 72°F and sunny"),
          decoded.meta.flatMap(_.get("unknown")).contains(Json.Arr(Json.Num(1), Json.Str("two"))),
        )
      ,
      test("core-only client receives fallback and ui iframe-host methods are not routed over HTTP"):
        ZIO.scoped:
          for
            uri      <- ZIO.fromEither(McpUiUri.parse("ui://weather/dashboard"))
            fallback <- ZIO.fromEither(McpAppsFallbackText.parse("Weather is sunny"))
            handler   = McpApps.tool(
                          McpTool("weather").handle(ZIO.succeed(McpApps.result(fallback))),
                          McpAppsToolMeta(uri),
                        )
            resource  = McpApps.resourceHandler(uri, "weather", McpAppsResourceMeta())(
                          ZIO.succeed("<!doctype html><h1>Weather</h1>")
                        )
            registry <- ZIO.fromEither(McpExtensions(McpApps.serverExtension))
            server    = McpServer("apps-server", "1.0.0")
                          .withExtensions(registry)
                          .tool(handler)
                          .resource(resource)
            port     <- Server.install(server.routes)
            client   <- McpClient.connect(
                          McpClientConfig(
                            s"http://localhost:$port/mcp",
                            preferredVersion = ProtocolVersion.V2026_07_28,
                          ),
                          McpClientExtensions.empty,
                        )
            tools    <- client.listTools
            result   <- client.callTool("weather")
            contents <- client.readResource(uri.value)
            uiMethod <- ZIO.fromEither(McpMethodName.parse("ui/initialize"))
            uiError  <- client.requestRaw(uiMethod, Json.Obj()).flip
          yield assertTrue(
            text(result).contains("Weather is sunny"),
            tools.headOption.flatMap(tool => uiObject(tool.meta)).flatMap(_.get("resourceUri"))
              .flatMap(_.asString).contains(uri.value),
            contents.headOption.flatMap(_.meta).flatMap(_.get("ui")).isDefined,
            uiError match
              case McpClientError.JsonRpc(code, _, _) => code == ErrorCode.MethodNotFound.code
              case McpClientError.Transport(_, _)      => false
              case McpClientError.Protocol(_)          => false
              case McpClientError.Decode(_)            => false
              case McpClientError.Auth(_)              => false
              case McpClientError.ToolFailed(_)        => false,
          )
      ,
    ).provide(
      Server.defaultWith(_.onAnyOpenPort),
      Client.default,
      McpServer.State.default,
    ) @@ withLiveClock @@ timeout(1.minute) @@ sequential
