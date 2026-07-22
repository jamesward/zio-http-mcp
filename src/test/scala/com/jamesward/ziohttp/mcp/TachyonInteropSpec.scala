package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.client.*
import zio.*
import zio.http.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*
import zio.test.TestAspect.*

import dev.tachyonmcp.server.TachyonServer
import dev.tachyonmcp.server.features.tools.{ToolHandler, ToolResult}

given canEqualStatusTachyon: CanEqual[Status, Status] = CanEqual.derived

/**
 * Cross-implementation interop against [[https://github.com/kpavlov/tachyon
 * kpavlov/tachyon]], a standalone pure-Java MCP server runtime. Tachyon is
 * dual-era: it implements the modern `2026-07-28` `server/discover` and also
 * answers the legacy `2025-11-25` `initialize` handshake. These tests prove our
 * client negotiates the modern era against a real third-party server, and that
 * pinning to the legacy era interoperates too.
 */
object TachyonInteropSpec extends ZIOSpecDefault:

  /** Start a tachyon server with a `greet` tool on an ephemeral port. */
  private def tachyonServer: ZIO[Scope, Throwable, TachyonServer] =
    ZIO.acquireRelease(
      ZIO.attemptBlocking(
        TachyonServer.builder()
          .name("tachyon-server")
          .version("1.0.0")
          .tool(ToolHandler.of(
            "greet",
            "Greets the caller",
            (_, _) => ToolResult.text("hello from tachyon"),
          ))
          .port(0)
          .start()
      )
    )(server => ZIO.attemptBlocking(server.close()).ignore)

  private val Modern = ProtocolVersion.V2026_07_28.wire

  /** Send a raw modern (2026-07-28) request to tachyon and parse the response body.
    * Includes the full modern `_meta` — tachyon requires `clientInfo`, which our
    * own client also always sends. */
  private def modernPost(port: Int, id: Int, method: String): ZIO[Client & Scope, Throwable, Json.Obj] =
    val meta = Json.Obj(
      McpMeta.ProtocolVersion    -> Json.Str(Modern),
      McpMeta.ClientInfo         -> Json.Obj("name" -> Json.Str("probe"), "version" -> Json.Str("1.0")),
      McpMeta.ClientCapabilities -> Json.Obj(),
    )
    val params = Json.Obj("_meta" -> (meta: Json))
    val body = s"""{"jsonrpc":"2.0","id":$id,"method":"$method","params":${(params: Json).toJson}}"""
    val url = URL.decode(s"http://localhost:$port/mcp").toOption.get
    val req = Request.post(url, Body.fromString(body))
      .addHeader(Header.ContentType(MediaType.application.json))
      .addHeader("accept", "application/json, text/event-stream")
      .addHeader(Negotiation.ProtocolVersionHeader, Modern)
      .addHeader(Negotiation.MethodHeader, method)
    ZClient.batched(req).flatMap: resp =>
      resp.body.asString.flatMap(s => ZIO.fromEither(s.fromJson[Json.Obj]).mapError(e => RuntimeException(s"$e: $s")))

  override def spec =
    suite("Tachyon interop (third-party MCP server)")(

      test("modern-default client negotiates the modern era against tachyon via server/discover"):
        ZIO.scoped:
          for
            server <- tachyonServer
            port    = server.port()
            client <- McpClient.connect(s"http://localhost:$port/mcp")
            tools  <- client.listTools
          yield assertTrue(
            // tachyon implements server/discover, so the client stays modern.
            client.protocolVersion == ProtocolVersion.V2026_07_28.wire,
            client.serverInfo.name == "tachyon-server",
            tools.map(_.name.value).contains("greet"),
          )
      ,

      test("client calls a tachyon-hosted tool"):
        ZIO.scoped:
          for
            server <- tachyonServer
            port    = server.port()
            client <- McpClient.connect(s"http://localhost:$port/mcp")
            result <- client.callTool("greet")
          yield
            val text = result.content.collectFirst { case ToolContent.Text(t, _) => t }
            assertTrue(text.exists(_.contains("hello from tachyon")))
      ,

      test("explicitly legacy-pinned client also interoperates with tachyon"):
        ZIO.scoped:
          for
            server <- tachyonServer
            port    = server.port()
            client <- McpClient.connect(McpClientConfig(
                        s"http://localhost:$port/mcp",
                        preferredVersion = ProtocolVersion.V2025_11_25,
                      ))
            tools  <- client.listTools
          yield assertTrue(
            client.protocolVersion == ProtocolVersion.V2025_11_25.wire,
            tools.map(_.name.value).contains("greet"),
          )
      ,

      // Wire-level cross-checks: a real third-party modern server (tachyon) and
      // our implementation must agree on the 2026-07-28 envelope field names.
      test("tachyon's server/discover returns the modern envelope we expect"):
        ZIO.scoped:
          for
            server <- tachyonServer
            port    = server.port()
            b      <- modernPost(port, 1, "server/discover")
          yield
            val r = b.get("result").flatMap(_.asObject)
            val supported = r.flatMap(_.get("supportedVersions")).flatMap(_.asArray)
              .map(_.flatMap(_.asString).toList).getOrElse(Nil)
            assertTrue(
              supported.contains(ProtocolVersion.V2026_07_28.wire),
              r.flatMap(_.get("resultType")).flatMap(_.asString).contains("complete"),
              r.flatMap(_.get("_meta")).flatMap(_.asObject).flatMap(_.get(McpMeta.ServerInfo)).isDefined,
            )
      ,

      test("tachyon answers a modern tools/list with resultType complete"):
        ZIO.scoped:
          for
            server <- tachyonServer
            port    = server.port()
            b      <- modernPost(port, 2, "tools/list")
          yield
            val r = b.get("result").flatMap(_.asObject)
            assertTrue(
              r.flatMap(_.get("resultType")).flatMap(_.asString).contains("complete"),
              r.flatMap(_.get("tools")).flatMap(_.asArray).exists(_.nonEmpty),
            )
      ,

    ).provide(Client.default, Scope.default) @@
      withLiveClock @@ timeout(2.minutes) @@ sequential @@ TestAspect.withLiveEnvironment
