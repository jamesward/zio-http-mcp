package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.client.*
import zio.*
import zio.http.*
import zio.test.*
import zio.test.TestAspect.*

import dev.tachyonmcp.server.TachyonServer
import dev.tachyonmcp.server.features.tools.{ToolHandler, ToolResult}

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

    ).provide(Client.default, Scope.default) @@
      withLiveClock @@ timeout(2.minutes) @@ sequential @@ TestAspect.withLiveEnvironment
