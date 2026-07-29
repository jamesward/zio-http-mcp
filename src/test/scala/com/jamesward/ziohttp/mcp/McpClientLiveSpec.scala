package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.client.*
import zio.*
import zio.http.*
import zio.json.ast.Json
import zio.test.*
import zio.test.TestAspect.*

/**
 * End-to-end test of our [[com.jamesward.ziohttp.mcp.client.McpClient]] against a real,
 * publicly-hosted MCP server built with the official Java MCP SDK:
 * `https://www.javadocs.dev/mcp` (no authorization).
 *
 * This is the "interop with a real third-party server" check for the no-auth path. The
 * authenticated `client_credentials` path is covered against our own auth-protected
 * [[McpServer]] in [[McpClientAuthSpec]].
 *
 * Requires network access; tagged `live` for offline filtering.
 */
object McpClientLiveSpec extends ZIOSpecDefault:

  private val javadocsUrl = "https://www.javadocs.dev/mcp"

  override def spec =
    suite("McpClientLiveSpec")(
      suite("javadocs.dev (no auth)")(
        test("initialize negotiates protocol and reports server info"):
          ZIO.scoped:
            for
              client <- McpClient.connect(javadocsUrl)
            yield assertTrue(
              client.serverInfo.name == "javadocs.dev",
              // A third-party server's era can change under us (javadocs.dev now
              // negotiates 2026-07-28); assert we settled on a supported revision
              // rather than pinning the era of a live server we don't control.
              ProtocolVersion.supportedWire.contains(client.protocolVersion),
              client.serverCapabilities.tools.isDefined,
            )
        ,
        test("tools/list returns the documented tools"):
          ZIO.scoped:
            for
              client <- McpClient.connect(javadocsUrl)
              tools  <- client.listTools
            yield
              val names = tools.map(_.name.value).toSet
              assertTrue(
                tools.nonEmpty,
                names.contains("search_artifacts"),
                tools.forall(_.inputSchema.get("type").flatMap(_.asString).contains("object")),
              )
        ,
        test("tools/call returns content"):
          ZIO.scoped:
            for
              client <- McpClient.connect(javadocsUrl)
              result <- client.callTool("search_artifacts", Json.Obj(Chunk("query" -> Json.Str("zio-http"))))
            yield assertTrue(
              result.isError.forall(!_),
              result.content.nonEmpty,
            )
      ) @@ tag("live"),
    ).provide(Client.default) @@ sequential @@ withLiveClock @@ timeout(90.seconds)
