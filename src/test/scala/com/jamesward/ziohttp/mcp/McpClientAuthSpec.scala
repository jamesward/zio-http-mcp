package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.client.*
import zio.*
import zio.http.*
import zio.json.ast.Json
import zio.schema.*
import zio.test.*
import zio.test.TestAspect.*

/**
 * End-to-end test of our [[com.jamesward.ziohttp.mcp.client.McpClient]] OAuth 2.1
 * `client_credentials` flow against *our own* auth-protected [[McpServer]].
 *
 * This is the auth counterpart to [[McpClientSpec]]: the client runs the full
 * discovery chain it would use against any real server —
 *
 *   1. probe the MCP endpoint → `401` with a `WWW-Authenticate: resource_metadata=…`
 *      challenge (served by our [[McpServer]] auth middleware),
 *   2. fetch the RFC 9728 Protected Resource Metadata our server publishes,
 *   3. discover the token endpoint from the authorization server metadata, and
 *   4. obtain a `client_credentials` token bound to the resource and attach it as a
 *      bearer token on every request.
 *
 * The authorization server is the real, open-DCR `https://login.jamesward.dev`
 * (same one [[LiveAuthSpec]] / [[JavaSdkAuthSpec]] use), so the client credentials
 * are minted on the fly via DCR — no manual setup. Tagged `live-auth`.
 */
object McpClientAuthSpec extends ZIOSpecDefault:

  case class AddInput(a: Int, b: Int) derives Schema

  private val addTool: McpToolHandlerR[Client] = McpTool("add")
    .description("Add two numbers")
    .handle[Client, Nothing, AddInput, String]: input =>
      ZIO.succeed(s"${input.a + input.b}")

  override def spec =
    suite("McpClientAuthSpec")(
      test("client_credentials discovery + token + authenticated tools/list & tools/call"):
        ZIO.scoped:
          for
            port   <- AuthTestHelpers.serveAuthenticatedScoped(Chunk(addTool))
            creds  <- AuthTestHelpers.dynamicallyRegister()
            client <- McpClient.connect(McpClientConfig(
                        serverUrl = s"http://localhost:$port/mcp",
                        oauth = Some(OAuthClientCredentials(
                          clientId = creds.clientId,
                          clientSecret = Config.Secret(creds.clientSecret),
                          scopes = Set("mcp:tools"),
                        )),
                      ))
            tools  <- client.listTools
            result <- client.callTool("add", Json.Obj(Chunk("a" -> Json.Num(2), "b" -> Json.Num(3))))
          yield
            val text = result.content.collectFirst { case ToolContent.Text(t, _) => t }
            assertTrue(
              tools.map(_.name.value).contains("add"),
              result.isError.forall(!_),
              text.contains("5"),
            )
    ).provide(Client.default) @@ tag("live-auth") @@ sequential @@ withLiveClock @@ timeout(90.seconds)
