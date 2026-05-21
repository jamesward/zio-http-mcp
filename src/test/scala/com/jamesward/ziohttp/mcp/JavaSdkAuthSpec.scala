package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.AuthTestHelpers.*
import com.jamesward.ziohttp.mcp.auth.*
import io.modelcontextprotocol.client.McpClient
import io.modelcontextprotocol.client.transport.HttpClientStreamableHttpTransport
import io.modelcontextprotocol.client.transport.customizer.{McpHttpClientAuthorizationErrorHandler, McpSyncHttpClientRequestCustomizer}
import io.modelcontextprotocol.spec.McpSchema as JMcpSchema
import zio.*
import zio.http.*
import zio.json.*
import zio.json.ast.Json
import zio.schema.*
import zio.test.*
import zio.test.TestAspect.*

import java.time.Duration as JDuration

/**
 * End-to-end test of our auth implementation against the official Java MCP SDK as a client.
 *
 * Validates that any MCP-spec-conformant client can talk to our auth-protected server given
 * a valid bearer token. The token is obtained via DCR + client_credentials against
 * `https://login.jamesward.dev` (open DCR) and then injected via the SDK's
 * [[McpSyncHttpClientRequestCustomizer]] hook.
 *
 * This is the "official client interop" half of the auth story. The other half is the full
 * DCR-aware client flow, which the Java SDK alone does not implement. That requires
 * `org.springaicommunity:mcp-client-security` + Spring AI / Spring Security; we leave that
 * to a future, optional integration test (it pulls in a heavy dep tree).
 *
 * Tagged `live-auth` for offline filtering.
 */
object JavaSdkAuthSpec extends ZIOSpecDefault:

  case class AddInput(a: Int, b: Int) derives Schema

  /** Tool that requires auth context (R = Client) — verifies the auth-aware server still type-checks. */
  private val addTool: McpToolHandlerR[Client] = McpTool("add")
    .description("Add two numbers")
    .handle[Client, Nothing, AddInput, String]: input =>
      ZIO.succeed(s"${input.a + input.b}")

  /** Build an authenticated `McpSyncClient` that injects a bearer token on every request. */
  private def withAuthenticatedClient[A](port: Int, token: String)(f: io.modelcontextprotocol.client.McpSyncClient => A): Task[A] =
    ZIO.attemptBlocking:
      val customizer: McpSyncHttpClientRequestCustomizer =
        (builder, _method, _endpoint, _body, _ctx) =>
          builder.header("Authorization", s"Bearer $token")
      val transport = HttpClientStreamableHttpTransport.builder(s"http://localhost:$port")
        .endpoint("/mcp")
        .httpRequestCustomizer(customizer)
        .build()
      val client = McpClient.sync(transport)
        .requestTimeout(JDuration.ofSeconds(10))
        .clientInfo(JMcpSchema.Implementation("zio-http-mcp-test-client", "1.0.0"))
        .build()
      try
        client.initialize()
        f(client)
      finally
        client.close()

  /** Build an unauthenticated client (no header customizer) — expected to fail with 401. */
  private def withUnauthenticatedClient[A](port: Int)(f: io.modelcontextprotocol.client.McpSyncClient => A): Task[A] =
    ZIO.attemptBlocking:
      val transport = HttpClientStreamableHttpTransport.builder(s"http://localhost:$port")
        .endpoint("/mcp")
        .build()
      val client = McpClient.sync(transport)
        .requestTimeout(JDuration.ofSeconds(10))
        .clientInfo(JMcpSchema.Implementation("zio-http-mcp-test-client", "1.0.0"))
        .build()
      try
        client.initialize()
        f(client)
      finally
        client.close()

  override def spec =
    suite("JavaSdkAuthSpec")(

      test("Java MCP SDK with bearer token: initialize + tools/list succeeds"):
        ZIO.scoped {
          for
            port  <- serveAuthenticatedScoped(Chunk(addTool))
            creds <- dynamicallyRegister()
            tok   <- fetchToken(creds, s"http://localhost:$port/mcp")
            tools <- withAuthenticatedClient(port, tok)(_.listTools().tools())
          yield
            import scala.jdk.CollectionConverters.*
            val names = tools.asScala.map(_.name()).toSet
            assertTrue(names.contains("add"))
        }
      ,

      test("Java MCP SDK with bearer token: tools/call returns correct result"):
        ZIO.scoped {
          for
            port   <- serveAuthenticatedScoped(Chunk(addTool))
            creds  <- dynamicallyRegister()
            tok    <- fetchToken(creds, s"http://localhost:$port/mcp")
            result <- withAuthenticatedClient(port, tok): client =>
              client.callTool(JMcpSchema.CallToolRequest(
                "add",
                java.util.Map.of[String, Object]("a", Int.box(2), "b", Int.box(3)),
                null,
              ))
          yield
            val text = result.content().get(0).asInstanceOf[JMcpSchema.TextContent].text()
            assertTrue(
              result.isError == null || !result.isError,
              text == "5",
            )
        }
      ,

      test("Java MCP SDK without token: initialize fails (server returns 401)"):
        ZIO.scoped {
          for
            port   <- serveAuthenticatedScoped(Chunk(addTool))
            // The SDK throws on initialize when the server returns 4xx; we expect an exception.
            outcome <- withUnauthenticatedClient(port)(_ => ()).either
          yield assertTrue(outcome.isLeft)
        }
      ,

      test("Java MCP SDK with token bound to wrong audience: initialize fails"):
        ZIO.scoped {
          for
            port    <- serveAuthenticatedScoped(Chunk(addTool))
            creds   <- dynamicallyRegister()
            // Token with audience for a different resource
            wrongAud <- fetchToken(creds, "https://wrong.example.com/mcp")
            outcome  <- withAuthenticatedClient(port, wrongAud)(_ => ()).either
          yield assertTrue(outcome.isLeft)
        }
      ,
    ).provide(Client.default) @@ tag("live-auth") @@ sequential @@ withLiveClock @@ timeout(60.seconds)
