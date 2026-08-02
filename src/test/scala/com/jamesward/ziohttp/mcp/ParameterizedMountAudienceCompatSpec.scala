package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.auth.*
import zio.*
import zio.http.*
import zio.json.ast.Json
import zio.test.*
import zio.test.TestAspect.*

import java.time.Instant

/**
 * REPRODUCTION (pre-fix, expected RED): a parameterised mount must accept a token
 * whose audience is the host ORIGIN, not only the per-mount resource.
 *
 * Why this happens in the real world: a `mountedAtParam("slug")` mount advertises a
 * per-mount PRM `resource` (`http://host/<slug>`) — required so RFC 9728 §3.3-strict
 * clients (e.g. the Rust `rmcp` client behind some MCP CLIs) accept it. The client
 * then requests `resource=http://host/<slug>` at the token endpoint. But per RFC 8707
 * §2.1 an authorization server MAY canonicalize the resource, and real ones do — they
 * fold it to the ORIGIN (`http://host`) when minting the token `aud`. The resource
 * server therefore receives `aud=http://host` while the mount, if it only accepts the
 * per-mount audience, rejects it with "Audience mismatch" — breaking auth for every
 * such client. zio-http-mcp must accept the host-origin audience on a param mount so
 * library users don't have to discover and work around this themselves.
 *
 * See `.specs/MCP_AUTH_ERA_TESTING.md` (toolbook) for the end-to-end kiro-cli trace.
 */
object ParameterizedMountAudienceCompatSpec extends ZIOSpecDefault:

  private val issuer   = AuthorizationServer("https://auth.example.com")
  private val reqScope = OauthScope("mcp:tools")

  // Verifier where the bearer token string *is* the principal's audience — lets a
  // test bind the audience to an exact value (e.g. the host origin) without a real JWT.
  private val audienceEchoVerifier: TokenVerifier[Any] =
    TokenVerifier.fromFunction: raw =>
      ZIO.succeed(Principal(
        subject   = Some("u"),
        clientId  = Some("c"),
        scopes    = Set(reqScope),
        audience  = Set(raw),
        issuer    = Some("https://auth.example.com"),
        expiresAt = Some(Instant.now().plusSeconds(3600)),
        raw       = raw,
        claims    = Json.Obj(),
      ))

  private val pingTool: McpToolHandler = McpTool("ping").description("ping").handle(ZIO.succeed("pong"))

  private val server: McpServer[Any] =
    McpServer("param-mount", "0.1.0")
      .mountedAtParam("tenant")
      .auth(McpAuth(
        authorizationServers = NonEmptyChunk(issuer),
        verifier             = audienceEchoVerifier,
        scopesSupported      = Chunk(reqScope),
        requiredScopes       = Set(reqScope),
      ))
      .tool(pingTool)

  private def post(port: Int, path: String, body: String, token: String): ZIO[Client & Scope, Throwable, Response] =
    val url  = URL.decode(s"http://localhost:$port$path").toOption.get
    val base = Request.post(url, Body.fromString(body))
      .addHeader(Header.ContentType(MediaType.application.json))
      .addHeader("accept", "application/json, text/event-stream")
      .addHeader("authorization", s"Bearer $token")
    ZClient.batched(base)

  private val toolsListRequest = """{"jsonrpc":"2.0","id":2,"method":"tools/list"}"""

  override def spec =
    suite("ParameterizedMountAudienceCompatSpec")(

      test("a token whose aud is the host ORIGIN is accepted on a param mount (AS canonicalized the resource)"):
        for
          port <- Server.install(server.statelessRoutes)
          // The AS folded the requested `http://localhost:$port/tenant-a` to its origin.
          resp <- post(port, "/tenant-a", toolsListRequest, token = s"http://localhost:$port")
        yield assertTrue(resp.status.code == 200)
      ,

      test("the exact per-mount audience is still accepted (regression guard)"):
        for
          port <- Server.install(server.statelessRoutes)
          resp <- post(port, "/tenant-a", toolsListRequest, token = s"http://localhost:$port/tenant-a")
        yield assertTrue(resp.status.code == 200)
      ,

    ).provide(
      Server.defaultWith(_.onAnyOpenPort),
      Client.default,
      Scope.default,
    ) @@ withLiveClock @@ timeout(60.seconds) @@ sequential
