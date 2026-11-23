package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.auth.*
import zio.*
import zio.http.*
import zio.json.ast.Json
import zio.test.*
import zio.test.TestAspect.*

import java.time.Instant

/**
 * The `McpAuth.acceptOriginAudience` quirk. A `mountedAtParam("slug")` mount
 * advertises a per-mount resource (`http://host/<slug>`), so a client asks for a
 * token for it. RFC 8707 §2.1 lets an authorization server shorten that `resource`,
 * and some shorten it to the origin (`http://host`) in the token's `aud`.
 *
 * By default the server refuses such a token: the MCP spec requires a server to
 * check that a token was issued for it specifically, and a token for the origin
 * would otherwise be accepted at every mount on it. A server behind such an
 * authorization server turns the quirk on to accept it.
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

  private def server(acceptOriginAudience: Boolean): McpServer[Any] =
    McpServer("param-mount", "0.1.0")
      .mountedAtParam("tenant")
      .auth(McpAuth(
        authorizationServers = NonEmptyChunk(issuer),
        verifier             = audienceEchoVerifier,
        scopesSupported      = Chunk(reqScope),
        requiredScopes       = Set(reqScope),
        acceptOriginAudience = acceptOriginAudience,
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

      test("by default, a token whose aud is only the host origin is refused on a param mount"):
        for
          port <- Server.install(server(acceptOriginAudience = false).statelessRoutes)
          resp <- post(port, "/tenant-a", toolsListRequest, token = s"http://localhost:$port")
        yield assertTrue(resp.status.code == 401)
      ,

      test("with acceptOriginAudience, a token whose aud is the host origin is accepted on a param mount"):
        for
          port <- Server.install(server(acceptOriginAudience = true).statelessRoutes)
          // The AS shortened the requested `http://localhost:$port/tenant-a` to its origin.
          resp <- post(port, "/tenant-a", toolsListRequest, token = s"http://localhost:$port")
        yield assertTrue(resp.status.code == 200)
      ,

      test("the exact per-mount audience is accepted either way"):
        for
          port  <- Server.install(server(acceptOriginAudience = false).statelessRoutes)
          plain <- post(port, "/tenant-a", toolsListRequest, token = s"http://localhost:$port/tenant-a")
          port2 <- Server.install(server(acceptOriginAudience = true).statelessRoutes)
          quirk <- post(port2, "/tenant-a", toolsListRequest, token = s"http://localhost:$port2/tenant-a")
        yield assertTrue(plain.status.code == 200, quirk.status.code == 200)
      ,

    ).provide(
      Server.defaultWith(_.onAnyOpenPort),
      Client.default,
      Scope.default,
    ) @@ withLiveClock @@ timeout(60.seconds) @@ sequential
