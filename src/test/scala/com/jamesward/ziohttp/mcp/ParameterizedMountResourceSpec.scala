package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.auth.*
import zio.*
import zio.http.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*
import zio.test.TestAspect.*

import java.time.Instant

/**
 * A parameterised mount (`mountedAtParam`) with a derived resource URI must be
 * RFC 9728 §3.3 compliant: the advertised Protected Resource Metadata `resource`
 * must identify the exact URL the client accessed (`http://host/<value>`), and the
 * `WWW-Authenticate` challenge must point at the matching path-inserted PRM URL.
 * The audience check accepts a token whose `aud` is that per-mount resource OR its
 * host origin — an AS may canonicalize the requested `resource` to its origin
 * (RFC 8707 §2.1), so both must pass; only an unrelated audience is rejected.
 */
object ParameterizedMountResourceSpec extends ZIOSpecDefault:

  private val issuer   = AuthorizationServer("https://auth.example.com")
  private val reqScope = OauthScope("mcp:tools")

  /**
   * A stand-in verifier where the bearer token string *is* the audience the
   * principal claims. This lets a test assert audience matching against a
   * dynamically-bound loopback port without pre-computing it: pass the URL you
   * want the token to be "for" as the bearer value.
   */
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

  private val pingTool: McpToolHandler = McpTool("ping")
    .description("ping")
    .handle:
      ZIO.succeed("pong")

  // Parameterised mount, derived resource URI (resourceUri = None).
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

  private def installStateless(s: McpServer[Any]): ZIO[Server, Throwable, Int] =
    Server.install(s.statelessRoutes)

  private def post(port: Int, path: String, body: String, token: Option[String]): ZIO[Client & Scope, Throwable, Response] =
    val url  = URL.decode(s"http://localhost:$port$path").toOption.get
    val base = Request.post(url, Body.fromString(body))
      .addHeader(Header.ContentType(MediaType.application.json))
      .addHeader("accept", "application/json, text/event-stream")
    ZClient.batched(token.fold(base)(t => base.addHeader("authorization", s"Bearer $t")))

  private def get(port: Int, path: String): ZIO[Client & Scope, Throwable, Response] =
    ZClient.batched(Request.get(URL.decode(s"http://localhost:$port$path").toOption.get))

  private def initRequest =
    """{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"t","version":"0"}}}"""
  private def toolsListRequest = """{"jsonrpc":"2.0","id":2,"method":"tools/list"}"""

  override def spec =
    suite("ParameterizedMountResourceSpec")(

      test("PRM document for /<tenant> advertises the per-mount resource (not the host origin)"):
        for
          port <- installStateless(server)
          resp <- get(port, "/.well-known/oauth-protected-resource/tenant-a")
          body <- resp.body.asString
        yield
          val resource = body.fromJson[Json.Obj].toOption
            .flatMap(_.get("resource")).flatMap(_.asString)
          assertTrue(
            resp.status.code == 200,
            resource.contains(s"http://localhost:$port/tenant-a"),
          )
      ,

      test("401 WWW-Authenticate points at the per-mount (path-inserted) PRM URL"):
        for
          port <- installStateless(server)
          resp <- post(port, "/tenant-a", initRequest, token = None)
        yield
          val www = resp.rawHeader("www-authenticate").getOrElse("")
          assertTrue(
            resp.status.code == 401,
            www.contains(s"""resource_metadata="http://localhost:$port/.well-known/oauth-protected-resource/tenant-a""""),
          )
      ,

      test("audience: the per-mount resource AND its host origin are accepted; a foreign audience is rejected"):
        for
          port     <- installStateless(server)
          perMount <- post(port, "/tenant-a", toolsListRequest, token = Some(s"http://localhost:$port/tenant-a"))
          origin   <- post(port, "/tenant-a", toolsListRequest, token = Some(s"http://localhost:$port"))
          foreign  <- post(port, "/tenant-a", toolsListRequest, token = Some("https://evil.example.com"))
        yield assertTrue(
          perMount.status.code == 200, // exact per-mount audience
          origin.status.code == 200,   // host-origin audience (AS canonicalized the resource, RFC 8707 §2.1) — accepted
          foreign.status.code == 401,  // an unrelated audience is still rejected
        )
      ,

      test("two different tenants get distinct resources (per-tenant isolation)"):
        for
          port <- installStateless(server)
          a    <- get(port, "/.well-known/oauth-protected-resource/tenant-a")
          b    <- get(port, "/.well-known/oauth-protected-resource/tenant-b")
          ab   <- a.body.asString
          bb   <- b.body.asString
        yield
          def res(s: String) = s.fromJson[Json.Obj].toOption.flatMap(_.get("resource")).flatMap(_.asString)
          assertTrue(
            res(ab).contains(s"http://localhost:$port/tenant-a"),
            res(bb).contains(s"http://localhost:$port/tenant-b"),
          )
      ,

    ).provide(
      Server.defaultWith(_.onAnyOpenPort),
      Client.default,
      Scope.default,
    ) @@ withLiveClock @@ timeout(60.seconds) @@ sequential
