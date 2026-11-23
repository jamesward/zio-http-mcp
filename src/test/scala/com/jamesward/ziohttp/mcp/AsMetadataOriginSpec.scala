package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.auth.*
import zio.*
import zio.http.*
import zio.json.ast.Json
import zio.test.*
import zio.test.TestAspect.*

import java.time.Instant

/**
 * An auth-protected server whose authorization server is on another origin serves
 * no authorization-server metadata of its own: every client finds the
 * authorization server through the protected resource metadata (RFC 9728), as the
 * spec says.
 *
 * The server used to answer origin probes with a `User-Agent`-keyed 302 to the
 * authorization server, for an older kiro-cli (Rust `rmcp`) that ignored the
 * protected resource metadata. kiro-cli 3.5.0 no longer probes there on either
 * engine ([[KiroCliDiscoverySpec]]), so the redirect was removed. Every client,
 * kiro-cli included, now gets the same 404.
 */
object AsMetadataOriginSpec extends ZIOSpecDefault:

  private val issuer = AuthorizationServer("https://auth.example.com")

  private val pingTool: McpToolHandler = McpTool("ping").description("ping").handle(ZIO.succeed("pong"))

  private val verifier: TokenVerifier[Any] =
    TokenVerifier.fromFunction(raw => ZIO.succeed(Principal(
      subject = Some("u"), clientId = Some("c"), scopes = Set.empty, audience = Set.empty,
      issuer = Some("https://auth.example.com"), expiresAt = Some(Instant.now().plusSeconds(3600)),
      raw = raw, claims = Json.Obj())))

  private val server: McpServer[Any] =
    McpServer("param-mount", "0.1.0")
      .mountedAtParam("tenant")
      .auth(McpAuth(authorizationServers = NonEmptyChunk(issuer), verifier = verifier))
      .tool(pingTool)

  private def getUA(port: Int, path: String, ua: Option[String]): ZIO[Client & Scope, Throwable, Response] =
    val req = Request.get(URL.decode(s"http://localhost:$port$path").toOption.get)
    ZClient.batched(ua.fold(req)(req.addHeader("user-agent", _)))

  /** The origin probes an RFC 8414 / OIDC discovery client makes, root and path-inserted. */
  private val probes = List(
    "/.well-known/oauth-authorization-server",
    "/.well-known/oauth-authorization-server/tenant-a",
    "/.well-known/openid-configuration",
    "/.well-known/openid-configuration/tenant-a",
  )

  /** Old kiro-cli (rmcp), kiro-cli 3.5.0's two engines, MCP Inspector (Node), Claude, and none. */
  private val userAgents = List(
    Some("kiro-cli/0.3 rmcp/0.2"), Some("kiro-cli/3.5.0"), Some("node"), Some("undici"),
    Some("claude-code/1.2"), None,
  )

  override def spec =
    suite("AsMetadataOriginSpec")(
      test("the MCP origin serves no authorization-server metadata, whatever the client"):
        for
          port  <- Server.install(server.statelessRoutes)
          resps <- ZIO.foreach(for p <- probes; ua <- userAgents yield (p, ua)): (p, ua) =>
                     getUA(port, p, ua).map(r => (p, ua, r.status.code, r.rawHeader("location")))
        yield assertTrue(
          resps.forall((_, _, code, _) => code == 404),
          resps.forall((_, _, _, location) => location.isEmpty),
        )
      ,
      test("the protected resource metadata names the cross-origin authorization server"):
        for
          port <- Server.install(server.statelessRoutes)
          resp <- getUA(port, "/.well-known/oauth-protected-resource/tenant-a", None)
          body <- resp.body.asString
        yield assertTrue(resp.status.code == 200, body.contains("https://auth.example.com"))
    ).provide(
      Server.defaultWith(_.onAnyOpenPort),
      Client.default,
      Scope.default,
    ) @@ withLiveClock @@ timeout(60.seconds) @@ sequential
