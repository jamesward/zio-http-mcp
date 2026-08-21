package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.auth.*
import zio.*
import zio.http.*
import zio.json.ast.Json
import zio.test.*
import zio.test.TestAspect.*

import java.time.Instant

/**
 * Backwards-compat AS-metadata discovery (see [[McpServer.asMetadataRedirectRoutes]]).
 * Older MCP clients (e.g. the Rust `rmcp` client) predate the RFC 9728 resource↔AS
 * split: they IGNORE the PRM `authorization_servers` and probe the RESOURCE origin
 * for OAuth authorization-server metadata. The server must 302-redirect those probes
 * (RFC 8414 `oauth-authorization-server` + OIDC `openid-configuration`, root and
 * path-inserted) to the configured authorization server so discovery succeeds.
 *
 * Regression guard for the "OAuth discovery failed: the server does not advertise
 * OAuth endpoints" failure kiro-cli hit against a cross-origin AS.
 */
object AsMetadataRedirectSpec extends ZIOSpecDefault:

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

  private def get(port: Int, path: String): ZIO[Client & Scope, Throwable, Response] =
    ZClient.batched(Request.get(URL.decode(s"http://localhost:$port$path").toOption.get))

  private def getUA(port: Int, path: String, ua: String): ZIO[Client & Scope, Throwable, Response] =
    ZClient.batched(Request.get(URL.decode(s"http://localhost:$port$path").toOption.get).addHeader("user-agent", ua))

  private val expectedLocation = "https://auth.example.com/.well-known/oauth-authorization-server"

  override def spec =
    suite("AsMetadataRedirectSpec")(

      test("mount-origin AS-metadata probes redirect (302) to the cross-origin authorization server"):
        for
          port  <- Server.install(server.statelessRoutes)
          // The exact paths a pre-RFC-9728 client (kiro-cli/rmcp) probes on the resource origin.
          probes = List(
                     "/.well-known/oauth-authorization-server",
                     "/.well-known/oauth-authorization-server/tenant-a",
                     "/.well-known/openid-configuration",
                     "/.well-known/openid-configuration/tenant-a",
                   )
          resps <- ZIO.foreach(probes)(p => get(port, p).map(r => (p, r.status.code, r.rawHeader("location"))))
        yield assertTrue(
          resps.forall((_, code, _) => code == 302),
          resps.forall((_, _, loc) => loc.contains(expectedLocation)),
        )
      ,

      test("the PRM well-known is still served locally (not redirected)"):
        for
          port <- Server.install(server.statelessRoutes)
          resp <- get(port, "/.well-known/oauth-protected-resource/tenant-a")
        yield assertTrue(resp.status.code == 200)
      ,

      // --- User-Agent-keyed discovery (the only client signal on these
      // unauthenticated well-known GETs) --------------------------------------

      test("kiro-cli (legacy rmcp) origin probe still gets the 302 compat redirect"):
        for
          port <- Server.install(server.statelessRoutes)
          a <- getUA(port, "/.well-known/oauth-authorization-server", "kiro-cli/0.3 rmcp/0.2")
          b <- getUA(port, "/.well-known/oauth-authorization-server/tenant-a", "kiro-cli/0.3")
          c <- getUA(port, "/.well-known/openid-configuration/tenant-a", "kiro-cli/0.3")
        yield assertTrue(
          a.status.code == 302, a.rawHeader("location").contains(expectedLocation),
          b.status.code == 302, b.rawHeader("location").contains(expectedLocation),
          c.status.code == 302, c.rawHeader("location").contains(expectedLocation),
        )
      ,

      // MCP Inspector connects via the TypeScript MCP SDK, whose HTTP layer is
      // Node's global `fetch` — its User-Agent is literally "node" (verified on
      // Node v24). Such clients strictly validate AS-metadata `issuer` against the
      // probed origin (RFC 8414 §3.3) and reject a cross-origin redirect, so we
      // 404 the origin probe and they fall back to the PRM.
      test("MCP Inspector (TypeScript SDK / Node fetch, user-agent \"node\") gets 404, not the redirect"):
        for
          port <- Server.install(server.statelessRoutes)
          a <- getUA(port, "/.well-known/oauth-authorization-server", "node")
          b <- getUA(port, "/.well-known/oauth-authorization-server/tenant-a", "node")
          c <- getUA(port, "/.well-known/openid-configuration", "node")
          // Some Node stacks report the undici default instead.
          d <- getUA(port, "/.well-known/oauth-authorization-server", "undici")
        yield assertTrue(
          a.status.code == 404,
          b.status.code == 404,
          c.status.code == 404,
          d.status.code == 404,
        )
      ,

      test("Claude (PRM-first client) origin probe gets 404"):
        for
          port <- Server.install(server.statelessRoutes)
          a <- getUA(port, "/.well-known/oauth-authorization-server", "claude-code/1.2")
          b <- getUA(port, "/.well-known/oauth-authorization-server/tenant-a", "Claude/1.0")
        yield assertTrue(a.status.code == 404, b.status.code == 404)
      ,

    ).provide(
      Server.defaultWith(_.onAnyOpenPort),
      Client.default,
      Scope.default,
    ) @@ withLiveClock @@ timeout(60.seconds) @@ sequential
