package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.auth.*
import com.jamesward.ziohttp.mcp.client.{McpClient, McpClientConfig}
import zio.*
import zio.http.*
import zio.json.ast.Json
import zio.schema.*
import zio.test.*
import zio.test.TestAspect.*

import java.time.Instant

/**
 * End-to-end, OFFLINE proof that one auth-protected [[McpServer]] serves our
 * [[McpClient]] in BOTH protocol eras — the combination real end-user agents hit:
 *
 *   - **legacy `2025-11-25`**: `initialize` handshake + session, bearer on every request;
 *   - **modern `2026-07-28`**: stateless `server/discover`, bearer on every request.
 *
 * This is the auth × dual-era matrix that [[DualEraIntegrationSpec]] (dual-era, no
 * auth) and [[McpClientAuthSpec]] (auth, single-era, LIVE external AS) don't cover
 * together. No network/AS dependency: a `TokenVerifier.fromFunction` maps the bearer
 * to a principal, so it runs in CI.
 */
object AuthenticatedDualEraSpec extends ZIOSpecDefault:

  private val reqScope       = OauthScope("mcp:tools")
  private val pinnedResource = "https://mcp.test"

  // The bearer string IS the principal's audience, so a token equal to the pinned
  // resource authenticates; scope satisfies the per-request requirement.
  private val verifier: TokenVerifier[Any] =
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

  case class AddInput(a: Int, b: Int) derives Schema
  case class AddOutput(result: Int) derives Schema

  private val addTool: McpToolHandler = McpTool("add")
    .description("Add two numbers")
    .handle[Any, Nothing, AddInput, AddOutput]: in =>
      ZIO.succeed(AddOutput(in.a + in.b))

  private val server: McpServer[Any] =
    McpServer("authed-dual-era", "1.0.0")
      .instructions("auth required; works for legacy and modern clients")
      .auth(McpAuth(
        authorizationServers = NonEmptyChunk(AuthorizationServer("https://auth.example.com")),
        verifier             = verifier,
        resourceUri          = Some(ResourceUri.unsafe(pinnedResource)),
        scopesSupported      = Chunk(reqScope),
        requiredScopes       = Set(reqScope),
      ))
      .tool(addTool)

  private def bearer = Headers(Header.Authorization.Bearer(pinnedResource))

  private def driveAuthed(port: Int, era: ProtocolVersion, a: Int, b: Int) =
    ZIO.scoped:
      for
        c      <- McpClient.connect(McpClientConfig(
                    serverUrl        = s"http://localhost:$port/mcp",
                    preferredVersion = era,
                    headers          = bearer,
                  ))
        tools  <- c.listTools
        result <- c.callToolAs[AddOutput]("add", Json.Obj("a" -> Json.Num(a), "b" -> Json.Num(b)))
      yield (c.protocolVersion, tools.map(_.name.value).toSet, result.result)

  override def spec =
    suite("AuthenticatedDualEraSpec (one authed server, legacy + modern clients)")(

      test("an AUTHENTICATED client drives the authed server in BOTH eras"):
        for
          port   <- Server.install(server.routes)
          legacy <- driveAuthed(port, ProtocolVersion.V2025_11_25, 2, 3)
          modern <- driveAuthed(port, ProtocolVersion.V2026_07_28, 10, 11)
        yield assertTrue(
          legacy._1 == ProtocolVersion.V2025_11_25.wire, // legacy handshake negotiated
          legacy._2 == Set("add"),
          legacy._3 == 5,
          modern._1 == ProtocolVersion.V2026_07_28.wire, // modern stateless negotiated
          modern._2 == Set("add"),
          modern._3 == 21,
        )
      ,

      test("an UNAUTHENTICATED client is rejected by the authed server (both eras)"):
        for
          port      <- Server.install(server.routes)
          legacyErr <- ZIO.scoped(McpClient.connect(McpClientConfig(s"http://localhost:$port/mcp", preferredVersion = ProtocolVersion.V2025_11_25)).flatMap(_.listTools)).either
          modernErr <- ZIO.scoped(McpClient.connect(McpClientConfig(s"http://localhost:$port/mcp", preferredVersion = ProtocolVersion.V2026_07_28)).flatMap(_.listTools)).either
        yield assertTrue(legacyErr.isLeft, modernErr.isLeft)
      ,

    ).provide(
      Server.defaultWith(_.onAnyOpenPort),
      Client.default,
      McpServer.State.default,
    ) @@ withLiveClock @@ timeout(2.minutes) @@ sequential
