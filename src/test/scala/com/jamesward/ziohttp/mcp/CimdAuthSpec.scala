package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.auth.*
import com.jamesward.ziohttp.mcp.client.*
import zio.*
import zio.http.*
import zio.json.*
import zio.json.ast.Json
import zio.schema.*
import zio.test.*
import zio.test.TestAspect.*

/**
 * End-to-end integration tests of the client-side MCP 2026-07-28 hardened
 * authorization flow — CIMD, PKCE, RFC 8707 `resource` binding, and RFC 9207 `iss`
 * validation — with every party in-process:
 *
 *   - [[TestIdp]] plays the authorization server (auto-approving, CIMD-dereferencing,
 *     JWT-minting),
 *   - our own auth-protected [[McpServer]] plays the resource server (validating the
 *     minted JWTs via `TokenVerifier.discoverJwks` against the TestIdp), and
 *   - [[com.jamesward.ziohttp.mcp.client.McpClient]] with [[OAuthAuthorizationCode]]
 *     plays the client, hosting its Client ID Metadata Document on the TestIdp's
 *     HTTP server.
 *
 * No external network, no Docker. The official conformance kit's client auth
 * scenarios (`auth/basic-cimd`, `auth/iss-*`, …) cover the same behaviors against
 * the reference implementation in [[ClientConformanceSpec]].
 */
object CimdAuthSpec extends ZIOSpecDefault:

  case class AddInput(a: Int, b: Int) derives Schema

  private val addTool: McpToolHandlerR[Client] = McpTool("add")
    .description("Add two numbers")
    .handle[Client, Nothing, AddInput, String]: input =>
      ZIO.succeed(s"${input.a + input.b}")

  /** The client's CIMD document, hosted on the IDP's HTTP server for the test. */
  private def cimdRoutes(redirectUri: String)(idpPort: Int): Routes[Any, Response] =
    val docUrl = cimdDocUrl(idpPort)
    def document(clientId: String): Response =
      Response.json(
        Json.Obj(Chunk(
          "client_id" -> Json.Str(clientId),
          "client_name" -> Json.Str("zio-http-mcp test client"),
          "redirect_uris" -> Json.Arr(Chunk(Json.Str(redirectUri))),
          "grant_types" -> Json.Arr(Chunk(Json.Str("authorization_code"))),
          "response_types" -> Json.Arr(Chunk(Json.Str("code"))),
          "token_endpoint_auth_method" -> Json.Str("none"),
        )).toJson
      )
    Routes(
      Method.GET / "client-metadata.json" -> handler(document(docUrl)),
      // Declares someone else's URL as its `client_id` — the AS must refuse it.
      Method.GET / "client-metadata-mismatch.json" ->
        handler(document("https://elsewhere.example.com/client-metadata.json")),
    )

  private def cimdDocUrl(idpPort: Int): String = s"http://localhost:$idpPort/client-metadata.json"

  private def cimdMismatchDocUrl(idpPort: Int): String =
    s"http://localhost:$idpPort/client-metadata-mismatch.json"

  private val redirectUri = "http://127.0.0.1:23456/callback"

  /** Serve an auth-protected MCP server whose verifier trusts the TestIdp. */
  private def serveMcpServer(idp: TestIdp): ZIO[Client & Scope, Throwable, Int] =
    for
      port     <- AuthTestHelpers.findFreePort
      verifier <- TokenVerifier.discoverJwks(issuer = idp.issuer)
      resource  = ResourceUri.parse(s"http://localhost:$port/mcp").toOption.get
      server    = McpServer("cimd-test", "0.1.0")
                    .auth(McpAuth(
                      resourceUri = Some(resource),
                      authorizationServers = NonEmptyChunk(AuthorizationServer(idp.issuer)),
                      scopesSupported = Chunk(OauthScope("mcp:tools")),
                      verifier = verifier,
                      requiredScopes = Set(OauthScope("mcp:tools")),
                    ))
                    .tool(addTool)
      _        <- Server.serve(server.statelessRoutes)
                    .provideSome[Client](Server.defaultWithPort(port))
                    .forkScoped
      _        <- AuthTestHelpers.waitForBind(port)
    yield port

  private def connectAndCall(port: Int, oauth: OAuthAuthorizationCode) =
    for
      client <- McpClient.connect(McpClientConfig(serverUrl = s"http://localhost:$port/mcp", oauth = Some(oauth)))
      tools  <- client.listTools
      result <- client.callTool("add", Json.Obj(Chunk("a" -> Json.Num(2), "b" -> Json.Num(3))))
    yield (tools, result)

  override def spec =
    suite("CimdAuthSpec")(

      test("full CIMD flow: URL client_id, PKCE S256, resource binding, iss validation, tool call"):
        ZIO.scoped:
          for
            idp             <- TestIdp.serveScoped(extraRoutes = cimdRoutes(redirectUri))
            port            <- serveMcpServer(idp)
            docUrl           = cimdDocUrl(idpUrlPort(idp))
            (tools, result) <- connectAndCall(port, OAuthAuthorizationCode(
                                 clientMetadataUrl = Some(docUrl),
                                 redirectUri = redirectUri,
                               ))
            events          <- idp.recordedEvents
          yield
            val authz = events.authorizations.headOption
            val token = events.tokens.headOption
            val text = result.content.collectFirst { case ToolContent.Text(t, _) => t }
            assertTrue(
              // the client used its CIMD URL as client_id and the AS dereferenced it
              authz.map(_.clientId).contains(docUrl),
              events.cimdFetches == Chunk(docUrl),
              // PKCE S256 on the authorization request
              authz.exists(_.codeChallenge.exists(_.nonEmpty)),
              authz.flatMap(_.codeChallengeMethod).contains("S256"),
              // RFC 8707 resource binding on both requests
              authz.flatMap(_.resource).contains(s"http://localhost:$port/mcp"),
              token.flatMap(_.resource).contains(s"http://localhost:$port/mcp"),
              // scope selection strategy picked up the server's advertised scope
              authz.flatMap(_.scope).contains("mcp:tools"),
              // and the MCP calls worked with the audience-bound JWT
              tools.map(_.name.value).contains("add"),
              result.isError.forall(!_),
              text.contains("5"),
            )
      ,
      test("client rejects a wrong iss in the authorization response (RFC 9207)"):
        ZIO.scoped:
          for
            idp    <- TestIdp.serveScoped(
                        TestIdp.Config(issMode = TestIdp.IssMode.Wrong),
                        extraRoutes = cimdRoutes(redirectUri),
                      )
            port   <- serveMcpServer(idp)
            result <- connectAndCall(port, OAuthAuthorizationCode(
                        clientMetadataUrl = Some(cimdDocUrl(idpUrlPort(idp))),
                        redirectUri = redirectUri,
                      )).exit
            events <- idp.recordedEvents
          yield assertTrue(
            result.isFailure,
            result.causeOption.flatMap(_.failureOption).exists {
              case McpClientError.Auth(msg) => msg.contains("iss")
              case _                        => false
            },
            // the client must not have proceeded to the token endpoint
            events.tokens.isEmpty,
          )
      ,
      test("client rejects a missing iss when the AS advertises iss support (RFC 9207)"):
        ZIO.scoped:
          for
            idp    <- TestIdp.serveScoped(
                        TestIdp.Config(issMode = TestIdp.IssMode.Omit, issAdvertised = true),
                        extraRoutes = cimdRoutes(redirectUri),
                      )
            port   <- serveMcpServer(idp)
            result <- connectAndCall(port, OAuthAuthorizationCode(
                        clientMetadataUrl = Some(cimdDocUrl(idpUrlPort(idp))),
                        redirectUri = redirectUri,
                      )).exit
            events <- idp.recordedEvents
          yield assertTrue(result.isFailure, events.tokens.isEmpty)
      ,
      test("client proceeds when iss is absent and not advertised"):
        ZIO.scoped:
          for
            idp             <- TestIdp.serveScoped(
                                 TestIdp.Config(issMode = TestIdp.IssMode.Omit, issAdvertised = false),
                                 extraRoutes = cimdRoutes(redirectUri),
                               )
            port            <- serveMcpServer(idp)
            (tools, result) <- connectAndCall(port, OAuthAuthorizationCode(
                                 clientMetadataUrl = Some(cimdDocUrl(idpUrlPort(idp))),
                                 redirectUri = redirectUri,
                               ))
          yield assertTrue(tools.nonEmpty, result.isError.forall(!_))
      ,
      test("AS rejects a CIMD document whose client_id does not match its URL"):
        ZIO.scoped:
          for
            idp    <- TestIdp.serveScoped(extraRoutes = cimdRoutes(redirectUri))
            port   <- serveMcpServer(idp)
            result <- connectAndCall(port, OAuthAuthorizationCode(
                        clientMetadataUrl = Some(cimdMismatchDocUrl(idpUrlPort(idp))),
                        redirectUri = redirectUri,
                      )).exit
            events <- idp.recordedEvents
          yield assertTrue(
            result.isFailure,
            events.rejections.exists(_.contains("client_id")),
            events.tokens.isEmpty,
          )
      ,
      test("AS rejects a redirect_uri that is not in the CIMD document"):
        ZIO.scoped:
          for
            idp    <- TestIdp.serveScoped(extraRoutes = cimdRoutes(redirectUri))
            port   <- serveMcpServer(idp)
            result <- connectAndCall(port, OAuthAuthorizationCode(
                        clientMetadataUrl = Some(cimdDocUrl(idpUrlPort(idp))),
                        redirectUri = "http://127.0.0.1:9/evil-callback",
                      )).exit
            events <- idp.recordedEvents
          yield assertTrue(
            result.isFailure,
            events.rejections.exists(_.contains("redirect_uri")),
            events.tokens.isEmpty,
          )
      ,
      test("DCR fallback: client registers dynamically when the AS does not support CIMD"):
        ZIO.scoped:
          for
            idp             <- TestIdp.serveScoped(
                                 TestIdp.Config(cimdSupported = false, dcrEnabled = true),
                                 extraRoutes = cimdRoutes(redirectUri),
                               )
            port            <- serveMcpServer(idp)
            (tools, result) <- connectAndCall(port, OAuthAuthorizationCode(
                                 clientMetadataUrl = Some(cimdDocUrl(idpUrlPort(idp))),
                                 redirectUri = redirectUri,
                               ))
            events          <- idp.recordedEvents
          yield assertTrue(
            // registered via DCR, not CIMD
            events.dcrRegistrations.nonEmpty,
            events.cimdFetches.isEmpty,
            events.authorizations.headOption.exists(_.clientId.startsWith("dcr-")),
            // SEP-837: loopback redirect ⇒ application_type native
            events.dcrRegistrations.headOption
              .flatMap(_.get("application_type")).flatMap(_.asString).contains("native"),
            tools.nonEmpty,
            result.isError.forall(!_),
          )
      ,
      test("pre-registered client id takes priority over CIMD and DCR"):
        ZIO.scoped:
          for
            idp             <- TestIdp.serveScoped(
                                 TestIdp.Config(cimdSupported = true, dcrEnabled = true),
                                 extraRoutes = cimdRoutes(redirectUri),
                               )
            port            <- serveMcpServer(idp)
            (tools, result) <- connectAndCall(port, OAuthAuthorizationCode(
                                 clientId = Some("pre-registered-client"),
                                 clientMetadataUrl = Some(cimdDocUrl(idpUrlPort(idp))),
                                 redirectUri = redirectUri,
                               ))
            events          <- idp.recordedEvents
          yield assertTrue(
            events.authorizations.headOption.map(_.clientId).contains("pre-registered-client"),
            events.cimdFetches.isEmpty,
            events.dcrRegistrations.isEmpty,
            tools.nonEmpty,
            result.isError.forall(!_),
          )
      ,
      test("client refuses a PRM whose resource does not match the server URL"):
        ZIO.scoped:
          for
            idp     <- TestIdp.serveScoped(extraRoutes = cimdRoutes(redirectUri))
            port    <- AuthTestHelpers.findFreePort
            // A hostile/misconfigured resource server: PRM names a different resource.
            evilPrm  = Json.Obj(Chunk(
                         "resource" -> Json.Str("https://evil.example.com/mcp"),
                         "authorization_servers" -> Json.Arr(Chunk(Json.Str(idp.issuer))),
                       )).toJson
            routes   = Routes(
                         Method.GET / ".well-known" / "oauth-protected-resource" -> handler(Response.json(evilPrm)),
                         Method.POST / "mcp" -> handler(
                           Response.json("""{"jsonrpc":"2.0","error":{"code":-32001,"message":"Unauthorized"},"id":null}""")
                             .status(Status.Unauthorized)
                         ),
                       )
            _       <- Server.serve(routes)
                         .provide(Server.defaultWithPort(port))
                         .forkScoped
            _       <- AuthTestHelpers.waitForBind(port)
            result  <- connectAndCall(port, OAuthAuthorizationCode(
                         clientMetadataUrl = Some(cimdDocUrl(idpUrlPort(idp))),
                         redirectUri = redirectUri,
                       )).exit
            events  <- idp.recordedEvents
          yield assertTrue(
            result.isFailure,
            // no authorization request may be made for a mismatched resource
            events.authorizations.isEmpty,
            events.tokens.isEmpty,
          )
      ,
    ).provide(Client.default) @@ sequential @@ withLiveClock @@ timeout(120.seconds)

  /** The TestIdp's port (its issuer is always `http://localhost:<port>`). */
  private def idpUrlPort(idp: TestIdp): Int =
    idp.issuer.split(':').last.toInt
