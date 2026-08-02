package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.auth.*
import zio.*
import zio.http.*
import zio.json.*
import zio.json.ast.Json
import zio.schema.*
import zio.test.*
import zio.test.TestAspect.*

/**
 * End-to-end test against the live `https://login.jamesward.dev` Spring Authorization Server.
 *
 * Validates the complete DCR flow advertised by the MCP authorization spec:
 *
 *   1. PRM document is reachable from the `WWW-Authenticate` header on a 401.
 *   2. The AS pointed to in the PRM supports RFC 7591 Dynamic Client Registration.
 *   3. A new client can be registered without auth (open DCR on this AS).
 *   4. A `client_credentials` token request with `resource=<MCP server URL>` produces a
 *      JWT whose `aud` claim binds it to the MCP server (RFC 8707).
 *   5. The MCP server accepts that token and lets the principal call tools.
 *   6. Tokens with the wrong audience or missing scopes are rejected.
 *
 * The test is tagged `live-auth` so it can be excluded from offline CI runs:
 * {{{
 *   sbt "testOnly *AuthSpec* *TokenVerifierSpec* *ProtectedResourceMetadataSpec*"
 * }}}
 *
 * Or skip with: `sbt "test -- -t \"live-auth\""` (zio-test tag filtering syntax).
 */
object LiveAuthSpec extends ZIOSpecDefault:

  private val asIssuer = "https://login.jamesward.dev"
  private val testScope = "mcp:tools"

  case class AddInput(a: Int, b: Int) derives Schema

  private val addTool: McpToolHandler = McpTool("add")
    .description("Add two numbers")
    .handle: (input: AddInput) =>
      ZIO.succeed(s"${input.a + input.b}")

  /** Find a free local port the OS can bind to. */
  private def findFreePort: UIO[Int] = ZIO.attemptBlocking {
    val s = new java.net.ServerSocket(0)
    try s.getLocalPort finally s.close()
  }.orDie

  /** Build a server with auth configured for the given resource URI. */
  private def buildServer(boundPort: Int): ZIO[Client & Scope, Throwable, McpServer[Any]] =
    val resourceUri = ResourceUri.parse(s"http://localhost:$boundPort/mcp").toOption.get
    for
      verifier <- TokenVerifier.discoverJwks(issuer = asIssuer)
    yield
      McpServer("live-auth-test", "0.1.0")
        .tool(addTool)
        .auth(McpAuth(
          resourceUri = Some(resourceUri),
          authorizationServers = NonEmptyChunk(AuthorizationServer(asIssuer)),
          scopesSupported = Chunk(OauthScope(testScope)),
          verifier = verifier,
          requiredScopes = Set(OauthScope(testScope)),
        ))

  /**
   * Bind a server on a free port for the duration of the surrounding scope.
   * Returns the port. Resource URI of the server matches the bound port.
   */
  private def serveScoped: ZIO[Client & Scope, Throwable, Int] =
    for
      port   <- findFreePort
      server <- buildServer(port)
      _      <- Server.serve(server.statelessRoutes)
                  .provide(Server.defaultWithPort(port))
                  .forkScoped
      _      <- waitForBind(port)
    yield port

  /** Block until something is listening on the given port (or 5 seconds elapse). */
  private def waitForBind(port: Int): ZIO[Any, Nothing, Unit] =
    val attempt = ZIO.attemptBlocking {
      val sock = new java.net.Socket()
      try
        sock.connect(new java.net.InetSocketAddress("localhost", port), 100)
        true
      catch case _: Throwable => false
      finally sock.close()
    }.catchAll(_ => ZIO.succeed(false))
    attempt.repeatUntil((b: Boolean) => b).timeoutFail(())(5.seconds).ignore

  // --- DCR + token helpers ---

  /** Result of an open DCR registration. */
  private case class DcrCredentials(clientId: String, clientSecret: String)

  /** Register a fresh client at login.jamesward.dev's open DCR endpoint. */
  private def dynamicallyRegister: ZIO[Client & Scope, Throwable, DcrCredentials] =
    val body =
      """{"client_name":"zio-http-mcp-LiveAuthSpec",
        |"grant_types":["client_credentials"],
        |"scope":"mcp:tools",
        |"token_endpoint_auth_method":"client_secret_basic"}""".stripMargin
    val url = URL.decode(s"$asIssuer/oauth2/register").toOption.get
    for
      client <- ZIO.service[Client]
      resp   <- client.batched(
                  Request.post(url, Body.fromString(body))
                    .addHeader(Header.ContentType(MediaType.application.json))
                )
      _      <- ZIO.fail(RuntimeException(s"DCR returned ${resp.status}"))
                  .when(!resp.status.isSuccess)
      raw    <- resp.body.asString
      json   <- ZIO.fromEither(raw.fromJson[Json.Obj]).mapError(e => RuntimeException(s"DCR: $e"))
      cid    <- ZIO.fromOption(json.get("client_id").flatMap(_.asString))
                  .orElseFail(RuntimeException("DCR response missing client_id"))
      sec    <- ZIO.fromOption(json.get("client_secret").flatMap(_.asString))
                  .orElseFail(RuntimeException("DCR response missing client_secret"))
    yield DcrCredentials(cid, sec)

  /** Obtain a `client_credentials` token bound to the supplied resource. */
  private def fetchToken(
    creds: DcrCredentials,
    resource: String,
    scope: String,
  ): ZIO[Client & Scope, Throwable, String] =
    val url = URL.decode(s"$asIssuer/oauth2/token").toOption.get
    val basic = Base64.encode(s"${creds.clientId}:${creds.clientSecret}")
    val form = s"grant_type=client_credentials" +
      s"&scope=${java.net.URLEncoder.encode(scope, "UTF-8")}" +
      s"&resource=${java.net.URLEncoder.encode(resource, "UTF-8")}"
    for
      client <- ZIO.service[Client]
      resp   <- client.batched(
                  Request.post(url, Body.fromString(form))
                    .addHeader(Header.ContentType(MediaType.application.`x-www-form-urlencoded`))
                    .addHeader("authorization", s"Basic $basic")
                )
      raw    <- resp.body.asString
      _      <- ZIO.fail(RuntimeException(s"Token endpoint returned ${resp.status}: $raw"))
                  .when(!resp.status.isSuccess)
      json   <- ZIO.fromEither(raw.fromJson[Json.Obj]).mapError(e => RuntimeException(s"Token: $e"))
      tok    <- ZIO.fromOption(json.get("access_token").flatMap(_.asString))
                  .orElseFail(RuntimeException("Token response missing access_token"))
    yield tok

  private object Base64:
    def encode(s: String): String =
      java.util.Base64.getEncoder.encodeToString(s.getBytes("UTF-8"))

  // --- HTTP helpers ---

  private def post(port: Int, body: String, token: Option[String] = None): ZIO[Client & Scope, Throwable, Response] =
    val url = URL.decode(s"http://localhost:$port/mcp").toOption.get
    val base = Request.post(url, Body.fromString(body))
      .addHeader(Header.ContentType(MediaType.application.json))
    val final0 = token.fold(base)(t => base.addHeader("authorization", s"Bearer $t"))
    ZClient.batched(final0)

  private val toolsCallAdd =
    """{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"add","arguments":{"a":2,"b":3}}}"""

  private val toolsList = """{"jsonrpc":"2.0","id":2,"method":"tools/list"}"""

  // --- Spec ---

  override def spec =
    suite("LiveAuthSpec")(

      test("1 — unauthenticated POST /mcp → 401 with WWW-Authenticate pointing at PRM URL"):
        ZIO.scoped {
          for
            port <- serveScoped
            resp <- post(port, toolsList)
          yield
            val www = resp.rawHeader("www-authenticate").getOrElse("")
            assertTrue(
              resp.status == Status.Unauthorized,
              www.contains(".well-known/oauth-protected-resource"),
            )
        }
      ,

      test("2 — PRM document lists login.jamesward.dev as authorization server"):
        ZIO.scoped {
          for
            port <- serveScoped
            resp <- {
                      val url = URL.decode(s"http://localhost:$port/.well-known/oauth-protected-resource").toOption.get
                      ZClient.batched(Request.get(url)).flatMap(_.body.asString)
                    }
            json <- ZIO.fromEither(resp.fromJson[Json.Obj]).mapError(e => RuntimeException(e))
          yield assertTrue(
            json.get("authorization_servers").flatMap(_.asArray)
              .exists(_.exists(_.asString.contains(asIssuer))),
          )
        }
      ,

      test("3 — open DCR registration succeeds and returns client_id + client_secret"):
        ZIO.scoped {
          for creds <- dynamicallyRegister
          yield assertTrue(
            creds.clientId.nonEmpty,
            creds.clientSecret.nonEmpty,
          )
        }
      ,

      test("4 — client_credentials grant with resource= produces an audience-bound JWT"):
        ZIO.scoped {
          for
            port  <- serveScoped
            resourceUri = s"http://localhost:$port/mcp"
            creds <- dynamicallyRegister
            token <- fetchToken(creds, resourceUri, testScope)
          yield
            val parts = token.split('.')
            val payload = new String(java.util.Base64.getUrlDecoder.decode(parts(1)), "UTF-8")
            val claims = payload.fromJson[Json.Obj].toOption.get
            val aud    = claims.get("aud") match
              case Some(Json.Str(s))  => Set(s)
              case Some(Json.Arr(xs)) => xs.collect { case Json.Str(s) => s }.toSet
              case _                   => Set.empty[String]
            val iss    = claims.get("iss").flatMap(_.asString)
            assertTrue(
              parts.length == 3,
              iss.contains(asIssuer),
              aud.contains(resourceUri),
            )
        }
      ,

      test("5 — POST /mcp tools/list with a freshly-issued token → 200 + tools array"):
        ZIO.scoped {
          for
            port  <- serveScoped
            resourceUri = s"http://localhost:$port/mcp"
            creds <- dynamicallyRegister
            tok   <- fetchToken(creds, resourceUri, testScope)
            resp  <- post(port, toolsList, token = Some(tok))
            body  <- resp.body.asString
          yield
            val tools = body.fromJson[Json.Obj].toOption
              .flatMap(_.get("result")).flatMap(_.asObject)
              .flatMap(_.get("tools")).flatMap(_.asArray)
            assertTrue(
              resp.status == Status.Ok,
              tools.exists(_.exists(_.asObject.flatMap(_.get("name")).flatMap(_.asString).contains("add"))),
            )
        }
      ,

      test("6 — POST /mcp tools/call with a freshly-issued token → correct result"):
        ZIO.scoped {
          for
            port  <- serveScoped
            resourceUri = s"http://localhost:$port/mcp"
            creds <- dynamicallyRegister
            tok   <- fetchToken(creds, resourceUri, testScope)
            resp  <- post(port, toolsCallAdd, token = Some(tok))
            body  <- resp.body.asString
          yield
            val text = body.fromJson[Json.Obj].toOption
              .flatMap(_.get("result")).flatMap(_.asObject)
              .flatMap(_.get("content")).flatMap(_.asArray)
              .flatMap(_.headOption).flatMap(_.asObject)
              .flatMap(_.get("text")).flatMap(_.asString)
            assertTrue(
              resp.status == Status.Ok,
              text.contains("5"),
            )
        }
      ,

      test("7 — token whose `aud` is for a different resource → 401 invalid_token"):
        ZIO.scoped {
          for
            port  <- serveScoped
            creds <- dynamicallyRegister
            tok   <- fetchToken(creds, "https://wrong.example.com/mcp", testScope)
            resp  <- post(port, toolsList, token = Some(tok))
          yield
            val www = resp.rawHeader("www-authenticate").getOrElse("")
            assertTrue(
              resp.status == Status.Unauthorized,
              www.contains("""error="invalid_token""""),
            )
        }
      ,

      test("9 — tampered JWT signature → 401 invalid_token"):
        ZIO.scoped {
          for
            port  <- serveScoped
            resourceUri = s"http://localhost:$port/mcp"
            creds <- dynamicallyRegister
            tok   <- fetchToken(creds, resourceUri, testScope)
            parts  = tok.split('.')
            flipped = parts(2).head match
                        case 'a' => 'b'
                        case _   => 'a'
            tampered = s"${parts(0)}.${parts(1)}.$flipped${parts(2).tail}"
            resp  <- post(port, toolsList, token = Some(tampered))
          yield
            val www = resp.rawHeader("www-authenticate").getOrElse("")
            assertTrue(
              resp.status == Status.Unauthorized,
              www.contains("""error="invalid_token""""),
            )
        }
      ,
    ).provide(Client.default) @@ tag("live-auth") @@ sequential @@ withLiveClock @@ timeout(60.seconds) @@ AuthTestHelpers.retryTransientUpstream
