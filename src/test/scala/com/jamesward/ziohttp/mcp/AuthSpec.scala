package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.auth.*
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*
import zio.test.TestAspect.*

import java.time.Instant

given canEqualStatusAuth: CanEqual[zio.http.Status, zio.http.Status] = CanEqual.derived

object AuthSpec extends ZIOSpecDefault:

  // --- Test fixtures ---

  private val testResourceUri = ResourceUri.parse("http://localhost:0/mcp").toOption.get
  private val testIssuer = AuthorizationServer("https://auth.example.com")
  private val testScope = OauthScope("mcp:tools")
  private val adminScope = OauthScope("admin")

  private def principalWith(scopes: OauthScope*): Principal =
    Principal(
      subject = Some("test-user"),
      clientId = Some("test-client"),
      scopes = scopes.toSet,
      audience = Set("http://localhost:0/mcp"),
      issuer = Some("https://auth.example.com"),
      expiresAt = Some(Instant.now().plusSeconds(3600)),
      raw = "test-token",
      claims = Json.Obj(),
    )

  /** Verifier that accepts only the literal token "valid-token" with the configured scopes. */
  private def stubVerifier(scopes: OauthScope*): TokenVerifier[Any] =
    TokenVerifier.fromFunction:
      case "valid-token" => ZIO.succeed(principalWith(scopes*))
      case _             => ZIO.fail(AuthError.Invalid("not the test token"))

  private def authConfig(verifier: TokenVerifier[Any], required: Set[OauthScope] = Set.empty): McpAuth[Any] =
    McpAuth(
      resourceUri = Some(testResourceUri),
      authorizationServers = NonEmptyChunk(testIssuer),
      scopesSupported = Chunk(testScope),
      verifier = verifier,
      requiredScopes = required,
    )

  private val whoamiTool: McpToolHandler = McpTool("whoami")
    .description("Returns the authenticated subject")
    .handleWithContext: ctx =>
      ZIO.succeed(ctx.principal.flatMap(_.subject).getOrElse("anon"))

  private val deleteTool: McpToolHandler = McpTool("delete_user")
    .description("Deletes a user")
    .requireScopes(adminScope)
    .handle:
      ZIO.succeed("deleted")

  private def serverWithAuth(auth: McpAuth[Any]): McpServer[Any] =
    McpServer("auth-test", "0.1.0")
      .tool(whoamiTool)
      .tool(deleteTool)
      .auth(auth)

  private val serverWithoutAuth: McpServer[Any] =
    McpServer("noauth-test", "0.1.0")
      .tool(whoamiTool)
      .tool(deleteTool)

  // --- HTTP helpers ---

  import zio.http.*

  private def installStateful(server: McpServer[Any]): ZIO[Server, Throwable, Int] =
    Server.install(server.routes).provideSome[Server](McpServer.State.default)

  private def installStateless(server: McpServer[Any]): ZIO[Server, Throwable, Int] =
    Server.install(server.statelessRoutes)

  private def post(port: Int, body: String, token: Option[String] = None, sessionId: Option[String] = None): ZIO[Client & Scope, Throwable, Response] =
    val url = URL.decode(s"http://localhost:$port/mcp").toOption.get
    val base = Request.post(url, Body.fromString(body))
      .addHeader(Header.ContentType(MediaType.application.json))
    val withToken = token.fold(base)(t => base.addHeader("authorization", s"Bearer $t"))
    val withSession = sessionId.fold(withToken)(s => withToken.addHeader("mcp-session-id", s))
    ZClient.request(withSession)

  private def get(port: Int, path: String = "/mcp", token: Option[String] = None): ZIO[Client & Scope, Throwable, Response] =
    val url = URL.decode(s"http://localhost:$port$path").toOption.get
    val base = Request.get(url)
    val withToken = token.fold(base)(t => base.addHeader("authorization", s"Bearer $t"))
    ZClient.request(withToken)

  private def del(port: Int, token: Option[String] = None): ZIO[Client & Scope, Throwable, Response] =
    val url = URL.decode(s"http://localhost:$port/mcp").toOption.get
    val base = Request.delete(url)
    val withToken = token.fold(base)(t => base.addHeader("authorization", s"Bearer $t"))
    ZClient.request(withToken)

  private def initRequest =
    """{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"test","version":"0"}}}"""

  private def toolsListRequest = """{"jsonrpc":"2.0","id":2,"method":"tools/list"}"""

  private def toolsCallRequest(name: String) =
    s"""{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"$name","arguments":{}}}"""

  // --- Spec ---

  override def spec =
    suite("AuthSpec")(

      test("1 — without .auth(...), /.well-known/oauth-protected-resource returns 404"):
        for
          port <- installStateful(serverWithoutAuth)
          resp <- get(port, "/.well-known/oauth-protected-resource")
        yield assertTrue(resp.status == Status.NotFound)
      ,

      test("2 — with .auth(...), GET /.well-known/oauth-protected-resource returns 200 + PRM JSON"):
        for
          port <- installStateful(serverWithAuth(authConfig(stubVerifier(testScope))))
          resp <- get(port, "/.well-known/oauth-protected-resource")
          body <- resp.body.asString
        yield
          val json = body.fromJson[Json.Obj].toOption.get
          assertTrue(
            resp.status == Status.Ok,
            json.get("resource").flatMap(_.asString).contains(testResourceUri.value),
            json.get("authorization_servers").flatMap(_.asArray).exists(_.exists(_.asString.contains("https://auth.example.com"))),
            json.get("scopes_supported").flatMap(_.asArray).exists(_.exists(_.asString.contains("mcp:tools"))),
          )
      ,

      test("2b — path-suffixed PRM URL returns the same body"):
        for
          port    <- installStateful(serverWithAuth(authConfig(stubVerifier(testScope))))
          rootRsp <- get(port, "/.well-known/oauth-protected-resource")
          rootBody <- rootRsp.body.asString
          subRsp  <- get(port, "/.well-known/oauth-protected-resource/mcp")
          subBody <- subRsp.body.asString
        yield assertTrue(
          subRsp.status == Status.Ok,
          subBody == rootBody,
        )
      ,

      test("2d — PRM is served at any trailing path under /.well-known/oauth-protected-resource"):
        // Allows the WWW-Authenticate URL to point to any sub-path RFC 9728 §3.1 may dictate.
        for
          port <- installStateful(serverWithAuth(authConfig(stubVerifier(testScope))))
          a    <- get(port, "/.well-known/oauth-protected-resource/some/nested/path")
          b    <- get(port, "/.well-known/oauth-protected-resource/anything")
        yield assertTrue(
          a.status == Status.Ok,
          b.status == Status.Ok,
        )
      ,

      test("2c — PRM responses include Cache-Control: max-age=3600"):
        for
          port <- installStateful(serverWithAuth(authConfig(stubVerifier(testScope))))
          resp <- get(port, "/.well-known/oauth-protected-resource")
        yield
          val cc = resp.rawHeader("cache-control").getOrElse("")
          assertTrue(cc.contains("max-age=3600"))
      ,

      test("3 — POST /mcp without Authorization → 401 + WWW-Authenticate with realm + resource_metadata + scope"):
        for
          port <- installStateful(serverWithAuth(authConfig(stubVerifier(testScope), Set(testScope))))
          resp <- post(port, initRequest)
        yield
          val www = resp.rawHeader("www-authenticate").getOrElse("")
          assertTrue(
            resp.status == Status.Unauthorized,
            www.contains("Bearer"),
            www.contains("""realm="mcp""""),
            // RFC 9728 §3.1: /.well-known/oauth-protected-resource goes BETWEEN host and path,
            // not appended after the resource path. For resource http://localhost:0/mcp the PRM
            // URL must be http://localhost:0/.well-known/oauth-protected-resource/mcp.
            www.contains("""resource_metadata="http://localhost:0/.well-known/oauth-protected-resource/mcp""""),
            www.contains("""scope="mcp:tools""""),
          )
      ,

      test("3b — when resourceUri is None, derives from X-Forwarded-Proto + X-Forwarded-Host"):
        // Server has resourceUri = None; derives from forwarded headers.
        val derivedAuth = authConfig(stubVerifier(testScope)).copy(resourceUri = None)
        for
          port <- installStateful(McpServer("derive-test", "0.1.0").tool(whoamiTool).auth(derivedAuth))
          resp <- {
            val url = URL.decode(s"http://localhost:$port/mcp").toOption.get
            ZClient.request(
              Request.post(url, Body.fromString(initRequest))
                .addHeader(Header.ContentType(MediaType.application.json))
                .addHeader("x-forwarded-proto", "https")
                .addHeader("x-forwarded-host", "mcp.public.example.com")
            )
          }
        yield
          val www = resp.rawHeader("www-authenticate").getOrElse("")
          assertTrue(
            resp.status == Status.Unauthorized,
            // resource_metadata should reflect the forwarded headers, not localhost
            www.contains("""resource_metadata="https://mcp.public.example.com/.well-known/oauth-protected-resource/mcp""""),
          )
      ,

      test("3c — when resourceUri is None, derives from RFC 7239 Forwarded header"):
        val derivedAuth = authConfig(stubVerifier(testScope)).copy(resourceUri = None)
        for
          port <- installStateful(McpServer("derive-test", "0.1.0").tool(whoamiTool).auth(derivedAuth))
          resp <- {
            val url = URL.decode(s"http://localhost:$port/mcp").toOption.get
            ZClient.request(
              Request.post(url, Body.fromString(initRequest))
                .addHeader(Header.ContentType(MediaType.application.json))
                .addHeader("forwarded", """proto=https;host="mcp.via-forwarded.example.com"""")
            )
          }
        yield
          val www = resp.rawHeader("www-authenticate").getOrElse("")
          assertTrue(
            resp.status == Status.Unauthorized,
            www.contains("""resource_metadata="https://mcp.via-forwarded.example.com/.well-known/oauth-protected-resource/mcp""""),
          )
      ,

      test("3d — when resourceUri is None and no forwarded headers, derives from Host header"):
        val derivedAuth = authConfig(stubVerifier(testScope)).copy(resourceUri = None)
        for
          port <- installStateful(McpServer("derive-test", "0.1.0").tool(whoamiTool).auth(derivedAuth))
          resp <- post(port, initRequest)
        yield
          val www = resp.rawHeader("www-authenticate").getOrElse("")
          assertTrue(
            resp.status == Status.Unauthorized,
            // Host header is "localhost:<port>"; PRM URL is built from that with http scheme
            www.matches(""".*resource_metadata="http://localhost:\d+/.well-known/oauth-protected-resource/mcp".*""".r.pattern.toString) ||
              www.contains("""resource_metadata="http://localhost:"""),
          )
      ,

      test("4 — malformed Authorization → 401 invalid_token"):
        for
          port <- installStateful(serverWithAuth(authConfig(stubVerifier(testScope))))
          resp <- post(port, initRequest, token = Some("not-a-bearer-token-but-we-prefix-it"))
        yield
          val www = resp.rawHeader("www-authenticate").getOrElse("")
          // The Bearer prefix is required — pass the string without "Bearer " prefix in the helper to simulate.
          // The helper always prefixes; so the token IS prefixed. Instead, we test by sending a non-Bearer scheme directly.
          assertTrue(resp.status == Status.Unauthorized)
      ,

      test("4b — non-Bearer Authorization scheme → 401 invalid_token"):
        for
          port <- installStateful(serverWithAuth(authConfig(stubVerifier(testScope))))
          resp <- {
            val url = URL.decode(s"http://localhost:$port/mcp").toOption.get
            val req = Request.post(url, Body.fromString(initRequest))
              .addHeader(Header.ContentType(MediaType.application.json))
              .addHeader("authorization", "Basic dXNlcjpwYXNz")
            ZClient.request(req)
          }
        yield
          val www = resp.rawHeader("www-authenticate").getOrElse("")
          assertTrue(
            resp.status == Status.Unauthorized,
            www.contains("""error="invalid_token""""),
          )
      ,

      test("5 — verifier returns Invalid → 401 with JSON-RPC -32001 body"):
        for
          port <- installStateful(serverWithAuth(authConfig(stubVerifier(testScope))))
          resp <- post(port, initRequest, token = Some("bad-token"))
          body <- resp.body.asString
        yield
          val json = body.fromJson[Json.Obj].toOption.get
          val code = json.get("error").flatMap(_.asObject).flatMap(_.get("code")).flatMap(_.asNumber)
          assertTrue(
            resp.status == Status.Unauthorized,
            code.exists(_.value.intValue == -32001),
          )
      ,

      test("6 — verifier returns AudienceMismatch → 401 with audience in error_description"):
        val mismatchVerifier = TokenVerifier.fromFunction[Any]: _ =>
          ZIO.fail(AuthError.AudienceMismatch(testResourceUri, Set("https://other.example.com")))
        for
          port <- installStateful(serverWithAuth(authConfig(mismatchVerifier)))
          resp <- post(port, initRequest, token = Some("any"))
        yield
          val www = resp.rawHeader("www-authenticate").getOrElse("")
          assertTrue(
            resp.status == Status.Unauthorized,
            www.contains("""error="invalid_token""""),
            www.toLowerCase.contains("audience"),
          )
      ,

      test("7 — token without required scope → 403 insufficient_scope (step-up challenge)"):
        for
          port <- installStateful(serverWithAuth(authConfig(stubVerifier(), Set(testScope))))
          resp <- post(port, initRequest, token = Some("valid-token"))
        yield
          val www = resp.rawHeader("www-authenticate").getOrElse("")
          assertTrue(
            resp.status == Status.Forbidden,
            www.contains("""error="insufficient_scope""""),
            www.contains("""scope="mcp:tools""""),
            www.contains("""resource_metadata="http://localhost:0/.well-known/oauth-protected-resource/mcp""""),
          )
      ,

      test("8 — valid token with required scopes → tools/list succeeds (stateless)"):
        for
          port    <- installStateless(serverWithAuth(authConfig(stubVerifier(testScope), Set(testScope))))
          listRsp <- post(port, toolsListRequest, token = Some("valid-token"))
          body    <- listRsp.body.asString
        yield
          val json = body.fromJson[Json.Obj].toOption.get
          val tools = json.get("result").flatMap(_.asObject).flatMap(_.get("tools")).flatMap(_.asArray)
          assertTrue(
            listRsp.status == Status.Ok,
            tools.exists(_.size == 2),
          )
      ,

      test("9 — tool handler can read principal from ctx (whoami returns sub claim, stateless)"):
        for
          port    <- installStateless(serverWithAuth(authConfig(stubVerifier(testScope), Set(testScope))))
          callRsp <- post(port, toolsCallRequest("whoami"), token = Some("valid-token"))
          body    <- callRsp.body.asString
        yield
          val text = body.fromJson[Json.Obj].toOption
            .flatMap(_.get("result")).flatMap(_.asObject)
            .flatMap(_.get("content")).flatMap(_.asArray)
            .flatMap(_.headOption).flatMap(_.asObject)
            .flatMap(_.get("text")).flatMap(_.asString)
          assertTrue(
            callRsp.status == Status.Ok,
            text.contains("test-user"),
          )
      ,

      test("10 — per-tool scope: delete_user denied without admin scope, whoami still works (stateless)"):
        for
          port   <- installStateless(serverWithAuth(authConfig(stubVerifier(testScope), Set(testScope))))
          delRsp <- post(port, toolsCallRequest("delete_user"), token = Some("valid-token"))
          whoRsp <- post(port, toolsCallRequest("whoami"), token = Some("valid-token"))
        yield
          val www = delRsp.rawHeader("www-authenticate").getOrElse("")
          assertTrue(
            delRsp.status == Status.Forbidden,
            www.contains("""error="insufficient_scope""""),
            www.contains("admin"),
            whoRsp.status == Status.Ok,
          )
      ,

      test("11 — GET /mcp without token → 401"):
        for
          port <- installStateful(serverWithAuth(authConfig(stubVerifier(testScope))))
          resp <- get(port, "/mcp")
        yield assertTrue(resp.status == Status.Unauthorized)
      ,

      test("12 — DELETE /mcp without token → 401"):
        for
          port <- installStateful(serverWithAuth(authConfig(stubVerifier(testScope))))
          resp <- del(port)
        yield assertTrue(resp.status == Status.Unauthorized)
      ,

      test("13 — stateless routes apply the same auth middleware"):
        for
          port    <- installStateless(serverWithAuth(authConfig(stubVerifier(testScope), Set(testScope))))
          missing <- post(port, initRequest)
          good    <- post(port, initRequest, token = Some("valid-token"))
        yield assertTrue(
          missing.status == Status.Unauthorized,
          good.status == Status.Ok,
        )
      ,

      test("14 — token in URL query is rejected (OAuth 2.1 §5)"):
        for
          port <- installStateful(serverWithAuth(authConfig(stubVerifier(testScope))))
          resp <- {
            val url = URL.decode(s"http://localhost:$port/mcp?access_token=valid-token").toOption.get
            ZClient.request(
              Request.post(url, Body.fromString(initRequest))
                .addHeader(Header.ContentType(MediaType.application.json))
            )
          }
        yield assertTrue(resp.status == Status.Unauthorized)
      ,

      test("15 — when auth is disabled, .requireScopes(...) is silently ignored (stateless)"):
        for
          port   <- installStateless(serverWithoutAuth)
          delRsp <- post(port, toolsCallRequest("delete_user"))
          body   <- delRsp.body.asString
        yield
          val json = body.fromJson[Json.Obj].toOption.get
          val isError = json.get("result").flatMap(_.asObject).flatMap(_.get("isError")).flatMap(_.asBoolean)
          assertTrue(
            delRsp.status == Status.Ok,
            !isError.contains(true),
          )
      ,
    ).provideSome[Scope](Server.defaultWithPort(0), Client.default) @@ sequential @@ withLiveClock @@ timeout(30.seconds)
