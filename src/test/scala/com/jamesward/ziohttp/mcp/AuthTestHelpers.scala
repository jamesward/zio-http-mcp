package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.auth.*
import zio.*
import zio.http.*
import zio.json.*
import zio.json.ast.Json

import java.nio.charset.StandardCharsets

/**
 * Helpers for talking to a real OAuth 2.1 authorization server during integration tests.
 * Currently targets `https://login.jamesward.dev` (Spring Authorization Server with open DCR).
 *
 * Used by [[LiveAuthSpec]] (raw HTTP client) and [[JavaSdkAuthSpec]] (Java MCP SDK client).
 */
private[mcp] object AuthTestHelpers:

  val asIssuer = "https://login.jamesward.dev"

  final case class DcrCredentials(clientId: String, clientSecret: String)

  /** Register a fresh client at the AS's open DCR endpoint. */
  def dynamicallyRegister(scope: String = "mcp:tools"): ZIO[Client & Scope, Throwable, DcrCredentials] =
    val body =
      s"""{"client_name":"zio-http-mcp-IntegrationTest",
         |"grant_types":["client_credentials"],
         |"scope":"$scope",
         |"token_endpoint_auth_method":"client_secret_basic"}""".stripMargin
    val url = URL.decode(s"$asIssuer/oauth2/register").toOption.get
    for
      client <- ZIO.service[Client]
      resp   <- client.batched(
                  Request.post(url, Body.fromString(body))
                    .addHeader(Header.ContentType(MediaType.application.json))
                )
      raw    <- resp.body.asString
      _      <- ZIO.fail(RuntimeException(s"DCR returned ${resp.status}: $raw"))
                  .when(!resp.status.isSuccess)
      json   <- ZIO.fromEither(raw.fromJson[Json.Obj]).mapError(e => RuntimeException(s"DCR JSON: $e"))
      cid    <- ZIO.fromOption(json.get("client_id").flatMap(_.asString))
                  .orElseFail(RuntimeException("DCR response missing client_id"))
      sec    <- ZIO.fromOption(json.get("client_secret").flatMap(_.asString))
                  .orElseFail(RuntimeException("DCR response missing client_secret"))
    yield DcrCredentials(cid, sec)

  /** Obtain a `client_credentials` token bound to the supplied resource. */
  def fetchToken(
    creds: DcrCredentials,
    resource: String,
    scope: String = "mcp:tools",
  ): ZIO[Client & Scope, Throwable, String] =
    val url = URL.decode(s"$asIssuer/oauth2/token").toOption.get
    val basic = java.util.Base64.getEncoder.encodeToString(
      s"${creds.clientId}:${creds.clientSecret}".getBytes(StandardCharsets.UTF_8)
    )
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
      json   <- ZIO.fromEither(raw.fromJson[Json.Obj]).mapError(e => RuntimeException(s"Token JSON: $e"))
      tok    <- ZIO.fromOption(json.get("access_token").flatMap(_.asString))
                  .orElseFail(RuntimeException("Token response missing access_token"))
    yield tok

  /** Find a free local port the OS can bind to. */
  def findFreePort: UIO[Int] = ZIO.attemptBlocking {
    val s = new java.net.ServerSocket(0)
    try s.getLocalPort finally s.close()
  }.orDie

  /** Block until something is listening on the given port (up to 5 seconds). */
  def waitForBind(port: Int): UIO[Unit] =
    val attempt = ZIO.attemptBlocking {
      val sock = new java.net.Socket()
      try
        sock.connect(new java.net.InetSocketAddress("localhost", port), 100)
        true
      catch case _: Throwable => false
      finally sock.close()
    }.catchAll(_ => ZIO.succeed(false))
    attempt.repeatUntil((b: Boolean) => b).timeoutFail(())(5.seconds).ignore

  /** Build a server with auth configured for the given resource URI. */
  def buildAuthServer(
    boundPort: Int,
    tools: Chunk[McpToolHandlerR[Client]] = Chunk.empty,
  ): ZIO[Client & Scope, Throwable, McpServer[Client]] =
    val resourceUri = ResourceUri.parse(s"http://localhost:$boundPort/mcp").toOption.get
    for
      verifier <- TokenVerifier.discoverJwks(issuer = asIssuer)
    yield
      val authServer = McpServer("integration-test", "0.1.0")
        .auth(McpAuth(
          resourceUri = Some(resourceUri),
          authorizationServers = NonEmptyChunk(AuthorizationServer(asIssuer)),
          scopesSupported = Chunk(OauthScope("mcp:tools")),
          verifier = verifier,
          requiredScopes = Set(OauthScope("mcp:tools")),
        ))
      tools.foldLeft[McpServer[Client]](authServer)((srv, t) => srv.tool(t))

  /**
   * Bind an MCP server with auth on a free port for the duration of the surrounding scope.
   * Returns the port. The audience-bound token must reference this port.
   */
  def serveAuthenticatedScoped(
    tools: Chunk[McpToolHandlerR[Client]] = Chunk.empty,
  ): ZIO[Client & Scope, Throwable, Int] =
    for
      port   <- findFreePort
      server <- buildAuthServer(port, tools)
      _      <- Server.serve(server.statelessRoutes)
                  .provideSome[Client](Server.defaultWithPort(port))
                  .forkScoped
      _      <- waitForBind(port)
    yield port
