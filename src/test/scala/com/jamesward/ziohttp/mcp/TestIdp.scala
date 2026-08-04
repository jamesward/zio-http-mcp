package com.jamesward.ziohttp.mcp

import com.nimbusds.jose.crypto.RSASSASigner
import com.nimbusds.jose.jwk.RSAKey
import com.nimbusds.jose.{JWSAlgorithm, JWSHeader}
import com.nimbusds.jwt.{JWTClaimsSet, SignedJWT}
import zio.*
import zio.http.*
import zio.json.*
import zio.json.ast.Json

import java.security.KeyPairGenerator
import java.security.interfaces.{RSAPrivateKey, RSAPublicKey}
import java.util.Date

/**
 * A minimal in-process OAuth 2.1 authorization server for integration tests of the
 * client-side MCP authorization flow (2026-07-28 "hardened" profile).
 *
 * Implements exactly what an MCP client needs from an AS:
 *   - RFC 8414 metadata (advertising CIMD / `iss` / PKCE support per [[TestIdp.Config]])
 *   - `GET /authorize` with auto-approval: validates the client identity — dereferencing
 *     Client ID Metadata Documents for URL-formatted `client_id`s — and 302-redirects
 *     with `code`, `state`, and (per config) the RFC 9207 `iss` parameter
 *   - `POST /token`: `authorization_code` (verifying PKCE S256) and `refresh_token`
 *     grants, minting RS256 JWTs audience-bound to the request's `resource`
 *   - `GET /jwks` so [[com.jamesward.ziohttp.mcp.auth.TokenVerifier.discoverJwks]]
 *     can validate the minted tokens
 *   - optional RFC 7591 `POST /register` (Dynamic Client Registration fallback)
 *
 * Every request is recorded so tests can assert on the wire-level behavior (PKCE
 * challenge present, `resource` parameter sent, CIMD document fetched, …).
 */
final class TestIdp private (
  val issuer: String,
  config: TestIdp.Config,
  client: Client,
  rsaKey: RSAKey,
  codes: Ref[Map[String, TestIdp.CodeRecord]],
  refreshTokens: Ref[Map[String, TestIdp.CodeRecord]],
  registrations: Ref[Map[String, Chunk[String]]],
  events: Ref[TestIdp.Events],
):
  import TestIdp.*

  def recordedEvents: UIO[Events] = events.get

  private def metadataJson: Json.Obj =
    Json.Obj(
      Chunk(
        Some("issuer" -> Json.Str(issuer)),
        Some("authorization_endpoint" -> Json.Str(s"$issuer/authorize")),
        Some("token_endpoint" -> Json.Str(s"$issuer/token")),
        Some("jwks_uri" -> Json.Str(s"$issuer/jwks")),
        Option.when(config.dcrEnabled)("registration_endpoint" -> Json.Str(s"$issuer/register")),
        Some("response_types_supported" -> Json.Arr(Chunk(Json.Str("code")))),
        Some("grant_types_supported" -> Json.Arr(Chunk(Json.Str("authorization_code"), Json.Str("refresh_token")))),
        Some("code_challenge_methods_supported" -> Json.Arr(Chunk(Json.Str("S256")))),
        Some("token_endpoint_auth_methods_supported" -> Json.Arr(Chunk(Json.Str("none"), Json.Str("client_secret_basic")))),
        Option.when(config.cimdSupported)("client_id_metadata_document_supported" -> Json.Bool(true)),
        Option.when(config.issAdvertised)("authorization_response_iss_parameter_supported" -> Json.Bool(true)),
      ).flatten
    )

  private val jwksJson: String =
    s"""{"keys":[${rsaKey.toPublicJWK.toJSONString}]}"""

  private def mintJwt(record: CodeRecord): Task[String] =
    ZIO.attempt {
      val now = java.time.Instant.now()
      val claims = new JWTClaimsSet.Builder()
        .issuer(issuer)
        .subject(record.clientId)
        .audience(record.resource.getOrElse("urn:test-idp:no-resource"))
        .claim("client_id", record.clientId)
        .claim("scope", record.scope.getOrElse(""))
        .issueTime(Date.from(now))
        .expirationTime(Date.from(now.plusSeconds(3600)))
        .jwtID(java.util.UUID.randomUUID().toString)
        .build()
      val jwt = new SignedJWT(new JWSHeader.Builder(JWSAlgorithm.RS256).keyID(rsaKey.getKeyID).build(), claims)
      jwt.sign(new RSASSASigner(rsaKey.toRSAPrivateKey))
      jwt.serialize()
    }

  /**
   * Validate the client identity on an authorization request. URL-formatted
   * `client_id`s are treated as Client ID Metadata Documents: the document is
   * fetched and must declare a matching `client_id` and list the `redirect_uri`.
   */
  private def validateClient(clientId: String, redirectUri: String): IO[String, Unit] =
    if clientId.startsWith("http://") || clientId.startsWith("https://") then
      if !config.cimdSupported then ZIO.fail("URL-formatted client_id but CIMD is not supported")
      else
        for
          url  <- ZIO.fromEither(URL.decode(clientId)).mapError(e => s"Invalid client_id URL: ${e.getMessage}")
          resp <- client.batched(Request.get(url)).mapError(t => s"Failed to fetch client metadata: ${t.getMessage}")
          body <- resp.body.asString.mapError(t => s"Failed to read client metadata: ${t.getMessage}")
          _    <- ZIO.fail(s"Client metadata fetch returned ${resp.status.code}").when(!resp.status.isSuccess)
          json <- ZIO.fromEither(body.fromJson[Json.Obj]).mapError(e => s"Client metadata is not valid JSON: $e")
          _    <- events.update(e => e.copy(cimdFetches = e.cimdFetches :+ clientId))
          docClientId = json.get("client_id").flatMap(_.asString)
          _    <- ZIO.fail(s"client_id in metadata document (${docClientId.getOrElse("missing")}) does not match its URL ($clientId)")
                    .when(!docClientId.contains(clientId))
          uris  = json.get("redirect_uris").flatMap(_.asArray).map(_.flatMap(_.asString)).getOrElse(Chunk.empty)
          _    <- ZIO.fail(s"redirect_uri '$redirectUri' not in client metadata redirect_uris")
                    .when(!uris.contains(redirectUri))
        yield ()
    else
      registrations.get.flatMap: regs =>
        regs.get(clientId) match
          case Some(uris) =>
            ZIO.fail(s"redirect_uri '$redirectUri' not registered for '$clientId'").when(!uris.contains(redirectUri)).unit
          case None =>
            // Pre-registered clients are accepted with any redirect URI: the test
            // owns both sides, and real redirect-URI policy is exercised via CIMD/DCR.
            ZIO.unit

  def routes: Routes[Any, Response] = Routes(
    Method.GET / ".well-known" / "oauth-authorization-server" -> handler(Response.json(metadataJson.toJson)),
    Method.GET / "jwks" -> handler(Response.json(jwksJson)),

    Method.GET / "authorize" -> handler { (req: Request) =>
      val q = (name: String) => req.url.queryParams.queryParam(name)
      val redirectUri = q("redirect_uri").getOrElse("")
      val clientId    = q("client_id").getOrElse("")
      val event = AuthorizeEvent(
        clientId = clientId,
        redirectUri = redirectUri,
        codeChallenge = q("code_challenge"),
        codeChallengeMethod = q("code_challenge_method"),
        resource = q("resource"),
        scope = q("scope"),
        state = q("state"),
      )
      for
        _      <- events.update(e => e.copy(authorizations = e.authorizations :+ event))
        result <- validateClient(clientId, redirectUri).either
        resp   <- result match
                    case Left(err) =>
                      events
                        .update(e => e.copy(rejections = e.rejections :+ err))
                        .as(Response.json(Json.Obj(Chunk("error" -> Json.Str("invalid_client"), "error_description" -> Json.Str(err))).toJson).status(Status.BadRequest))
                    case Right(_) =>
                      for
                        code <- Random.nextUUID.map(_.toString)
                        _    <- codes.update(_ + (code -> CodeRecord(clientId, q("code_challenge"), q("resource"), q("scope"))))
                      yield
                        val loc = URL.decode(redirectUri).toOption.get
                          .addQueryParam("code", code)
                        val withState = q("state").fold(loc)(s => loc.addQueryParam("state", s))
                        val withIss = config.issMode match
                          case IssMode.Correct => withState.addQueryParam("iss", issuer)
                          case IssMode.Wrong   => withState.addQueryParam("iss", "https://evil.example.com")
                          case IssMode.Omit    => withState
                        Response.status(Status.Found).addHeader(Header.Location(withIss))
      yield resp
    },

    Method.POST / "token" -> handler { (req: Request) =>
      for
        form   <- req.body.asString.orElseSucceed("").map(parseForm)
        event   = TokenEvent(
                    grantType = form.getOrElse("grant_type", ""),
                    clientId = form.get("client_id"),
                    codeVerifier = form.get("code_verifier"),
                    resource = form.get("resource"),
                  )
        _      <- events.update(e => e.copy(tokens = e.tokens :+ event))
        resp   <- form.getOrElse("grant_type", "") match
                    case "authorization_code" => handleCodeGrant(form)
                    case "refresh_token"      => handleRefreshGrant(form)
                    case other                => ZIO.succeed(tokenError("unsupported_grant_type", s"grant_type '$other'"))
      yield resp
    },
  ) ++ (if config.dcrEnabled then dcrRoutes else Routes.empty)

  private def handleCodeGrant(form: Map[String, String]): UIO[Response] =
    val outcome =
      for
        code   <- ZIO.fromOption(form.get("code")).orElseFail("missing 'code'")
        record <- codes.get.map(_.get(code)).someOrFail(s"unknown code '$code'")
        _      <- codes.update(_ - code)
        _      <- record.codeChallenge match
                    case None => ZIO.fail("no code_challenge was sent on the authorization request — PKCE is required")
                    case Some(challenge) =>
                      form.get("code_verifier") match
                        case None           => ZIO.fail("missing code_verifier — PKCE is required")
                        case Some(verifier) =>
                          ZIO.fail(s"code_verifier does not match code_challenge").when(sha256Url(verifier) != challenge).unit
        jwt     <- mintJwt(record.copy(resource = form.get("resource").orElse(record.resource))).mapError(_.getMessage)
        refresh <- Random.nextUUID.map(_.toString)
        _       <- refreshTokens.update(_ + (refresh -> record))
      yield tokenSuccess(jwt, Some(refresh), record.scope)
    outcome.foldZIO(err => ZIO.succeed(tokenError("invalid_grant", err)), ZIO.succeed(_))

  private def handleRefreshGrant(form: Map[String, String]): UIO[Response] =
    val outcome =
      for
        refresh <- ZIO.fromOption(form.get("refresh_token")).orElseFail("missing 'refresh_token'")
        record  <- refreshTokens.get.map(_.get(refresh)).someOrFail(s"unknown refresh_token")
        jwt     <- mintJwt(record.copy(resource = form.get("resource").orElse(record.resource))).mapError(_.getMessage)
      yield tokenSuccess(jwt, Some(refresh), record.scope)
    outcome.foldZIO(err => ZIO.succeed(tokenError("invalid_grant", err)), ZIO.succeed(_))

  private def dcrRoutes: Routes[Any, Response] = Routes(
    Method.POST / "register" -> handler { (req: Request) =>
      for
        body <- req.body.asString.orElseSucceed("{}")
        json  = body.fromJson[Json.Obj].getOrElse(Json.Obj())
        uris  = json.get("redirect_uris").flatMap(_.asArray).map(_.flatMap(_.asString)).getOrElse(Chunk.empty)
        id   <- Random.nextUUID.map(u => s"dcr-${u.toString.take(8)}")
        _    <- registrations.update(_ + (id -> uris))
        _    <- events.update(e => e.copy(dcrRegistrations = e.dcrRegistrations :+ json))
      yield Response.json(
        Json.Obj(Chunk(
          "client_id" -> Json.Str(id),
          "redirect_uris" -> Json.Arr(uris.map(Json.Str(_))),
          "token_endpoint_auth_method" -> Json.Str("none"),
        )).toJson
      ).status(Status.Created)
    },
  )

  private def tokenSuccess(jwt: String, refresh: Option[String], scope: Option[String]): Response =
    Response.json(
      Json.Obj(
        Chunk(
          Some("access_token" -> Json.Str(jwt)),
          Some("token_type" -> Json.Str("Bearer")),
          Some("expires_in" -> Json.Num(3600)),
          refresh.map(r => "refresh_token" -> Json.Str(r)),
          scope.map(s => "scope" -> Json.Str(s)),
        ).flatten
      ).toJson
    )

  private def tokenError(error: String, description: String): Response =
    Response.json(
      Json.Obj(Chunk("error" -> Json.Str(error), "error_description" -> Json.Str(description))).toJson
    ).status(Status.BadRequest)

  private def parseForm(body: String): Map[String, String] =
    body.split('&').toList.flatMap { pair =>
      pair.split("=", 2) match
        case Array(k, v) => Some(java.net.URLDecoder.decode(k, "UTF-8") -> java.net.URLDecoder.decode(v, "UTF-8"))
        case _           => None
    }.toMap

  private def sha256Url(verifier: String): String =
    val digest = java.security.MessageDigest.getInstance("SHA-256")
      .digest(verifier.getBytes(java.nio.charset.StandardCharsets.US_ASCII))
    java.util.Base64.getUrlEncoder.withoutPadding.encodeToString(digest)

object TestIdp:

  enum IssMode:
    case Correct, Wrong, Omit
  object IssMode:
    given CanEqual[IssMode, IssMode] = CanEqual.derived

  /**
   * @param issMode        whether the authorization redirect carries a correct, wrong,
   *                       or no RFC 9207 `iss` parameter
   * @param issAdvertised  whether metadata advertises `authorization_response_iss_parameter_supported`
   * @param cimdSupported  whether metadata advertises `client_id_metadata_document_supported`
   *                       (and URL client_ids are dereferenced)
   * @param dcrEnabled     whether a `registration_endpoint` is advertised and served
   */
  final case class Config(
    issMode: IssMode = IssMode.Correct,
    issAdvertised: Boolean = true,
    cimdSupported: Boolean = true,
    dcrEnabled: Boolean = false,
  )

  final case class CodeRecord(
    clientId: String,
    codeChallenge: Option[String],
    resource: Option[String],
    scope: Option[String],
  )

  final case class AuthorizeEvent(
    clientId: String,
    redirectUri: String,
    codeChallenge: Option[String],
    codeChallengeMethod: Option[String],
    resource: Option[String],
    scope: Option[String],
    state: Option[String],
  )

  final case class TokenEvent(
    grantType: String,
    clientId: Option[String],
    codeVerifier: Option[String],
    resource: Option[String],
  )

  final case class Events(
    cimdFetches: Chunk[String] = Chunk.empty,
    authorizations: Chunk[AuthorizeEvent] = Chunk.empty,
    tokens: Chunk[TokenEvent] = Chunk.empty,
    dcrRegistrations: Chunk[Json.Obj] = Chunk.empty,
    rejections: Chunk[String] = Chunk.empty,
  )

  def make(issuer: String, config: Config = Config()): ZIO[Client, Nothing, TestIdp] =
    for
      client        <- ZIO.service[Client]
      rsaKey        <- ZIO.succeed(generateKey())
      codes         <- Ref.make(Map.empty[String, CodeRecord])
      refreshTokens <- Ref.make(Map.empty[String, CodeRecord])
      registrations <- Ref.make(Map.empty[String, Chunk[String]])
      events        <- Ref.make(Events())
    yield new TestIdp(issuer, config, client, rsaKey, codes, refreshTokens, registrations, events)

  /**
   * Run a [[TestIdp]] on a free port for the duration of the scope. `extraRoutes`
   * (e.g. a hosted Client ID Metadata Document) are served on the same port; they
   * receive the chosen port so URLs in their bodies can be self-referential.
   */
  def serveScoped(
    config: Config = Config(),
    extraRoutes: Int => Routes[Any, Response] = _ => Routes.empty,
  ): ZIO[Client & Scope, Throwable, TestIdp] =
    for
      port <- AuthTestHelpers.findFreePort
      idp  <- make(s"http://localhost:$port", config)
      _    <- Server.serve(idp.routes ++ extraRoutes(port))
                .provide(Server.defaultWithPort(port))
                .forkScoped
      _    <- AuthTestHelpers.waitForBind(port)
    yield idp

  private def generateKey(): RSAKey =
    val gen = KeyPairGenerator.getInstance("RSA")
    gen.initialize(2048)
    val kp = gen.generateKeyPair()
    new RSAKey.Builder(kp.getPublic.asInstanceOf[RSAPublicKey])
      .privateKey(kp.getPrivate.asInstanceOf[RSAPrivateKey])
      .keyID("test-idp-key")
      .build()
