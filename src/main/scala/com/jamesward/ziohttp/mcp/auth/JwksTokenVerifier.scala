package com.jamesward.ziohttp.mcp.auth

import com.guizmaii.scalajwt.core.{InvalidToken, JwtToken, SupportedJWSAlgorithm}
import com.guizmaii.scalajwt.zio.{JwksConfig, ZioJwtValidator}
import com.nimbusds.jose.proc.SecurityContext
import com.nimbusds.jwt.proc.{DefaultJWTClaimsVerifier, JWTClaimsSetVerifier}
import com.nimbusds.jwt.JWTClaimsSet
import zio.*
import zio.http.*
import zio.json.*
import zio.json.ast.Json
import zio.telemetry.opentelemetry.OpenTelemetry

import scala.jdk.CollectionConverters.*

/**
 * JWT bearer-token verifier built on top of [[com.guizmaii.scalajwt.zio.ZioJwtValidator]].
 *
 * The library handles:
 *   - Initial JWKS fetch (fail-fast at startup, with retries)
 *   - Background refresh (default every 4 minutes)
 *   - Lock-free, non-blocking JWT validation against the cached JWKS
 *   - `iss`, `exp`, `nbf` claim validation via a `DefaultJWTClaimsVerifier`
 *
 * Audience binding (`aud`) is intentionally NOT validated here so that
 * [[McpAuth.resourceUri]] can be derived per-request — the auth middleware does that check.
 *
 * RSA-only in v1 (RS256). EC and EdDSA support is future work.
 */
final class JwksTokenVerifier private (
  expectedIssuer: String,
  validator: ZioJwtValidator,
) extends TokenVerifier[Any]:

  def verify(rawToken: String): ZIO[Any, AuthError, Principal] =
    validator
      .validate(JwtToken(rawToken))
      .mapError(translateError)
      .flatMap(claims => buildPrincipal(rawToken, claims))

  /** Translate the underlying lib's [[InvalidToken]] into our [[AuthError]] taxonomy. */
  private def translateError(err: InvalidToken): AuthError =
    val msg = Option(err.message).getOrElse("Invalid token")
    val lower = msg.toLowerCase
    if lower.contains("expired") then AuthError.Expired
    else if lower.contains("iss") && (lower.contains("claim") || lower.contains("issuer")) then
      AuthError.IssuerMismatch(expectedIssuer, None)
    else AuthError.Invalid(msg)

  private def buildPrincipal(rawToken: String, claims: JWTClaimsSet): IO[AuthError, Principal] =
    val issuer   = Option(claims.getIssuer)
    val audience = Option(claims.getAudience).map(_.asScala.toSet).getOrElse(Set.empty)
    val claimsJson = jwtClaimsAsJsonObj(claims)
    val scopeStr = claimsJson.get("scope") match
      case Some(Json.Str(s))  => s
      case Some(Json.Arr(xs)) => xs.collect { case Json.Str(s) => s }.mkString(" ")
      case _                  => ""
    val scopes   = scopeStr.split("\\s+").filter(_.nonEmpty).map(OauthScope(_)).toSet
    val clientId = claimsJson.get("client_id").flatMap(_.asString)
      .orElse(claimsJson.get("azp").flatMap(_.asString))
    val subject  = Option(claims.getSubject)
    val exp      = Option(claims.getExpirationTime).map(_.toInstant)
    ZIO.succeed(Principal(
      subject = subject,
      clientId = clientId,
      scopes = scopes,
      audience = audience,
      issuer = issuer,
      expiresAt = exp,
      raw = rawToken,
      claims = claimsJson,
    ))

  /** Convert nimbus's JWTClaimsSet to a `zio-json` `Json.Obj` for [[Principal.claims]]. */
  private def jwtClaimsAsJsonObj(claims: JWTClaimsSet): Json.Obj =
    claims.toString.fromJson[Json.Obj].getOrElse(Json.Obj())

object JwksTokenVerifier:

  /**
   * Discover the JWKS URI from the issuer's RFC 8414 metadata (with OIDC fallback) and
   * build a verifier. JWKS is fetched eagerly at startup (fail-fast) and refreshed every
   * `refreshInterval` in a background fiber.
   *
   * The validator's lifetime is tied to the surrounding [[zio.Scope]] — when the scope
   * closes, the background refresh fiber terminates. In a typical `ZIOAppDefault` app
   * the scope lives for the lifetime of the app, which is what you want.
   */
  def discoverJwks(
    issuer: String,
    refreshInterval: Duration = 4.minutes,
    fetchTimeout: Duration = 30.seconds,
  ): ZIO[Client & Scope, Throwable, TokenVerifier[Any]] =
    for
      client  <- ZIO.service[Client]
      jwksUri <- discoverJwksUri(issuer, client)
      v       <- buildValidator(issuer, jwksUri, refreshInterval, fetchTimeout)
    yield new JwksTokenVerifier(issuer, v)

  /** Build a verifier with a known JWKS URI (no metadata discovery). */
  def jwks(
    jwksUri: String,
    expectedIssuer: String,
    refreshInterval: Duration = 4.minutes,
    fetchTimeout: Duration = 30.seconds,
  ): ZIO[Client & Scope, Throwable, TokenVerifier[Any]] =
    for
      url <- ZIO.fromEither(URL.decode(jwksUri))
      v   <- buildValidator(expectedIssuer, url, refreshInterval, fetchTimeout)
    yield new JwksTokenVerifier(expectedIssuer, v)

  /**
   * Build a [[ZioJwtValidator]] in the surrounding scope. We use a noop OpenTelemetry
   * tracer because the library requires one but we don't expose telemetry through this
   * layer (apps that want it can wire their own tracer at the layer level).
   */
  private def buildValidator(
    expectedIssuer: String,
    jwksUri: URL,
    refreshInterval: Duration,
    fetchTimeout: Duration,
  ): ZIO[Client & Scope, Throwable, ZioJwtValidator] =
    val claimsVerifier: JWTClaimsSetVerifier[SecurityContext] =
      val exactMatch = new JWTClaimsSet.Builder().issuer(expectedIssuer).build()
      // acceptedAudience = null means "accept any audience" (the audience check happens in
      // the auth middleware against per-request resourceUri).
      // requiredClaims includes "exp" so missing-exp tokens are rejected.
      new DefaultJWTClaimsVerifier[SecurityContext](
        null.asInstanceOf[java.util.Set[String]],
        exactMatch,
        java.util.Collections.singleton[String]("exp"),
        java.util.Collections.emptySet[String](),
      )

    val configLayer = ZLayer.succeed(
      JwksConfig(jwksUri = jwksUri, refreshInterval = refreshInterval, fetchTimeout = fetchTimeout)
    )
    val tracerLayer = (OpenTelemetry.noop() >>> OpenTelemetry.tracer("zio-http-mcp"))
    val validatorLayer = ZioJwtValidator.configured(claimsVerifier, SupportedJWSAlgorithm.RS256)

    val combined = (configLayer ++ tracerLayer ++ ZLayer.environment[Client]) >>> validatorLayer
    combined.build.map(_.get[ZioJwtValidator])

  // --- AS metadata discovery (kept from the previous in-house impl) ---

  /**
   * Fetch the AS metadata document at `<issuer>/.well-known/oauth-authorization-server`
   * (with OIDC fallback) and extract the `jwks_uri`.
   */
  private def discoverJwksUri(issuer: String, client: Client): IO[Throwable, URL] =
    val base = issuer.stripSuffix("/")
    val oauthMeta = s"$base/.well-known/oauth-authorization-server"
    val oidcMeta  = s"$base/.well-known/openid-configuration"
    fetchJwksUri(oauthMeta, client).orElse(fetchJwksUri(oidcMeta, client))

  private def fetchJwksUri(metadataUrl: String, client: Client): IO[Throwable, URL] =
    for
      url  <- ZIO.fromEither(URL.decode(metadataUrl))
      resp <- client.batched(Request.get(url))
      _    <- ZIO.fail(RuntimeException(s"AS metadata returned ${resp.status}"))
                .when(!resp.status.isSuccess)
      body <- resp.body.asString
      json <- ZIO.fromEither(body.fromJson[Json.Obj])
                .mapError(e => RuntimeException(s"Invalid AS metadata JSON: $e"))
      uriS <- ZIO.fromOption(json.get("jwks_uri").flatMap(_.asString))
                .orElseFail(RuntimeException("AS metadata missing 'jwks_uri'"))
      uri  <- ZIO.fromEither(URL.decode(uriS))
    yield uri
