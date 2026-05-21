package com.jamesward.ziohttp.mcp.auth

import com.nimbusds.jose.{JOSEException, JWSAlgorithm}
import com.nimbusds.jose.crypto.RSASSAVerifier
import com.nimbusds.jose.jwk.{JWK, JWKSet, RSAKey}
import com.nimbusds.jwt.{JWTClaimsSet, SignedJWT}
import zio.*
import zio.http.*
import zio.json.*
import zio.json.ast.Json

import java.security.PublicKey
import java.text.ParseException
import java.time.Instant
import scala.jdk.CollectionConverters.*

/**
 * Validates a JWT bearer token by:
 *
 *   1. Parsing the JWT and extracting the (untrusted) `kid` from the header.
 *   2. Looking up the matching public key from a JWKS document fetched from the AS.
 *   3. Verifying the RSA signature with `nimbus-jose-jwt`.
 *   4. Validating `iss`, `exp`, `nbf` claims.
 *   5. Building a [[Principal]] from the claims.
 *
 * Audience validation is the responsibility of the auth middleware (so the resource URI can
 * be derived per-request rather than baked into the verifier at construction time).
 *
 * The AS metadata document and JWKS are cached with a configurable TTL.
 *
 * RSA only in v1 (RS256/RS384/RS512). EC and EdDSA support is future work.
 */
final class JwksTokenVerifier private (
  expectedIssuer: String,
  jwksProvider: JwksProvider,
  clockSkew: Duration,
) extends TokenVerifier[Any]:

  def verify(rawToken: String): ZIO[Any, AuthError, Principal] =
    for
      signed    <- parseSigned(rawToken)
      kid       <- ZIO.fromOption(Option(signed.getHeader.getKeyID))
                     .orElseFail(AuthError.Invalid("JWT header missing 'kid'"))
      _         <- requireRsa(signed.getHeader.getAlgorithm)
      key       <- jwksProvider.lookup(kid)
                     .orElse(jwksProvider.refreshAndLookup(kid))
                     .orElseFail(AuthError.Invalid(s"No JWKS key found for kid '$kid'"))
      _         <- verifySignature(signed, key)
      claims    <- ZIO.fromTry(scala.util.Try(signed.getJWTClaimsSet))
                     .mapError(t => AuthError.Invalid(s"Failed to read JWT claims: ${t.getMessage}"))
      _         <- validateTiming(claims)
      principal <- buildPrincipal(rawToken, claims)
    yield principal

  private def parseSigned(rawToken: String): IO[AuthError, SignedJWT] =
    ZIO.attempt(SignedJWT.parse(rawToken)).mapError {
      case _: ParseException => AuthError.Invalid("Malformed JWT")
      case t                 => AuthError.Invalid(s"Failed to parse JWT: ${t.getMessage}")
    }

  private def requireRsa(alg: JWSAlgorithm): IO[AuthError, Unit] =
    val supported = Set(JWSAlgorithm.RS256, JWSAlgorithm.RS384, JWSAlgorithm.RS512)
    ZIO.unless(supported.contains(alg))(
      ZIO.fail(AuthError.Invalid(s"Unsupported JWT algorithm: $alg (only RS256/RS384/RS512 are supported in v1)"))
    ).unit

  private def verifySignature(signed: SignedJWT, key: PublicKey): IO[AuthError, Unit] =
    key match
      case rsa: java.security.interfaces.RSAPublicKey =>
        val verifier = new RSASSAVerifier(rsa)
        ZIO.attempt(signed.verify(verifier))
          .mapError(t => AuthError.Invalid(s"Signature verification error: ${t.getMessage}"))
          .flatMap(ok => ZIO.fail(AuthError.Invalid("Signature does not match")).when(!ok).unit)
      case _ =>
        ZIO.fail(AuthError.Invalid("Public key is not an RSA key"))

  private def validateTiming(claims: JWTClaimsSet): IO[AuthError, Unit] =
    val now = java.time.Instant.now()
    val skewMs = clockSkew.toMillis
    val exp = Option(claims.getExpirationTime).map(_.toInstant)
    val nbf = Option(claims.getNotBeforeTime).map(_.toInstant)

    val expired  = exp.exists(e => now.toEpochMilli > e.toEpochMilli + skewMs)
    val notYet   = nbf.exists(n => now.toEpochMilli < n.toEpochMilli - skewMs)
    if expired then ZIO.fail(AuthError.Expired)
    else if notYet then ZIO.fail(AuthError.Invalid("Token not yet valid (nbf claim in future)"))
    else ZIO.unit

  private def buildPrincipal(rawToken: String, claims: JWTClaimsSet): IO[AuthError, Principal] =
    val issuer   = Option(claims.getIssuer)
    val audience = Option(claims.getAudience).map(_.asScala.toSet).getOrElse(Set.empty)

    if !issuer.contains(expectedIssuer) then
      ZIO.fail(AuthError.IssuerMismatch(expectedIssuer, issuer))
    else
      val claimsJson = jwtClaimsAsJsonObj(claims)
      val scopeStr   = claimsJson.get("scope") match
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

  /** Convert nimbus's JWTClaimsSet to a `zio-json` `Json.Obj` for `Principal.claims`. */
  private def jwtClaimsAsJsonObj(claims: JWTClaimsSet): Json.Obj =
    val raw = claims.toString  // nimbus serializes to JSON
    raw.fromJson[Json.Obj].getOrElse(Json.Obj())

object JwksTokenVerifier:

  /**
   * Discover the JWKS URI from the issuer's RFC 8414 metadata (with OIDC fallback) and build
   * a verifier. The JWKS document is fetched lazily on first verification and cached.
   *
   * Audience binding (RFC 8707) is enforced by the auth middleware against
   * [[McpAuth.resourceUri]] (or its per-request derivation), not here.
   */
  def discoverJwks(
    issuer: String,
    cacheTtl: Duration = 1.hour,
    clockSkew: Duration = 60.seconds,
  ): ZIO[Client, Nothing, TokenVerifier[Any]] =
    for
      client   <- ZIO.service[Client]
      provider <- JwksProvider.discover(issuer, client, cacheTtl)
    yield new JwksTokenVerifier(issuer, provider, clockSkew)

  /** Build a verifier that fetches its JWKS from a known URL (no metadata discovery). */
  def jwks(
    jwksUri: String,
    expectedIssuer: String,
    cacheTtl: Duration = 1.hour,
    clockSkew: Duration = 60.seconds,
  ): ZIO[Client, Nothing, TokenVerifier[Any]] =
    for
      client   <- ZIO.service[Client]
      provider <- JwksProvider.fixed(jwksUri, client, cacheTtl)
    yield new JwksTokenVerifier(expectedIssuer, provider, clockSkew)

  /** Test-visible re-export: parse a JWKS document into kid → PublicKey map. */
  private[mcp] def parseJwks(body: String): Either[String, Map[String, PublicKey]] =
    JwksProvider.parseJwks(body)

// --- JWKS provider with caching ---

private[auth] trait JwksProvider:
  /** Look up a key by kid from the cache. Fails with `Missing` if not present. */
  def lookup(kid: String): IO[AuthError, PublicKey]
  /** Force a refresh of the JWKS document and look up a key. */
  def refreshAndLookup(kid: String): IO[AuthError, PublicKey]

private[auth] object JwksProvider:

  /** Cached JWKS keyed by kid, with the time the keys were fetched. */
  private case class Cache(keys: Map[String, PublicKey], fetchedAt: Instant):
    def isFresh(now: Instant, ttl: Duration): Boolean =
      java.time.Duration.between(fetchedAt, now).toMillis < ttl.toMillis

  /** Discover the jwks_uri via RFC 8414 / OIDC metadata, then fetch the JWKS. */
  def discover(issuer: String, client: Client, cacheTtl: Duration): UIO[JwksProvider] =
    Ref.make[Option[Cache]](None).flatMap { cache =>
      Ref.make[Option[String]](None).map { jwksUriRef =>
        new JwksProvider:
          def lookup(kid: String): IO[AuthError, PublicKey] =
            for
              now    <- Clock.instant
              cached <- cache.get
              key    <- cached match
                          case Some(c) if c.isFresh(now, cacheTtl) =>
                            ZIO.fromOption(c.keys.get(kid)).orElseFail(AuthError.Missing)
                          case _ =>
                            ZIO.fail(AuthError.Missing)
            yield key

          def refreshAndLookup(kid: String): IO[AuthError, PublicKey] =
            for
              jwksUri <- resolveJwksUri(issuer, client, jwksUriRef)
              keys    <- fetchAndParseJwks(jwksUri, client)
              now     <- Clock.instant
              _       <- cache.set(Some(Cache(keys, now)))
              key     <- ZIO.fromOption(keys.get(kid))
                           .orElseFail(AuthError.Invalid(s"No JWKS key found for kid '$kid'"))
            yield key
      }
    }

  /** Use a fixed jwks_uri without metadata discovery. */
  def fixed(jwksUri: String, client: Client, cacheTtl: Duration): UIO[JwksProvider] =
    Ref.make[Option[Cache]](None).map { cache =>
      new JwksProvider:
        def lookup(kid: String): IO[AuthError, PublicKey] =
          for
            now    <- Clock.instant
            cached <- cache.get
            key    <- cached match
                        case Some(c) if c.isFresh(now, cacheTtl) =>
                          ZIO.fromOption(c.keys.get(kid)).orElseFail(AuthError.Missing)
                        case _ =>
                          ZIO.fail(AuthError.Missing)
          yield key

        def refreshAndLookup(kid: String): IO[AuthError, PublicKey] =
          for
            keys <- fetchAndParseJwks(jwksUri, client)
            now  <- Clock.instant
            _    <- cache.set(Some(Cache(keys, now)))
            key  <- ZIO.fromOption(keys.get(kid))
                      .orElseFail(AuthError.Invalid(s"No JWKS key found for kid '$kid'"))
          yield key
    }

  /** Fetch the AS metadata document at /.well-known/oauth-authorization-server (with OIDC
   *  fallback) and extract the jwks_uri. Cached after first success. */
  private def resolveJwksUri(
    issuer: String,
    client: Client,
    jwksUriRef: Ref[Option[String]],
  ): IO[AuthError, String] =
    jwksUriRef.get.flatMap {
      case Some(uri) => ZIO.succeed(uri)
      case None =>
        val base = issuer.stripSuffix("/")
        val oauthMeta = s"$base/.well-known/oauth-authorization-server"
        val oidcMeta  = s"$base/.well-known/openid-configuration"
        fetchJwksUri(oauthMeta, client)
          .orElse(fetchJwksUri(oidcMeta, client))
          .tap(uri => jwksUriRef.set(Some(uri)))
    }

  private def fetchJwksUri(metadataUrl: String, client: Client): IO[AuthError, String] =
    ZIO.scoped {
      for
        url    <- ZIO.fromEither(URL.decode(metadataUrl))
                    .mapError(t => AuthError.UpstreamFailure(s"Bad metadata URL: ${t.getMessage}"))
        resp   <- client.request(Request.get(url))
                    .mapError(t => AuthError.UpstreamFailure(s"Failed to fetch AS metadata: ${t.getMessage}"))
        _      <- ZIO.fail(AuthError.UpstreamFailure(s"AS metadata returned ${resp.status}"))
                    .when(!resp.status.isSuccess)
        body   <- resp.body.asString
                    .mapError(t => AuthError.UpstreamFailure(s"Failed to read AS metadata body: ${t.getMessage}"))
        json   <- ZIO.fromEither(body.fromJson[Json.Obj])
                    .mapError(e => AuthError.UpstreamFailure(s"Invalid AS metadata JSON: $e"))
        uri    <- ZIO.fromOption(json.get("jwks_uri").flatMap(_.asString))
                    .orElseFail(AuthError.UpstreamFailure("AS metadata missing 'jwks_uri'"))
      yield uri
    }

  private def fetchAndParseJwks(
    jwksUri: String,
    client: Client,
  ): IO[AuthError, Map[String, PublicKey]] =
    ZIO.scoped {
      for
        url  <- ZIO.fromEither(URL.decode(jwksUri))
                  .mapError(t => AuthError.UpstreamFailure(s"Bad JWKS URL: ${t.getMessage}"))
        resp <- client.request(Request.get(url))
                  .mapError(t => AuthError.UpstreamFailure(s"Failed to fetch JWKS: ${t.getMessage}"))
        _    <- ZIO.fail(AuthError.UpstreamFailure(s"JWKS endpoint returned ${resp.status}"))
                  .when(!resp.status.isSuccess)
        body <- resp.body.asString
                  .mapError(t => AuthError.UpstreamFailure(s"Failed to read JWKS body: ${t.getMessage}"))
        keys <- ZIO.fromEither(parseJwks(body))
                  .mapError(e => AuthError.UpstreamFailure(s"Invalid JWKS document: $e"))
      yield keys
    }

  /** Parse a JWKS document via nimbus-jose-jwt. Drops keys that aren't RSA or are missing kid. */
  private[auth] def parseJwks(body: String): Either[String, Map[String, PublicKey]] =
    scala.util.Try(JWKSet.parse(body)).toEither.left.map(t => s"Failed to parse JWKS: ${t.getMessage}").map { jwks =>
      jwks.getKeys.asScala.iterator.flatMap { jwk =>
        for
          kid <- Option(jwk.getKeyID)
          key <- toPublicKey(jwk).toOption
        yield kid -> key
      }.toMap
    }

  /** Reconstruct a `java.security.PublicKey` from a JWK. RSA only in v1. */
  private[auth] def toPublicKey(jwk: JWK): Either[String, PublicKey] =
    jwk match
      case rsa: RSAKey =>
        scala.util.Try(rsa.toRSAPublicKey).toEither.left.map(t => s"RSA key error: ${t.getMessage}")
      case _ =>
        Left(s"Unsupported key type: ${jwk.getKeyType} (only RSA is supported in v1)")
