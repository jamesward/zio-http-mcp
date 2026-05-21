package com.jamesward.ziohttp.mcp.auth

import zio.*
import zio.http.Client

/**
 * Validates a bearer token and produces a [[Principal]] on success.
 *
 * Implementations are expected to handle:
 *   - signature / structural validation (for JWTs) or AS introspection (for opaque tokens),
 *   - `iss` claim validation,
 *   - `exp` / `nbf` time-bound validation.
 *
 * Audience binding (RFC 8707) and per-route scope enforcement happen in the auth middleware
 * after the verifier returns. This split keeps the verifier configuration static while
 * letting [[McpAuth.resourceUri]] be derived per-request when not explicitly configured.
 */
trait TokenVerifier[-R]:
  def verify(rawToken: String): ZIO[R, AuthError, Principal]

object TokenVerifier:
  /**
   * For tests and custom flows. The function receives the raw bearer token and returns
   * either a [[Principal]] or an [[AuthError]].
   */
  def fromFunction[R](f: String => ZIO[R, AuthError, Principal]): TokenVerifier[R] =
    new TokenVerifier[R]:
      def verify(rawToken: String): ZIO[R, AuthError, Principal] = f(rawToken)

  /**
   * JWT validation against the JWKS published by the authorization server, with metadata
   * discovery (RFC 8414, with OIDC Discovery 1.0 fallback). Validates signature, `iss`,
   * `exp`, `nbf`. JWKS is fetched eagerly at startup and refreshed every `refreshInterval`
   * by a background fiber.
   *
   * Audience binding (RFC 8707) is enforced by the auth middleware against
   * [[McpAuth.resourceUri]] (or its per-request derivation), not here.
   *
   * The validator's lifetime is tied to the surrounding [[zio.Scope]] — when the scope
   * closes, the background refresh fiber terminates. In a typical `ZIOAppDefault` app
   * the scope lives for the lifetime of the app, which is what you want.
   *
   * Supports RS256 in v1. EC and EdDSA support is future work.
   */
  def discoverJwks(
    issuer: String,
    refreshInterval: Duration = 4.minutes,
    fetchTimeout: Duration = 30.seconds,
  ): ZIO[Client & Scope, Throwable, TokenVerifier[Any]] =
    JwksTokenVerifier.discoverJwks(issuer, refreshInterval, fetchTimeout)

  /** Build a JWT verifier with a fixed `jwks_uri` (no metadata discovery). */
  def jwks(
    jwksUri: String,
    expectedIssuer: String,
    refreshInterval: Duration = 4.minutes,
    fetchTimeout: Duration = 30.seconds,
  ): ZIO[Client & Scope, Throwable, TokenVerifier[Any]] =
    JwksTokenVerifier.jwks(jwksUri, expectedIssuer, refreshInterval, fetchTimeout)

  /**
   * RFC 7662 OAuth 2.0 Token Introspection. Posts the bearer token to the AS's introspection
   * endpoint with the registered MCP server's client credentials. Use when the AS issues
   * opaque tokens or when real-time revocation is required.
   *
   * For JWT tokens with a published JWKS, prefer [[discoverJwks]] (no AS round-trip per request).
   */
  def introspection(
    endpoint: zio.http.URL,
    clientId: String,
    clientSecret: zio.Config.Secret,
    expectedIssuer: String,
  ): TokenVerifier[Client] =
    IntrospectionTokenVerifier(endpoint, clientId, clientSecret, expectedIssuer)
