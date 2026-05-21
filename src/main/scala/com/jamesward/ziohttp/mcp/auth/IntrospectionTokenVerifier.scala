package com.jamesward.ziohttp.mcp.auth

import zio.*
import zio.http.*
import zio.json.*
import zio.json.ast.Json

import java.nio.charset.StandardCharsets
import java.time.Instant
import java.util.Base64

/**
 * RFC 7662 OAuth 2.0 Token Introspection. Posts the bearer token to the AS's introspection
 * endpoint with the registered MCP server's client credentials and parses the response.
 *
 * Use this when the AS issues opaque tokens (no public verification possible) or when the
 * operator wants real-time revocation. For JWT tokens with a published JWKS, prefer
 * [[TokenVerifier.discoverJwks]] (no AS round-trip per request, no client secret needed).
 *
 * Audience binding is enforced by the auth middleware, not here.
 *
 * @param endpoint              The introspection endpoint URL.
 * @param clientId              Client ID for HTTP Basic auth on the introspection request.
 * @param clientSecret          Client secret (treated as a [[zio.Config.Secret]]).
 * @param expectedIssuer        `iss` value the introspection response must contain.
 */
final class IntrospectionTokenVerifier private (
  endpoint: URL,
  clientId: String,
  clientSecret: Config.Secret,
  expectedIssuer: String,
) extends TokenVerifier[Client]:

  def verify(rawToken: String): ZIO[Client, AuthError, Principal] =
    ZIO.scoped {
      for
        client <- ZIO.service[Client]
        resp   <- callIntrospection(client, rawToken)
        body   <- resp.body.asString
                    .mapError(t => AuthError.UpstreamFailure(s"Failed to read introspection body: ${t.getMessage}"))
        _      <- ZIO.fail(AuthError.UpstreamFailure(s"Introspection returned ${resp.status}: $body"))
                    .when(!resp.status.isSuccess)
        json   <- ZIO.fromEither(body.fromJson[Json.Obj])
                    .mapError(e => AuthError.UpstreamFailure(s"Invalid introspection JSON: $e"))
        principal <- buildPrincipal(rawToken, json)
      yield principal
    }

  private def callIntrospection(client: Client, token: String): ZIO[Scope, AuthError, Response] =
    val secret = String(clientSecret.value.toArray)
    val basic = Base64.getEncoder.encodeToString(s"$clientId:$secret".getBytes(StandardCharsets.UTF_8))
    val form = s"token=${java.net.URLEncoder.encode(token, StandardCharsets.UTF_8)}&token_type_hint=access_token"
    val request = Request.post(endpoint, Body.fromString(form))
      .addHeader(Header.ContentType(MediaType.application.`x-www-form-urlencoded`))
      .addHeader(Header.Authorization.Basic(clientId, secret))
      .addHeader("accept", "application/json")
    client.batched(request)
      .mapError(t => AuthError.UpstreamFailure(s"Introspection request failed: ${t.getMessage}"))

  private def buildPrincipal(rawToken: String, body: Json.Obj): IO[AuthError, Principal] =
    val active = body.get("active").flatMap(_.asBoolean).getOrElse(false)
    if !active then ZIO.fail(AuthError.Invalid("Token is not active per introspection"))
    else
      val issuer  = body.get("iss").flatMap(_.asString)
      val aud     = body.get("aud") match
        case Some(Json.Str(s))  => Set(s)
        case Some(Json.Arr(xs)) => xs.collect { case Json.Str(s) => s }.toSet
        case _                   => Set.empty[String]

      if !issuer.contains(expectedIssuer) then
        ZIO.fail(AuthError.IssuerMismatch(expectedIssuer, issuer))
      else
        val scopeStr = body.get("scope").flatMap(_.asString).getOrElse("")
        val scopes = scopeStr.split("\\s+").filter(_.nonEmpty).map(OauthScope(_)).toSet
        val expiresAt = body.get("exp").flatMap(_.asNumber).map(n => Instant.ofEpochSecond(n.value.longValueExact))
        val subject = body.get("sub").flatMap(_.asString)
        val cId = body.get("client_id").flatMap(_.asString)
        ZIO.succeed(Principal(
          subject = subject,
          clientId = cId,
          scopes = scopes,
          audience = aud,
          issuer = issuer,
          expiresAt = expiresAt,
          raw = rawToken,
          claims = body,
        ))

object IntrospectionTokenVerifier:
  /**
   * Build an RFC 7662 introspection-based verifier.
   *
   * Audience binding is enforced by the auth middleware against [[McpAuth.resourceUri]],
   * not here.
   */
  def apply(
    endpoint: URL,
    clientId: String,
    clientSecret: Config.Secret,
    expectedIssuer: String,
  ): TokenVerifier[Client] =
    new IntrospectionTokenVerifier(endpoint, clientId, clientSecret, expectedIssuer)
