package com.jamesward.ziohttp.mcp.auth

import zio.*
import zio.json.ast.Json

import java.time.Instant

/**
 * The authenticated identity behind an MCP request. Available to tool handlers via
 * [[com.jamesward.ziohttp.mcp.McpToolContext.principal]] when authorization is enabled.
 *
 * @param subject    `sub` claim — the user or client identifier.
 * @param clientId   `client_id` / `azp` — the OAuth client that obtained the token.
 * @param scopes     Granted scopes (parsed from `scope` claim).
 * @param audience   `aud` claim — entries the token was issued for.
 * @param issuer     `iss` claim — the AS that issued the token.
 * @param expiresAt  `exp` claim — token expiry.
 * @param raw        The original bearer token, opaque to handlers.
 * @param claims     Full claim set as a JSON object (includes anything the verifier doesn't normalize).
 */
final case class Principal(
  subject: Option[String],
  clientId: Option[String],
  scopes: Set[OauthScope],
  audience: Set[String],
  issuer: Option[String],
  expiresAt: Option[Instant],
  raw: String,
  claims: Json.Obj,
):
  def hasScope(scope: OauthScope): Boolean = scopes.contains(scope)

  def hasAllScopes(required: Set[OauthScope]): Boolean = required.subsetOf(scopes)

object Principal:
  given CanEqual[Principal, Principal] = CanEqual.derived

/**
 * Errors a [[TokenVerifier]] may return. The auth middleware translates these into
 * appropriate HTTP responses (`401`/`403`/`503`) with `WWW-Authenticate` challenges.
 */
enum AuthError:
  /** No `Authorization` header on the request. Client should authenticate. */
  case Missing
  /** Token failed signature, structure, or general validation. */
  case Invalid(reason: String)
  /** Token expired (`exp` in the past). */
  case Expired
  /** `aud` claim does not include this resource. */
  case AudienceMismatch(expected: ResourceUri, actual: Set[String])
  /** `iss` claim does not match a configured authorization server. */
  case IssuerMismatch(expected: String, actual: Option[String])
  /** Token is otherwise valid but lacks the required scopes (yields 403, not 401). */
  case InsufficientScope(required: Set[OauthScope], actual: Set[OauthScope])
  /** Verifier could not reach the AS or otherwise failed independently of the token. */
  case UpstreamFailure(reason: String)

object AuthError:
  given CanEqual[AuthError, AuthError] = CanEqual.derived

  extension (e: AuthError) def description: String = e match
    case Missing                      => "Missing access token"
    case Invalid(reason)              => s"Invalid token: $reason"
    case Expired                      => "Token expired"
    case AudienceMismatch(exp, act)   =>
      s"Audience mismatch: expected ${exp.value}, got ${if act.isEmpty then "none" else act.mkString(", ")}"
    case IssuerMismatch(exp, act)     =>
      s"Issuer mismatch: expected $exp, got ${act.getOrElse("none")}"
    case InsufficientScope(req, act)  =>
      val missing = req -- act
      s"Insufficient scope: missing ${missing.map(_.value).mkString(", ")}"
    case UpstreamFailure(reason)      => s"Authorization server unreachable: $reason"
