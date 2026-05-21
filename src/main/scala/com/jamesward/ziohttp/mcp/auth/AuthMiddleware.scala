package com.jamesward.ziohttp.mcp.auth

import com.jamesward.ziohttp.mcp.{ErrorCode, JsonRpcError, ErrorDetail}
import zio.*
import zio.http.*
import zio.json.*

/**
 * Bearer-token extraction, audience binding, scope enforcement, and `WWW-Authenticate`
 * challenge construction.
 *
 * The audience check happens here (rather than inside the `TokenVerifier`) so the
 * server's resource URI can be derived from request headers when not explicitly
 * configured — see [[ResourceUriResolver]].
 */
private[mcp] object AuthMiddleware:

  // Reject access tokens passed via URL query (per OAuth 2.1 §5.1.1).
  private val queryTokenParams = Set("access_token", "token")

  /**
   * Extract a bearer token from the request. Returns `Right(token)` on success or
   * `Left(error)` on missing/malformed credentials.
   *
   * Per OAuth 2.1 §5.1.1, only the `Authorization: Bearer …` header form is accepted —
   * tokens in the URL query string are rejected even if structurally valid.
   */
  def extractBearerToken(request: Request): Either[AuthError, String] =
    val queryHasToken = queryTokenParams.exists(p => request.url.queryParams.queryParam(p).isDefined)
    if queryHasToken then
      Left(AuthError.Invalid("Access token must not be sent in the URL query string"))
    else
      request.rawHeader("authorization") match
        case None => Left(AuthError.Missing)
        case Some(value) =>
          val trimmed = value.trim
          val prefix = "bearer "
          if trimmed.length <= prefix.length || !trimmed.toLowerCase.startsWith(prefix) then
            Left(AuthError.Invalid("Authorization header must use the Bearer scheme"))
          else
            val token = trimmed.substring(prefix.length).trim
            if token.isEmpty then Left(AuthError.Invalid("Empty bearer token"))
            else Right(token)

  /**
   * Run the full authentication, audience-binding, and scope-enforcement check.
   *
   * The required scope set is the union of server-wide [[McpAuth.requiredScopes]] and
   * any per-route scopes.
   *
   * Audience is checked against the resolved-per-request resource URI (see
   * [[ResourceUriResolver]]).
   *
   * @param resourcePath path component used when [[McpAuth.resourceUri]] is `None` and
   *                     the resource URI is derived from request headers. Owned by
   *                     `McpServer` so the route mount path and the advertised
   *                     resource URI cannot drift.
   */
  def authenticate[R](
    auth: McpAuth[R],
    resourcePath: String,
    request: Request,
    additionalRequiredScopes: Set[OauthScope],
  ): ZIO[R, AuthError, Principal] =
    val resourceUri = ResourceUriResolver.resolve(auth.resourceUri, resourcePath, request)
    val required    = auth.requiredScopes ++ additionalRequiredScopes
    for
      raw       <- ZIO.fromEither(extractBearerToken(request))
      principal <- auth.verifier.verify(raw)
      _         <- ZIO.fail(AuthError.AudienceMismatch(resourceUri, principal.audience))
                     .when(!principal.audience.exists(resourceUri.matchesAudience))
      _         <- ZIO.fail(AuthError.InsufficientScope(required, principal.scopes))
                     .when(!principal.hasAllScopes(required))
    yield principal

  /**
   * PRM URL per [[https://datatracker.ietf.org/doc/html/rfc9728#section-3.1 RFC 9728 §3.1]]:
   * `/.well-known/oauth-protected-resource` is inserted between the host component and
   * the path component of the resource identifier.
   *
   * Examples:
   *   - resource `https://mcp.example.com/mcp` → `https://mcp.example.com/.well-known/oauth-protected-resource/mcp`
   *   - resource `https://mcp.example.com`     → `https://mcp.example.com/.well-known/oauth-protected-resource`
   */
  def resourceMetadataUrl(resourceUri: ResourceUri): String =
    val uri = resourceUri.value
    val schemeEnd = uri.indexOf("://")
    if schemeEnd < 0 then s"$uri/.well-known/oauth-protected-resource"
    else
      val pathStart = uri.indexOf('/', schemeEnd + 3)
      if pathStart < 0 then s"$uri/.well-known/oauth-protected-resource"
      else
        val origin = uri.substring(0, pathStart)
        val path   = uri.substring(pathStart).stripSuffix("/")
        if path.isEmpty then s"$origin/.well-known/oauth-protected-resource"
        else s"$origin/.well-known/oauth-protected-resource$path"

  /**
   * Build a `WWW-Authenticate: Bearer …` header per RFC 6750 §3 + RFC 9728 §5.1, including
   * the 2025-11-25 `scope` hint and (when applicable) `error` / `error_description`.
   */
  def wwwAuthenticate(
    auth: McpAuth[?],
    resourceUri: ResourceUri,
    error: AuthError,
    requiredScopes: Set[OauthScope],
  ): String =
    val scopeStr = requiredScopes.toSeq.map(_.value).sorted.mkString(" ")
    val params = scala.collection.mutable.ArrayBuffer[(String, String)]()
    params += "realm" -> auth.realm
    params += "resource_metadata" -> resourceMetadataUrl(resourceUri)
    if scopeStr.nonEmpty then params += "scope" -> scopeStr
    error match
      case AuthError.Missing => ()
      case AuthError.InsufficientScope(_, _) =>
        params += "error" -> "insufficient_scope"
        params += "error_description" -> error.description
      case AuthError.Invalid(_)
        | AuthError.Expired
        | AuthError.AudienceMismatch(_, _)
        | AuthError.IssuerMismatch(_, _) =>
        params += "error" -> "invalid_token"
        params += "error_description" -> error.description
      case AuthError.UpstreamFailure(_) => ()
    val rendered = params
      .map((k, v) => s"""$k="${v.replace("\\", "\\\\").replace("\"", "\\\"")}"""")
      .mkString(", ")
    s"Bearer $rendered"

  /** Status code for an [[AuthError]]. */
  def statusFor(error: AuthError): Status = error match
    case AuthError.InsufficientScope(_, _) => Status.Forbidden
    case AuthError.UpstreamFailure(_)      => Status.ServiceUnavailable
    case _                                  => Status.Unauthorized

  /** ErrorCode for the JSON-RPC body. */
  def errorCodeFor(error: AuthError): ErrorCode = error match
    case AuthError.InsufficientScope(_, _) => ErrorCode.Forbidden
    case _                                  => ErrorCode.Unauthorized

  /** Build the HTTP response for an authorization failure. */
  def errorResponse(
    auth: McpAuth[?],
    resourcePath: String,
    request: Request,
    error: AuthError,
    requiredScopes: Set[OauthScope],
  ): Response =
    val resourceUri = ResourceUriResolver.resolve(auth.resourceUri, resourcePath, request)
    val status      = statusFor(error)
    val code        = errorCodeFor(error)
    val body =
      JsonRpcError(None, ErrorDetail(code.code, error.description)).toJson
    Response.json(body)
      .status(status)
      .addHeader("WWW-Authenticate", wwwAuthenticate(auth, resourceUri, error, requiredScopes))
