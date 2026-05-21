package com.jamesward.ziohttp.mcp.auth

import zio.*
import zio.json.*

// --- OAuth scope (named OauthScope to avoid clashing with zio.Scope) ---

opaque type OauthScope = String

object OauthScope:
  def apply(s: String): OauthScope = s
  extension (s: OauthScope) def value: String = s
  given CanEqual[OauthScope, OauthScope] = CanEqual.derived
  given JsonEncoder[OauthScope] = JsonEncoder.string
  given JsonDecoder[OauthScope] = JsonDecoder.string

// --- Canonical resource URI per RFC 8707 §2 ---

/**
 * Canonical resource URI per [[https://www.rfc-editor.org/rfc/rfc8707.html#section-2 RFC 8707 §2]].
 *
 * Construction follows parse-don't-validate:
 *   - MUST have a scheme (e.g. `http` or `https`)
 *   - MUST NOT contain a fragment
 *   - SHOULD use lowercase scheme/host (we accept any case but compare case-insensitively in the host)
 *   - SHOULD NOT have a trailing slash unless semantically significant
 */
opaque type ResourceUri = String

object ResourceUri:
  /** Parse a string as a canonical resource URI. Returns Left with a reason if non-canonical. */
  def parse(s: String): Either[String, ResourceUri] =
    if s.contains('#') then
      Left(s"Resource URI must not contain a fragment: $s")
    else
      val schemeIdx = s.indexOf("://")
      if schemeIdx <= 0 then
        Left(s"Resource URI must include a scheme (e.g. https://): $s")
      else
        Right(s)

  /** Construct without validation. Use only for trusted/internal values. */
  def unsafe(s: String): ResourceUri = s

  extension (r: ResourceUri)
    def value: String = r

    /**
     * Compare for audience matching. Per the MCP spec, accept uppercase scheme/host
     * and tolerate trailing slash differences.
     */
    def matchesAudience(audience: String): Boolean =
      ResourceUri.canonicalize(r) == ResourceUri.canonicalize(audience)

  private def canonicalize(s: String): String =
    // Lowercase scheme + host; preserve path; strip trailing slash on bare-host URIs.
    val schemeIdx = s.indexOf("://")
    if schemeIdx < 0 then s.toLowerCase.stripSuffix("/")
    else
      val scheme = s.substring(0, schemeIdx).toLowerCase
      val rest = s.substring(schemeIdx + 3)
      val slashIdx = rest.indexOf('/')
      if slashIdx < 0 then s"$scheme://${rest.toLowerCase}".stripSuffix("/")
      else
        val host = rest.substring(0, slashIdx).toLowerCase
        val path = rest.substring(slashIdx)
        // Strip trailing slash on bare-host paths but keep meaningful trailing slashes.
        val normalizedPath = if path == "/" then "" else path
        s"$scheme://$host$normalizedPath"

  given CanEqual[ResourceUri, ResourceUri] = CanEqual.derived
  given JsonEncoder[ResourceUri] = JsonEncoder.string
  given JsonDecoder[ResourceUri] = JsonDecoder.string.mapOrFail(parse)

// --- Authorization server (the issuer URL) ---

opaque type AuthorizationServer = String

object AuthorizationServer:
  def apply(issuer: String): AuthorizationServer = issuer
  extension (a: AuthorizationServer) def issuer: String = a
  given CanEqual[AuthorizationServer, AuthorizationServer] = CanEqual.derived
  given JsonEncoder[AuthorizationServer] = JsonEncoder.string
  given JsonDecoder[AuthorizationServer] = JsonDecoder.string

// --- McpAuth: opt-in authorization configuration ---

/**
 * Configuration for opt-in OAuth 2.1 authorization on an MCP server.
 *
 * @param resourceUri          Canonical URI of this MCP server (RFC 8707). When `None`,
 *                             the URI is derived per-request from the `Forwarded` /
 *                             `X-Forwarded-*` / `Host` headers, with [[resourcePath]]
 *                             appended. Set explicitly in production for stability.
 * @param authorizationServers At least one authorization server (issuer URL).
 *                             Listed in PRM `authorization_servers`.
 * @param scopesSupported      Scopes advertised in PRM `scopes_supported`. May be empty.
 * @param resourceName         Human-readable resource name in PRM `resource_name`.
 * @param resourceDocumentation Documentation URL in PRM `resource_documentation`.
 * @param verifier             Token verifier — typically `TokenVerifier.discoverJwks(...)`
 *                             or `TokenVerifier.introspection(...)`.
 * @param requiredScopes       Server-wide scope requirements. Per-tool scopes are additive.
 * @param resourcePath         Path component used when [[resourceUri]] is derived from
 *                             headers. Defaults to `/mcp`, matching where the MCP routes
 *                             are mounted.
 * @param realm                `WWW-Authenticate` realm parameter.
 */
final case class McpAuth[-R](
  authorizationServers: NonEmptyChunk[AuthorizationServer],
  verifier: TokenVerifier[R],
  resourceUri: Option[ResourceUri] = None,
  scopesSupported: Chunk[OauthScope] = Chunk.empty,
  resourceName: Option[String] = None,
  resourceDocumentation: Option[String] = None,
  requiredScopes: Set[OauthScope] = Set.empty,
  resourcePath: String = "/mcp",
  realm: String = "mcp",
):
  def withResourceUri(uri: ResourceUri): McpAuth[R] =
    copy(resourceUri = Some(uri))

  def withRequiredScopes(scopes: OauthScope*): McpAuth[R] =
    copy(requiredScopes = scopes.toSet)

  def withRealm(r: String): McpAuth[R] = copy(realm = r)

object McpAuth:
  /** Convenience constructor for the common case of a single authorization server. */
  def apply[R](
    authorizationServer: AuthorizationServer,
    verifier: TokenVerifier[R],
  ): McpAuth[R] =
    McpAuth(
      authorizationServers = NonEmptyChunk(authorizationServer),
      verifier = verifier,
    )
