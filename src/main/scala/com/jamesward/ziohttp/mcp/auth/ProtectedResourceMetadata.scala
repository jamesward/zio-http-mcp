package com.jamesward.ziohttp.mcp.auth

import zio.*
import zio.json.*

/**
 * RFC 9728 — OAuth 2.0 Protected Resource Metadata document.
 *
 * Served at `/.well-known/oauth-protected-resource` and (per MCP 2025-11-25) also
 * at `/.well-known/oauth-protected-resource/<server-path>`. The MCP spec adds an
 * `authorization_servers` field listing AS issuer URLs.
 *
 * @see [[https://datatracker.ietf.org/doc/html/rfc9728 RFC 9728]]
 */
final case class ProtectedResourceMetadata(
  resource: ResourceUri,
  authorization_servers: Chunk[AuthorizationServer],
  scopes_supported: Option[Chunk[OauthScope]] = None,
  bearer_methods_supported: Option[Chunk[String]] = None,
  resource_name: Option[String] = None,
  resource_documentation: Option[String] = None,
)

object ProtectedResourceMetadata:
  given CanEqual[ProtectedResourceMetadata, ProtectedResourceMetadata] = CanEqual.derived

  // Use explicit codec so optional fields are omitted (instead of serialized as `null`).
  given JsonCodec[ProtectedResourceMetadata] = DeriveJsonCodec.gen[ProtectedResourceMetadata]

  /** Build a PRM document from an [[McpAuth]] config and a resolved resource URI. */
  def fromAuth(auth: McpAuth[?], resourceUri: ResourceUri): ProtectedResourceMetadata =
    ProtectedResourceMetadata(
      resource = resourceUri,
      authorization_servers = Chunk.fromIterable(auth.authorizationServers),
      scopes_supported = if auth.scopesSupported.isEmpty then None else Some(auth.scopesSupported),
      bearer_methods_supported = Some(Chunk("header")),
      resource_name = auth.resourceName,
      resource_documentation = auth.resourceDocumentation,
    )
