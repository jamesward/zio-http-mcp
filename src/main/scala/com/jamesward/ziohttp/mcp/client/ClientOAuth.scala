package com.jamesward.ziohttp.mcp.client

import zio.*
import zio.http.*
import zio.json.*
import zio.json.ast.Json

import java.nio.charset.StandardCharsets
import java.time.Instant

/**
 * OAuth 2.1 `client_credentials` configuration for the MCP client.
 *
 * This is the machine-to-machine flow: the client authenticates as itself with a
 * registered `clientId` / `clientSecret`, with no human in the loop. It is the only
 * grant the client supports for now (the spec's `authorization_code` + PKCE flow is
 * future work).
 *
 * Discovery is automatic and follows the MCP authorization spec:
 *   1. The client probes the MCP endpoint; on `401` it reads the `resource_metadata`
 *      URL from the `WWW-Authenticate` challenge (falling back to the well-known
 *      path under the server origin).
 *   2. It fetches the RFC 9728 Protected Resource Metadata to learn the canonical
 *      `resource` (audience) and the `authorization_servers`.
 *   3. It fetches the RFC 8414 Authorization Server Metadata (OIDC discovery
 *      fallback) to learn the `token_endpoint`.
 *   4. It requests a token with `grant_type=client_credentials`, binding it to the
 *      resource via the RFC 8707 `resource` parameter.
 *
 * Supply [[tokenEndpoint]] and/or [[resource]] to pin those values and skip the
 * corresponding discovery step.
 *
 * @param clientId      OAuth client identifier.
 * @param clientSecret  OAuth client secret (held as a [[zio.Config.Secret]]).
 * @param scopes        Scopes to request. When empty, no `scope` parameter is sent
 *                      and the authorization server applies the client's defaults.
 * @param tokenEndpoint Pin the token endpoint, skipping metadata discovery.
 * @param resource      Pin the RFC 8707 audience, skipping PRM discovery.
 */
final case class OAuthClientCredentials(
  clientId: String,
  clientSecret: Config.Secret,
  scopes: Set[String] = Set.empty,
  tokenEndpoint: Option[String] = None,
  resource: Option[String] = None,
)

object OAuthClientCredentials:
  given CanEqual[OAuthClientCredentials, OAuthClientCredentials] = CanEqual.derived

/** A token plus the instant after which it should be considered expired. */
private[client] final case class CachedToken(value: String, expiresAt: Option[Instant]):
  /** Valid if there's no expiry, or expiry is more than `skew` in the future. */
  def isValid(now: Instant, skew: Duration = 30.seconds): Boolean =
    expiresAt.forall(_.isAfter(now.plusSeconds(skew.toSeconds)))

/** The endpoints resolved by discovery, cached after the first lookup. */
private[client] final case class ResolvedOAuth(tokenEndpoint: URL, resource: String)

/**
 * Stateless helpers implementing the `client_credentials` discovery + token flow.
 * The live client caches the [[ResolvedOAuth]] and [[CachedToken]] in a `Ref`.
 */
private[client] object ClientOAuth:

  /** Resolve the token endpoint + audience for the given server URL. */
  def resolve(
    client: Client,
    serverUrl: String,
    oauth: OAuthClientCredentials,
  ): IO[McpClientError, ResolvedOAuth] =
    (oauth.tokenEndpoint, oauth.resource) match
      case (Some(te), Some(res)) =>
        decodeUrl(te).map(ResolvedOAuth(_, res))
      case (explicitTe, explicitRes) =>
        for
          prmUrl   <- discoverPrmUrl(client, serverUrl)
          prm      <- fetchJsonObj(client, prmUrl, "protected resource metadata")
          resource <- explicitRes match
                        case Some(r) => ZIO.succeed(r)
                        case None    =>
                          ZIO.fromOption(prm.get("resource").flatMap(_.asString))
                            .orElseFail(McpClientError.Auth(s"PRM at $prmUrl missing 'resource'"))
          tokenUrl <- explicitTe match
                        case Some(te) => decodeUrl(te)
                        case None     =>
                          for
                            issuer <- ZIO.fromOption(
                                        prm.get("authorization_servers")
                                          .flatMap(_.asArray)
                                          .flatMap(_.headOption)
                                          .flatMap(_.asString)
                                      ).orElseFail(McpClientError.Auth(s"PRM at $prmUrl missing 'authorization_servers'"))
                            te     <- discoverTokenEndpoint(client, issuer)
                          yield te
        yield ResolvedOAuth(tokenUrl, resource)

  /** Fetch a fresh access token via `client_credentials`. */
  def fetchToken(
    client: Client,
    resolved: ResolvedOAuth,
    oauth: OAuthClientCredentials,
  ): IO[McpClientError, CachedToken] =
    val secret = String(oauth.clientSecret.value.toArray)
    val scopeParam =
      if oauth.scopes.isEmpty then ""
      else s"&scope=${urlEncode(oauth.scopes.mkString(" "))}"
    val form =
      s"grant_type=client_credentials&resource=${urlEncode(resolved.resource)}$scopeParam"
    val request = Request.post(resolved.tokenEndpoint, Body.fromString(form))
      .addHeader(Header.ContentType(MediaType.application.`x-www-form-urlencoded`))
      .addHeader(Header.Authorization.Basic(oauth.clientId, secret))
      .addHeader("accept", "application/json")
    for
      now  <- Clock.instant
      resp <- client.batched(request)
                .mapError(t => McpClientError.Auth(s"Token request failed: ${t.getMessage}"))
      body <- resp.body.asString
                .mapError(t => McpClientError.Auth(s"Failed to read token response: ${t.getMessage}"))
      _    <- ZIO.fail(McpClientError.Auth(s"Token endpoint returned ${resp.status.code}: $body"))
                .when(!resp.status.isSuccess)
      json <- ZIO.fromEither(body.fromJson[Json.Obj])
                .mapError(e => McpClientError.Auth(s"Invalid token JSON: $e"))
      tok  <- ZIO.fromOption(json.get("access_token").flatMap(_.asString))
                .orElseFail(McpClientError.Auth("Token response missing 'access_token'"))
    yield
      val expiresAt = json.get("expires_in")
        .flatMap(_.asNumber)
        .map(n => now.plusSeconds(n.value.longValue))
      CachedToken(tok, expiresAt)

  // --- discovery internals ---

  /**
   * Determine the Protected Resource Metadata URL. Probe the MCP endpoint
   * unauthenticated; on a 401 read the `resource_metadata` parameter from the
   * `WWW-Authenticate` challenge. Otherwise fall back to the well-known path under
   * the server origin (RFC 9728 §3.1).
   */
  private def discoverPrmUrl(client: Client, serverUrl: String): IO[McpClientError, URL] =
    val probeBody =
      """{"jsonrpc":"2.0","id":"probe","method":"initialize",""" +
        """"params":{"protocolVersion":"2025-11-25","capabilities":{},""" +
        """"clientInfo":{"name":"zio-http-mcp-client","version":"0.1.0"}}}"""
    for
      url  <- decodeUrl(serverUrl)
      req   = Request.post(url, Body.fromString(probeBody))
                .addHeader(Header.ContentType(MediaType.application.json))
                .addHeader("accept", "application/json, text/event-stream")
      resp <- client.batched(req)
                .mapError(t => McpClientError.Auth(s"Auth discovery probe failed: ${t.getMessage}"))
      prmUrl <- resp.rawHeader("www-authenticate").flatMap(parseResourceMetadata) match
                  case Some(metaUrl) => decodeUrl(metaUrl)
                  case None          => decodeUrl(defaultPrmUrl(url))
    yield prmUrl

  /** Extract `resource_metadata=<url>` (quoted or bare) from a WWW-Authenticate value. */
  private[client] def parseResourceMetadata(header: String): Option[String] =
    val marker = "resource_metadata="
    val idx = header.indexOf(marker)
    if idx < 0 then None
    else
      val rest = header.substring(idx + marker.length).trim
      val unquoted =
        if rest.startsWith("\"") then rest.drop(1).takeWhile(_ != '"')
        else rest.takeWhile(c => c != ',' && c != ' ')
      Some(unquoted).filter(_.nonEmpty)

  /** `<scheme>://<host[:port]>/.well-known/oauth-protected-resource` for a server URL. */
  private[client] def defaultPrmUrl(serverUrl: URL): String =
    val scheme = serverUrl.scheme.map(_.encode).getOrElse("https")
    val host = serverUrl.host.getOrElse("localhost")
    val portPart = serverUrl.port.map(p => s":$p").getOrElse("")
    s"$scheme://$host$portPart/.well-known/oauth-protected-resource"

  /** Fetch RFC 8414 AS metadata (OIDC fallback) and extract `token_endpoint`. */
  private def discoverTokenEndpoint(client: Client, issuer: String): IO[McpClientError, URL] =
    val base = issuer.stripSuffix("/")
    val oauthMeta = s"$base/.well-known/oauth-authorization-server"
    val oidcMeta  = s"$base/.well-known/openid-configuration"
    val fromOauth = fetchTokenEndpointFrom(client, oauthMeta)
    fromOauth.orElse(fetchTokenEndpointFrom(client, oidcMeta))

  private def fetchTokenEndpointFrom(client: Client, metadataUrl: String): IO[McpClientError, URL] =
    for
      url  <- decodeUrl(metadataUrl)
      json <- fetchJsonObj(client, url, "authorization server metadata")
      teS  <- ZIO.fromOption(json.get("token_endpoint").flatMap(_.asString))
                .orElseFail(McpClientError.Auth(s"AS metadata at $metadataUrl missing 'token_endpoint'"))
      te   <- decodeUrl(teS)
    yield te

  private def fetchJsonObj(client: Client, url: URL, what: String): IO[McpClientError, Json.Obj] =
    for
      resp <- client.batched(Request.get(url))
                .mapError(t => McpClientError.Auth(s"Failed to fetch $what: ${t.getMessage}"))
      body <- resp.body.asString
                .mapError(t => McpClientError.Auth(s"Failed to read $what: ${t.getMessage}"))
      _    <- ZIO.fail(McpClientError.Auth(s"$what returned ${resp.status.code}: $body"))
                .when(!resp.status.isSuccess)
      json <- ZIO.fromEither(body.fromJson[Json.Obj])
                .mapError(e => McpClientError.Auth(s"Invalid $what JSON: $e"))
    yield json

  private def decodeUrl(s: String): IO[McpClientError, URL] =
    ZIO.fromEither(URL.decode(s))
      .mapError(e => McpClientError.Auth(s"Invalid URL '$s': ${e.getMessage}"))

  private def urlEncode(s: String): String =
    java.net.URLEncoder.encode(s, StandardCharsets.UTF_8)
