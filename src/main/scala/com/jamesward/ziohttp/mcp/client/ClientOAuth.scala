package com.jamesward.ziohttp.mcp.client

import zio.*
import zio.http.*
import zio.json.*
import zio.json.ast.Json

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.security.SecureRandom
import java.time.Instant
import java.util.Base64

/**
 * Client-side OAuth configuration for the MCP client, covering the two supported
 * grants:
 *
 *   - [[OAuthClientCredentials]] — machine-to-machine: the client authenticates as
 *     itself with a registered `clientId` / `clientSecret`, no human in the loop.
 *   - [[OAuthAuthorizationCode]] — the MCP-spec authorization flow (2026-07-28
 *     "hardened" profile): authorization code + PKCE (S256), RFC 8707 `resource`
 *     binding, RFC 9207 `iss` validation, and client identification via
 *     pre-registration, Client ID Metadata Documents (CIMD), or Dynamic Client
 *     Registration (RFC 7591, deprecated fallback).
 */
sealed trait McpClientOAuth

object McpClientOAuth:
  given CanEqual[McpClientOAuth, McpClientOAuth] = CanEqual.derived

/**
 * OAuth 2.1 `client_credentials` configuration for the MCP client.
 *
 * This is the machine-to-machine flow: the client authenticates as itself with a
 * registered `clientId` / `clientSecret`, with no human in the loop.
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
) extends McpClientOAuth

object OAuthClientCredentials:
  given CanEqual[OAuthClientCredentials, OAuthClientCredentials] = CanEqual.derived

/**
 * OAuth 2.1 authorization-code + PKCE configuration for the MCP client, per the
 * MCP authorization spec (2026-07-28).
 *
 * The client runs the full hardened flow:
 *
 *   1. Probe the MCP endpoint → `401` with `WWW-Authenticate` naming the RFC 9728
 *      Protected Resource Metadata URL (and optionally a `scope` hint).
 *   2. Fetch the PRM; verify its `resource` matches the MCP server URL; pick the
 *      first advertised authorization server.
 *   3. Discover authorization-server metadata (RFC 8414 and OIDC Discovery forms,
 *      path-aware well-known priority order); verify the metadata `issuer`.
 *   4. Obtain a client id, in the spec's priority order:
 *      pre-registered [[clientId]] → CIMD [[clientMetadataUrl]] (when the AS
 *      advertises `client_id_metadata_document_supported`) → Dynamic Client
 *      Registration (deprecated fallback, when the AS has a
 *      `registration_endpoint`).
 *   5. Send the authorization request with PKCE (S256), `resource` (RFC 8707),
 *      `state`, and the selected scopes; drive the user agent via [[authorization]].
 *   6. Validate the authorization response: `state` round-trip and the RFC 9207
 *      `iss` parameter (exact string comparison against the recorded metadata
 *      issuer; required when the AS advertises
 *      `authorization_response_iss_parameter_supported`).
 *   7. Exchange the code (+ `code_verifier` + `resource`) for tokens; refresh with
 *      `grant_type=refresh_token` when the token expires.
 *
 * Scope selection follows the spec's strategy: explicit [[scopes]] win; otherwise
 * the `scope` hint from the `WWW-Authenticate` challenge; otherwise the PRM's
 * `scopes_supported`; otherwise no `scope` parameter is sent.
 *
 * @param clientId          Pre-registered OAuth client id (spec priority 1). When
 *                          set, CIMD and DCR are skipped.
 * @param clientSecret      Secret for a confidential pre-registered client; sent
 *                          via HTTP Basic on the token endpoint.
 * @param clientMetadataUrl HTTPS URL of this client's Client ID Metadata Document
 *                          (used as the `client_id` when the AS supports CIMD).
 * @param redirectUri       Redirect URI for the authorization response. Must be
 *                          listed in the CIMD document / registration.
 * @param scopes            Explicit scopes to request; overrides the discovery-based
 *                          scope selection strategy when non-empty.
 * @param clientName        `client_name` reported during Dynamic Client Registration.
 * @param authorization     How the authorization URL reaches the resource owner's
 *                          user agent and how the redirect comes back. Defaults to
 *                          [[AuthorizationHandler.autoRedirect]], which performs the
 *                          request non-interactively (suitable for auto-approving
 *                          authorization servers and tests). Real interactive apps
 *                          supply a handler that opens a browser and listens on
 *                          [[redirectUri]].
 */
final case class OAuthAuthorizationCode(
  clientId: Option[String] = None,
  clientSecret: Option[Config.Secret] = None,
  clientMetadataUrl: Option[String] = None,
  redirectUri: String = "http://127.0.0.1:3000/callback",
  scopes: Set[String] = Set.empty,
  clientName: String = "zio-http-mcp-client",
  authorization: AuthorizationHandler = AuthorizationHandler.autoRedirect,
) extends McpClientOAuth

object OAuthAuthorizationCode:
  given CanEqual[OAuthAuthorizationCode, OAuthAuthorizationCode] = CanEqual.derived

/**
 * Drives the resource owner's user agent through the authorization endpoint.
 *
 * Given the fully built authorization URL (with `client_id`, `code_challenge`,
 * `resource`, `state`, …), the handler must return the query parameters of the
 * authorization response — the query string the authorization server appended to
 * the redirect URI (`code`, `state`, `iss`, or `error` family).
 */
trait AuthorizationHandler:
  def authorize(client: Client, authorizationUrl: URL): IO[McpClientError, Map[String, String]]

object AuthorizationHandler:

  /**
   * Non-interactive handler: `GET` the authorization URL without following
   * redirects and read the authorization response from the `Location` header.
   *
   * This works against authorization servers that auto-approve (test IDPs, the MCP
   * conformance kit's mock AS, or an AS with an established session). Interactive
   * applications should supply their own handler that opens the system browser and
   * captures the redirect on a loopback listener.
   */
  val autoRedirect: AuthorizationHandler = new AuthorizationHandler:
    def authorize(client: Client, authorizationUrl: URL): IO[McpClientError, Map[String, String]] =
      for
        resp <- client.batched(Request.get(authorizationUrl))
                  .mapError(t => McpClientError.Auth(s"Authorization request failed: ${t.getMessage}"))
        loc  <- resp.rawHeader("location") match
                  case Some(l) if resp.status.isRedirection => ZIO.succeed(l)
                  case _ =>
                    resp.body.asString.orElseSucceed("").flatMap: b =>
                      ZIO.fail(McpClientError.Auth(
                        s"Authorization endpoint returned ${resp.status.code} without a redirect: $b"))
        url  <- ZIO.fromEither(URL.decode(loc))
                  .mapError(e => McpClientError.Auth(s"Invalid redirect Location '$loc': ${e.getMessage}"))
      yield url.queryParams.map.map((k, vs) => k -> vs.headOption.getOrElse("")).toMap

/** A token plus the instant after which it should be considered expired. */
private[client] final case class CachedToken(
  value: String,
  expiresAt: Option[Instant],
  refreshToken: Option[String] = None,
):
  /** Valid if there's no expiry, or expiry is more than `skew` in the future. */
  def isValid(now: Instant, skew: Duration = 30.seconds): Boolean =
    expiresAt.forall(_.isAfter(now.plusSeconds(skew.toSeconds)))

/**
 * Everything resolved by discovery (and, for the authorization-code flow, client
 * registration), cached after the first lookup.
 */
private[client] final case class ResolvedOAuth(
  tokenEndpoint: URL,
  resource: String,
  authorizationEndpoint: Option[URL] = None,
  issuer: Option[String] = None,
  issAdvertised: Boolean = false,
  clientId: Option[String] = None,
  clientSecret: Option[Config.Secret] = None,
  scopes: Set[String] = Set.empty,
)

/**
 * Stateless helpers implementing client-side OAuth discovery + token flows.
 * The live client caches the [[ResolvedOAuth]] and [[CachedToken]] in a `Ref`.
 */
private[client] object ClientOAuth:

  /** Resolve endpoints, audience, and (for authorization-code) the client identity. */
  def resolve(
    client: Client,
    serverUrl: String,
    oauth: McpClientOAuth,
  ): IO[McpClientError, ResolvedOAuth] = oauth match
    case cc: OAuthClientCredentials  => resolveClientCredentials(client, serverUrl, cc)
    case ac: OAuthAuthorizationCode  => resolveAuthorizationCode(client, serverUrl, ac)

  /** Obtain a fresh access token, using the refresh token from `previous` if present. */
  def fetchToken(
    client: Client,
    resolved: ResolvedOAuth,
    oauth: McpClientOAuth,
    previous: Option[CachedToken],
  ): IO[McpClientError, CachedToken] = oauth match
    case cc: OAuthClientCredentials => fetchClientCredentialsToken(client, resolved, cc)
    case ac: OAuthAuthorizationCode =>
      previous.flatMap(_.refreshToken) match
        case Some(rt) =>
          refreshToken(client, resolved, rt)
            .orElse(authorizationCodeDance(client, resolved, ac))
        case None => authorizationCodeDance(client, resolved, ac)

  // --- client_credentials ---

  private def resolveClientCredentials(
    client: Client,
    serverUrl: String,
    oauth: OAuthClientCredentials,
  ): IO[McpClientError, ResolvedOAuth] =
    (oauth.tokenEndpoint, oauth.resource) match
      case (Some(te), Some(res)) =>
        decodeUrl(te).map(ResolvedOAuth(_, res))
      case (explicitTe, explicitRes) =>
        for
          probe         <- discoverPrm(client, serverUrl)
          (prmUrl, prm) <- fetchPrm(client, probe)
          resource      <- explicitRes match
                             case Some(r) => ZIO.succeed(r)
                             case None    => prmResource(prm, prmUrl, serverUrl)
          tokenUrl      <- explicitTe match
                             case Some(te) => decodeUrl(te)
                             case None     =>
                               for
                                 issuer <- firstAuthorizationServer(prm, prmUrl)
                                 meta   <- discoverAsMetadata(client, issuer)
                                 te     <- decodeUrl(meta.tokenEndpoint)
                               yield te
        yield ResolvedOAuth(tokenUrl, resource)

  private def fetchClientCredentialsToken(
    client: Client,
    resolved: ResolvedOAuth,
    oauth: OAuthClientCredentials,
  ): IO[McpClientError, CachedToken] =
    val scopeParam =
      if oauth.scopes.isEmpty then ""
      else s"&scope=${urlEncode(oauth.scopes.mkString(" "))}"
    val form =
      s"grant_type=client_credentials&resource=${urlEncode(resolved.resource)}$scopeParam"
    val secret = String(oauth.clientSecret.value.toArray)
    val request = Request.post(resolved.tokenEndpoint, Body.fromString(form))
      .addHeader(Header.ContentType(MediaType.application.`x-www-form-urlencoded`))
      .addHeader(Header.Authorization.Basic(oauth.clientId, secret))
      .addHeader("accept", "application/json")
    requestToken(client, request)

  // --- authorization_code + PKCE ---

  private def resolveAuthorizationCode(
    client: Client,
    serverUrl: String,
    oauth: OAuthAuthorizationCode,
  ): IO[McpClientError, ResolvedOAuth] =
    for
      probe         <- discoverPrm(client, serverUrl)
      (prmUrl, prm) <- fetchPrm(client, probe)
      resource      <- prmResource(prm, prmUrl, serverUrl)
      issuer        <- firstAuthorizationServer(prm, prmUrl)
      meta     <- discoverAsMetadata(client, issuer)
      authEp   <- ZIO.fromOption(meta.authorizationEndpoint)
                    .orElseFail(McpClientError.Auth(s"AS metadata for $issuer missing 'authorization_endpoint'"))
                    .flatMap(decodeUrl)
      tokenEp  <- decodeUrl(meta.tokenEndpoint)
      scopes    = selectScopes(oauth.scopes, probe.scopeHint, prm)
      identity <- resolveClientIdentity(client, oauth, meta, scopes)
    yield ResolvedOAuth(
      tokenEndpoint = tokenEp,
      resource = resource,
      authorizationEndpoint = Some(authEp),
      issuer = Some(meta.issuer),
      issAdvertised = meta.issParameterSupported,
      clientId = Some(identity._1),
      clientSecret = identity._2,
      scopes = scopes,
    )

  /**
   * Client identification, in the spec's priority order: pre-registered client id →
   * Client ID Metadata Document (when the AS advertises support) → Dynamic Client
   * Registration (deprecated fallback).
   */
  private def resolveClientIdentity(
    client: Client,
    oauth: OAuthAuthorizationCode,
    meta: AsMetadata,
    scopes: Set[String],
  ): IO[McpClientError, (String, Option[Config.Secret])] =
    oauth.clientId match
      case Some(id) => ZIO.succeed((id, oauth.clientSecret))
      case None =>
        oauth.clientMetadataUrl match
          case Some(url) if meta.clientIdMetadataDocumentSupported =>
            ZIO.succeed((url, None))
          case _ =>
            meta.registrationEndpoint match
              case Some(regEp) => dynamicallyRegister(client, regEp, oauth, scopes)
              case None =>
                ZIO.fail(McpClientError.Auth(
                  "No way to identify the client: no pre-registered clientId, the authorization " +
                    "server does not advertise client_id_metadata_document_supported" +
                    (if oauth.clientMetadataUrl.isEmpty then " (and no clientMetadataUrl is configured)" else "") +
                    ", and it has no registration_endpoint for Dynamic Client Registration"))

  /** RFC 7591 Dynamic Client Registration (deprecated fallback path). */
  private def dynamicallyRegister(
    client: Client,
    registrationEndpoint: String,
    oauth: OAuthAuthorizationCode,
    scopes: Set[String],
  ): IO[McpClientError, (String, Option[Config.Secret])] =
    val isLoopback =
      oauth.redirectUri.startsWith("http://127.0.0.1") ||
        oauth.redirectUri.startsWith("http://localhost") ||
        oauth.redirectUri.startsWith("http://[::1]")
    val body = Json.Obj(
      Chunk(
        Some("client_name" -> Json.Str(oauth.clientName)),
        Some("redirect_uris" -> Json.Arr(Chunk(Json.Str(oauth.redirectUri)))),
        Some("grant_types" -> Json.Arr(Chunk(Json.Str("authorization_code"), Json.Str("refresh_token")))),
        Some("response_types" -> Json.Arr(Chunk(Json.Str("code")))),
        Some("token_endpoint_auth_method" -> Json.Str("none")),
        // SEP-837: OIDC-backed ASes constrain redirect URIs by application_type.
        Some("application_type" -> Json.Str(if isLoopback then "native" else "web")),
        Option.when(scopes.nonEmpty)("scope" -> Json.Str(scopes.mkString(" "))),
      ).flatten
    )
    for
      url  <- decodeUrl(registrationEndpoint)
      resp <- client.batched(
                Request.post(url, Body.fromString(body.toJson))
                  .addHeader(Header.ContentType(MediaType.application.json))
                  .addHeader("accept", "application/json"))
                .mapError(t => McpClientError.Auth(s"Dynamic client registration failed: ${t.getMessage}"))
      text <- resp.body.asString
                .mapError(t => McpClientError.Auth(s"Failed to read registration response: ${t.getMessage}"))
      _    <- ZIO.fail(McpClientError.Auth(s"Registration endpoint returned ${resp.status.code}: $text"))
                .when(!resp.status.isSuccess)
      json <- ZIO.fromEither(text.fromJson[Json.Obj])
                .mapError(e => McpClientError.Auth(s"Invalid registration JSON: $e"))
      id   <- ZIO.fromOption(json.get("client_id").flatMap(_.asString))
                .orElseFail(McpClientError.Auth("Registration response missing 'client_id'"))
    yield (id, json.get("client_secret").flatMap(_.asString).map(Config.Secret(_)))

  /** The authorization-code round trip: authorize (PKCE, resource, state) → validate → exchange. */
  private def authorizationCodeDance(
    client: Client,
    resolved: ResolvedOAuth,
    oauth: OAuthAuthorizationCode,
  ): IO[McpClientError, CachedToken] =
    for
      authEp   <- ZIO.fromOption(resolved.authorizationEndpoint)
                    .orElseFail(McpClientError.Auth("No authorization endpoint resolved"))
      clientId <- ZIO.fromOption(resolved.clientId)
                    .orElseFail(McpClientError.Auth("No client id resolved"))
      verifier <- ZIO.succeed(randomUrlSafe(32))
      state    <- ZIO.succeed(randomUrlSafe(16))
      challenge = s256(verifier)
      query     = Chunk(
                    "response_type"         -> "code",
                    "client_id"             -> clientId,
                    "redirect_uri"          -> oauth.redirectUri,
                    "code_challenge"        -> challenge,
                    "code_challenge_method" -> "S256",
                    "state"                 -> state,
                    "resource"              -> resolved.resource,
                  ) ++ (if resolved.scopes.nonEmpty then Chunk("scope" -> resolved.scopes.mkString(" ")) else Chunk.empty)
      authUrl   = query.foldLeft(authEp)((u, kv) => u.addQueryParam(kv._1, kv._2))
      params   <- oauth.authorization.authorize(client, authUrl)
      code     <- validateAuthorizationResponse(params, resolved, state)
      token    <- exchangeCode(client, resolved, oauth, code, verifier, clientId)
    yield token

  /**
   * Validate the authorization response per the MCP 2026-07-28 spec:
   *
   *   - RFC 9207 `iss` check first — exact string comparison against the recorded
   *     metadata issuer, no normalization. When the AS advertises
   *     `authorization_response_iss_parameter_supported`, a missing `iss` is a
   *     rejection; a present `iss` is always compared. On `iss` failure the client
   *     must not act on any `error` parameters in the response.
   *   - `state` must round-trip unchanged.
   *   - then `error` / `code` handling.
   */
  private def validateAuthorizationResponse(
    params: Map[String, String],
    resolved: ResolvedOAuth,
    expectedState: String,
  ): IO[McpClientError, String] =
    val issCheck: IO[McpClientError, Unit] =
      (params.get("iss"), resolved.issuer) match
        case (Some(iss), Some(expected)) =>
          ZIO.fail(McpClientError.Auth(
            s"Authorization response 'iss' mismatch: expected '$expected', got '$iss' (RFC 9207)"))
            .when(iss != expected).unit
        case (None, Some(expected)) if resolved.issAdvertised =>
          ZIO.fail(McpClientError.Auth(
            s"Authorization server advertises authorization_response_iss_parameter_supported but the " +
              s"authorization response has no 'iss' parameter (expected '$expected', RFC 9207)"))
        case _ => ZIO.unit
    for
      _    <- issCheck
      _    <- ZIO.fail(McpClientError.Auth(
                s"Authorization response 'state' mismatch: possible CSRF"))
                .when(!params.get("state").contains(expectedState))
      _    <- params.get("error") match
                case Some(err) =>
                  val desc = params.get("error_description").fold("")(d => s": $d")
                  ZIO.fail(McpClientError.Auth(s"Authorization request failed with '$err'$desc"))
                case None => ZIO.unit
      code <- ZIO.fromOption(params.get("code"))
                .orElseFail(McpClientError.Auth("Authorization response has no 'code'"))
    yield code

  private def exchangeCode(
    client: Client,
    resolved: ResolvedOAuth,
    oauth: OAuthAuthorizationCode,
    code: String,
    verifier: String,
    clientId: String,
  ): IO[McpClientError, CachedToken] =
    val base =
      s"grant_type=authorization_code&code=${urlEncode(code)}" +
        s"&redirect_uri=${urlEncode(oauth.redirectUri)}" +
        s"&code_verifier=${urlEncode(verifier)}" +
        s"&resource=${urlEncode(resolved.resource)}"
    requestToken(client, tokenRequest(resolved, base, clientId))

  private def refreshToken(
    client: Client,
    resolved: ResolvedOAuth,
    refresh: String,
  ): IO[McpClientError, CachedToken] =
    val base =
      s"grant_type=refresh_token&refresh_token=${urlEncode(refresh)}" +
        s"&resource=${urlEncode(resolved.resource)}"
    val clientId = resolved.clientId.getOrElse("")
    requestToken(client, tokenRequest(resolved, base, clientId))
      // A refresh token is single-use on many ASes; keep the old one if no new one came back.
      .map(t => t.copy(refreshToken = t.refreshToken.orElse(Some(refresh))))

  /** Build a token-endpoint request with the right client authentication. */
  private def tokenRequest(resolved: ResolvedOAuth, form: String, clientId: String): Request =
    val (body, auth) = resolved.clientSecret match
      case Some(secret) => (form, Some(Header.Authorization.Basic(clientId, String(secret.value.toArray))))
      case None         => (s"$form&client_id=${urlEncode(clientId)}", None)
    val base = Request.post(resolved.tokenEndpoint, Body.fromString(body))
      .addHeader(Header.ContentType(MediaType.application.`x-www-form-urlencoded`))
      .addHeader("accept", "application/json")
    auth.fold(base)(base.addHeader)

  /** POST a token request and parse the token response. */
  private def requestToken(client: Client, request: Request): IO[McpClientError, CachedToken] =
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
      CachedToken(tok, expiresAt, json.get("refresh_token").flatMap(_.asString))

  // --- discovery internals ---

  private[client] final case class PrmProbe(prmUrls: List[URL], scopeHint: Option[String])

  /** Authorization-server metadata (RFC 8414 / OIDC Discovery), the fields we use. */
  private[client] final case class AsMetadata(
    issuer: String,
    tokenEndpoint: String,
    authorizationEndpoint: Option[String],
    registrationEndpoint: Option[String],
    clientIdMetadataDocumentSupported: Boolean,
    issParameterSupported: Boolean,
    scopesSupported: Chunk[String],
  )

  /**
   * Determine the Protected Resource Metadata URL (and the challenge's `scope`
   * hint). Probe the MCP endpoint unauthenticated; on a 401 read the
   * `resource_metadata` parameter from the `WWW-Authenticate` challenge. Otherwise
   * fall back to the well-known path under the server origin (RFC 9728 §3.1).
   */
  private def discoverPrm(client: Client, serverUrl: String): IO[McpClientError, PrmProbe] =
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
      www     = resp.rawHeader("www-authenticate")
      prmUrls <- www.flatMap(parseChallengeParam(_, "resource_metadata")) match
                   case Some(metaUrl) => decodeUrl(metaUrl).map(List(_))
                   case None          => ZIO.foreach(defaultPrmUrls(url))(decodeUrl)
    yield PrmProbe(prmUrls, www.flatMap(parseChallengeParam(_, "scope")))

  /** Fetch the PRM document, trying each candidate well-known URL in priority order. */
  private def fetchPrm(client: Client, probe: PrmProbe): IO[McpClientError, (URL, Json.Obj)] =
    def loop(urls: List[URL], lastError: Option[McpClientError]): IO[McpClientError, (URL, Json.Obj)] =
      urls match
        case Nil =>
          ZIO.fail(lastError.getOrElse(McpClientError.Auth("No protected resource metadata URL to try")))
        case u :: rest =>
          fetchJsonObj(client, u, "protected resource metadata")
            .map(u -> _)
            .catchAll(e => loop(rest, Some(e)))
    loop(probe.prmUrls, None)

  /** Extract `<name>=<value>` (quoted or bare) from a WWW-Authenticate value. */
  private[client] def parseChallengeParam(header: String, name: String): Option[String] =
    val marker = s"$name="
    val idx = header.indexOf(marker)
    if idx < 0 then None
    else
      val rest = header.substring(idx + marker.length).trim
      val unquoted =
        if rest.startsWith("\"") then rest.drop(1).takeWhile(_ != '"')
        else rest.takeWhile(c => c != ',' && c != ' ')
      Some(unquoted).filter(_.nonEmpty)

  private[client] def parseResourceMetadata(header: String): Option[String] =
    parseChallengeParam(header, "resource_metadata")

  /**
   * Candidate well-known PRM URLs for a server URL, in RFC 9728 §3.1 priority order:
   * the path-inserted form first (`/.well-known/oauth-protected-resource/<path>`,
   * for a resource with a path component), then the root form.
   */
  private[client] def defaultPrmUrls(serverUrl: URL): List[String] =
    val scheme = serverUrl.scheme.map(_.encode).getOrElse("https")
    val host = serverUrl.host.getOrElse("localhost")
    val portPart = serverUrl.port.map(p => s":$p").getOrElse("")
    val origin = s"$scheme://$host$portPart"
    val root = s"$origin/.well-known/oauth-protected-resource"
    val path = serverUrl.path.encode.stripSuffix("/")
    if path.isEmpty || path == "/" then List(root)
    else List(s"$root$path", root)

  private[client] def defaultPrmUrl(serverUrl: URL): String =
    defaultPrmUrls(serverUrl).head

  /**
   * Read the PRM `resource` and verify it identifies the MCP server being accessed
   * (RFC 9728 §3.3): same scheme/host/port (case-insensitive scheme and host) and
   * the resource path must be a prefix of the server URL's path. A PRM naming a
   * different resource is a misconfiguration or an attack, and the client must not
   * request tokens for it.
   */
  private def prmResource(prm: Json.Obj, prmUrl: URL, serverUrl: String): IO[McpClientError, String] =
    for
      resource <- ZIO.fromOption(prm.get("resource").flatMap(_.asString))
                    .orElseFail(McpClientError.Auth(s"PRM at $prmUrl missing 'resource'"))
      _        <- ZIO.fail(McpClientError.Auth(
                    s"PRM resource '$resource' does not match the MCP server URL '$serverUrl' — refusing " +
                      "to request a token for a different resource"))
                    .when(!resourceMatchesServer(resource, serverUrl))
    yield resource

  private[client] def resourceMatchesServer(resource: String, serverUrl: String): Boolean =
    (URL.decode(resource).toOption, URL.decode(serverUrl).toOption) match
      case (Some(r), Some(s)) =>
        val sameOrigin =
          r.scheme.map(_.encode.toLowerCase) == s.scheme.map(_.encode.toLowerCase) &&
            r.host.map(_.toLowerCase) == s.host.map(_.toLowerCase) &&
            effectivePort(r) == effectivePort(s)
        val rPath = r.path.encode.stripSuffix("/")
        val sPath = s.path.encode.stripSuffix("/")
        sameOrigin && (sPath == rPath || sPath.startsWith(if rPath.isEmpty then "/" else s"$rPath/") || rPath.isEmpty)
      case _ => false

  private def effectivePort(u: URL): Option[Int] =
    u.port.orElse(u.scheme.map(_.encode.toLowerCase).collect {
      case "https" => 443
      case "http"  => 80
    })

  private def firstAuthorizationServer(prm: Json.Obj, prmUrl: URL): IO[McpClientError, String] =
    ZIO.fromOption(
      prm.get("authorization_servers")
        .flatMap(_.asArray)
        .flatMap(_.headOption)
        .flatMap(_.asString)
    ).orElseFail(McpClientError.Auth(s"PRM at $prmUrl missing 'authorization_servers'"))

  /**
   * Scope selection strategy (MCP 2026-07-28): explicit configuration wins; then
   * the `scope` hint from the `WWW-Authenticate` challenge; then the PRM's
   * `scopes_supported`; otherwise none.
   */
  private def selectScopes(configured: Set[String], scopeHint: Option[String], prm: Json.Obj): Set[String] =
    if configured.nonEmpty then configured
    else
      scopeHint.map(_.split(' ').toSet.filter(_.nonEmpty)).filter(_.nonEmpty).getOrElse {
        prm.get("scopes_supported")
          .flatMap(_.asArray)
          .map(_.flatMap(_.asString).toSet)
          .getOrElse(Set.empty)
      }

  /**
   * Discover authorization-server metadata, trying the well-known URI forms in the
   * MCP spec's priority order. For an issuer with a path component (RFC 8414 §3.1
   * inserts the well-known segment between host and path):
   *
   *   1. `https://host/.well-known/oauth-authorization-server/<path>`
   *   2. `https://host/.well-known/openid-configuration/<path>`
   *   3. `https://host/<path>/.well-known/openid-configuration`
   *
   * For an issuer with no path: `oauth-authorization-server` then
   * `openid-configuration` at the root.
   *
   * The returned metadata's `issuer` is validated against the requested issuer —
   * a mismatch indicates a misconfigured or hostile AS and fails the flow.
   */
  private[client] def discoverAsMetadata(client: Client, issuer: String): IO[McpClientError, AsMetadata] =
    val candidates = asMetadataUrls(issuer)
    def tryAll(urls: List[String], failures: List[String]): IO[McpClientError, Json.Obj] =
      urls match
        case Nil =>
          ZIO.fail(McpClientError.Auth(
            s"No authorization server metadata found for issuer '$issuer' (tried: ${failures.reverse.mkString(", ")})"))
        case u :: rest =>
          decodeUrl(u).flatMap(fetchJsonObj(client, _, "authorization server metadata"))
            .catchAll(_ => tryAll(rest, u :: failures))
    for
      json     <- tryAll(candidates, Nil)
      metaIss  <- ZIO.fromOption(json.get("issuer").flatMap(_.asString))
                    .orElseFail(McpClientError.Auth(s"AS metadata for '$issuer' missing 'issuer'"))
      _        <- ZIO.fail(McpClientError.Auth(
                    s"AS metadata issuer mismatch: requested '$issuer' but metadata declares '$metaIss'"))
                    .when(metaIss.stripSuffix("/") != issuer.stripSuffix("/"))
      tokenEp  <- ZIO.fromOption(json.get("token_endpoint").flatMap(_.asString))
                    .orElseFail(McpClientError.Auth(s"AS metadata for '$issuer' missing 'token_endpoint'"))
    yield AsMetadata(
      issuer = metaIss,
      tokenEndpoint = tokenEp,
      authorizationEndpoint = json.get("authorization_endpoint").flatMap(_.asString),
      registrationEndpoint = json.get("registration_endpoint").flatMap(_.asString),
      clientIdMetadataDocumentSupported =
        json.get("client_id_metadata_document_supported").flatMap(_.asBoolean).getOrElse(false),
      issParameterSupported =
        json.get("authorization_response_iss_parameter_supported").flatMap(_.asBoolean).getOrElse(false),
      scopesSupported =
        json.get("scopes_supported").flatMap(_.asArray).map(_.flatMap(_.asString)).getOrElse(Chunk.empty),
    )

  private[client] def asMetadataUrls(issuer: String): List[String] =
    val trimmed = issuer.stripSuffix("/")
    val schemeEnd = trimmed.indexOf("://")
    val pathStart = if schemeEnd < 0 then -1 else trimmed.indexOf('/', schemeEnd + 3)
    if pathStart < 0 then
      List(
        s"$trimmed/.well-known/oauth-authorization-server",
        s"$trimmed/.well-known/openid-configuration",
      )
    else
      val origin = trimmed.substring(0, pathStart)
      val path   = trimmed.substring(pathStart)
      List(
        s"$origin/.well-known/oauth-authorization-server$path",
        s"$origin/.well-known/openid-configuration$path",
        s"$origin$path/.well-known/openid-configuration",
      )

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

  // --- PKCE helpers ---

  private val secureRandom = new SecureRandom()

  private[client] def randomUrlSafe(bytes: Int): String =
    val buf = new Array[Byte](bytes)
    secureRandom.synchronized(secureRandom.nextBytes(buf))
    Base64.getUrlEncoder.withoutPadding.encodeToString(buf)

  /** PKCE S256: base64url(sha256(ascii(verifier))), no padding (RFC 7636 §4.2). */
  private[client] def s256(verifier: String): String =
    val digest = MessageDigest.getInstance("SHA-256").digest(verifier.getBytes(StandardCharsets.US_ASCII))
    Base64.getUrlEncoder.withoutPadding.encodeToString(digest)
