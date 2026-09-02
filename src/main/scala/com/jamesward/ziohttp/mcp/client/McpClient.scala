package com.jamesward.ziohttp.mcp.client

import com.jamesward.ziohttp.mcp.*
import zio.*
import zio.http.*
import zio.json.*
import zio.json.ast.Json
import zio.schema.Schema
import zio.schema.codec.JsonCodec as SchemaJsonCodec
import zio.stream.*

import scala.annotation.nowarn

/**
 * A connected MCP client speaking the Streamable HTTP transport
 * ([[https://modelcontextprotocol.io/specification/2025-11-25 MCP 2025-11-25]]).
 *
 * Obtain one with [[McpClient.connect]], which performs the `initialize` handshake
 * and sends `notifications/initialized` before handing back a ready client. The
 * connection (and its OAuth token / session) lives for the duration of the
 * surrounding [[zio.Scope]]; when the scope closes the client issues a best-effort
 * `DELETE` to release the server session.
 *
 * Transport behavior matches what real servers do in the wild:
 *   - requests are `POST`ed as JSON-RPC with `Accept: application/json, text/event-stream`,
 *   - responses are read whether the server replies with `application/json` or streams
 *     a `text/event-stream` (the client consumes the SSE only until the matching
 *     JSON-RPC response arrives, then releases the connection),
 *   - an `Mcp-Session-Id` returned by `initialize` is echoed on every later request,
 *   - the negotiated `MCP-Protocol-Version` is sent on every post-initialize request.
 */
trait McpClient:
  /** Server identity from the `initialize` result. */
  def serverInfo: Implementation

  /** Capabilities the server advertised during `initialize`. */
  def serverCapabilities: ServerCapabilities

  /** The protocol version the server negotiated. */
  def protocolVersion: String

  /**
   * The optional `instructions` string from the `initialize` result — a server-provided
   * hint describing how to use the server, which clients may surface to the LLM. `None`
   * when the server did not send one.
   */
  def instructions: Option[String]

  /** `ping` — round-trips an empty request to check liveness. */
  def ping: IO[McpClientError, Unit]

  /** `tools/list` — the tools the server exposes. */
  def listTools: IO[McpClientError, Chunk[ToolDefinition]]

  /** `tools/call` with JSON arguments. */
  def callTool(name: String, arguments: Json.Obj): IO[McpClientError, CallToolResult]

  /** `tools/call` with no arguments. */
  def callTool(name: String): IO[McpClientError, CallToolResult]

  /**
   * `tools/call` with a typed, `Schema`-derived argument value. The argument is
   * encoded to JSON with the same `zio-schema` codec the server uses to derive the
   * tool's `inputSchema`, so a shared input type round-trips exactly. `A` must encode
   * to a JSON object.
   */
  def callTool[A: Schema](name: String, arguments: A): IO[McpClientError, CallToolResult]

  /**
   * `tools/call` returning the result decoded into `B`. The tool's `structuredContent`
   * (or, failing that, its text content parsed as JSON) is decoded with `B`'s
   * `zio-schema` codec — so a successful decode *is* the schema-conformance check.
   * Fails with [[McpClientError.ToolFailed]] if the tool reported `isError`, or
   * [[McpClientError.Decode]] if the payload doesn't conform to `B`.
   */
  def callToolAs[B: Schema](name: String, arguments: Json.Obj): IO[McpClientError, B]

  /** `tools/call` with no arguments, returning the result decoded into `B`. */
  def callToolAs[B: Schema](name: String): IO[McpClientError, B]

  /** `tools/call` with a typed argument `A`, returning the result decoded into `B`. */
  def callToolAs[A: Schema, B: Schema](name: String, arguments: A): IO[McpClientError, B]

  /** `resources/list` — the concrete resources the server exposes. */
  def listResources: IO[McpClientError, Chunk[ResourceDefinition]]

  /** `resources/templates/list` — the resource templates the server exposes. */
  def listResourceTemplates: IO[McpClientError, Chunk[ResourceTemplateDefinition]]

  /** `resources/read` — the contents of a resource by URI. */
  def readResource(uri: String): IO[McpClientError, Chunk[ResourceContents]]

  /** `completion/complete` — completion candidates for a prompt/resource-template argument. */
  def complete(ref: CompletionRef, argument: CompletionArgument): IO[McpClientError, CompletionResult]

  /**
   * `completion/complete` forwarding raw JSON params and returning the raw `result`.
   * Useful for a proxy that relays a completion request verbatim without re-modeling it.
   */
  def complete(params: Json.Obj): IO[McpClientError, Json.Obj]

  /** `prompts/list` — the prompts the server exposes. */
  def listPrompts: IO[McpClientError, Chunk[PromptDefinition]]

  /** `prompts/get` with template arguments. */
  def getPrompt(name: String, arguments: Map[String, String]): IO[McpClientError, PromptGetResult]

  /** `prompts/get` with no arguments. */
  def getPrompt(name: String): IO[McpClientError, PromptGetResult]

  /** Release the server session (best effort). Called automatically on scope close. */
  def close: IO[McpClientError, Unit]

trait McpExtensionClient extends McpClient:
  def request[Params, Result](
    operation: McpOperation[Params, Result],
    params: Params,
  ): IO[McpClientError, Result]

  def requestRaw(
    method: McpMethodName,
    params: Json.Obj,
    routingName: Option[McpRoutingName] = None,
  ): IO[McpClientError, Json]

/**
 * Connection configuration.
 *
 * @param serverUrl   The MCP endpoint, e.g. `https://example.com/mcp`.
 * @param clientInfo  Identity reported to the server in `initialize`.
 * @param oauth       When set, the client runs an OAuth 2.1 flow (with automatic
 *                    discovery) and attaches a bearer token: [[OAuthClientCredentials]]
 *                    for machine-to-machine, or [[OAuthAuthorizationCode]] for the
 *                    MCP-spec authorization-code + PKCE flow (CIMD / DCR /
 *                    pre-registration, RFC 9207 `iss` validation).
 * @param headers     Static headers attached to every request (e.g. a fixed
 *                    `Authorization: Bearer <token>` or a custom auth header for an
 *                    upstream that authenticates by a pre-shared credential). Applied
 *                    in addition to — and before — any OAuth bearer token.
 */
final case class McpClientConfig(
  serverUrl: String,
  clientInfo: Implementation = Implementation("zio-http-mcp-client", "0.1.0"),
  oauth: Option[McpClientOAuth] = None,
  headers: Headers = Headers.empty,
  /**
   * The protocol version the client prefers. Defaults to the newest supported
   * revision ([[ProtocolVersion.latest]], `2026-07-28`). When it is a modern
   * (stateless) version the client probes the server with `server/discover` and
   * negotiates the highest mutually supported version, falling back to the
   * `2025-11-25` `initialize` handshake if the server is legacy. Pin this to
   * [[ProtocolVersion.V2025_11_25]] to force the legacy handshake.
   */
  preferredVersion: ProtocolVersion = ProtocolVersion.latest,
  /**
   * Handler invoked to satisfy a modern server's Multi Round-Trip Request: given
   * an [[InputRequest]] (a `sampling/createMessage` or `elicitation/create` the
   * server needs answered), it returns the result JSON to send back in
   * `inputResponses`. When unset, a server that asks for input fails the call
   * with [[McpClientError.Protocol]].
   */
  onInputRequest: Option[InputRequest => IO[McpClientError, Json]] = None,
)

object McpClient:

  /** Connect to an MCP server with full configuration. */
  def connect(config: McpClientConfig): ZIO[Client & Scope, McpClientError, McpClient] =
    connectInternal(config, McpClientExtensions.empty)

  /** Connect while advertising immutable vendor-extension declarations. */
  def connect(
    config: McpClientConfig,
    extensions: McpClientExtensions,
  ): ZIO[Client & Scope, McpClientError, McpExtensionClient] =
    connectInternal(config, extensions)

  private def connectInternal(
    config: McpClientConfig,
    extensions: McpClientExtensions,
  ): ZIO[Client & Scope, McpClientError, McpExtensionClient] =
    for
      zclient    <- ZIO.service[Client]
      stateRef   <- Ref.make(ClientState.initial)
      idRef      <- Ref.make(0L)
      transport   = Transport(config, extensions, zclient, stateRef, idRef)
      negotiated <- transport.negotiate
      _          <- ZIO.addFinalizer(transport.close.ignore)
    yield Live(transport, negotiated)

  /** Connect to an MCP server at `serverUrl` with no authorization. */
  def connect(serverUrl: String): ZIO[Client & Scope, McpClientError, McpClient] =
    connect(McpClientConfig(serverUrl))

  /**
   * Connect over the Streamable HTTP transport, attaching `headers` to every request.
   *
   * Convenience for upstreams that authenticate with a pre-shared credential — a static
   * `Authorization: Bearer <token>` or a custom header — rather than the OAuth
   * `client_credentials` flow. The end user's identity is never forwarded; only the
   * supplied headers are sent.
   */
  def streamableHttp(endpoint: URL, headers: Headers): ZIO[Client & Scope, McpClientError, McpClient] =
    connect(McpClientConfig(serverUrl = endpoint.encode, headers = headers))

  // --- internal state ---

  private final case class ClientState(
    sessionId: Option[String],
    protocolVersion: Option[String],
    resolvedOAuth: Option[ResolvedOAuth],
    token: Option[CachedToken],
    // True once negotiation settles on a modern (2026-07-28+) server: requests
    // then carry per-request `_meta` + routing headers and no session.
    modern: Boolean = false,
  )

  private object ClientState:
    val initial: ClientState = ClientState(None, None, None, None)

  /** The outcome of version negotiation: the connected server's identity plus
    * whether the client is operating in the modern or legacy era. */
  private final case class Negotiated(
    version: ProtocolVersion,
    modern: Boolean,
    serverInfo: Implementation,
    capabilities: ServerCapabilities,
    instructions: Option[String],
  )

  /** Parsed `server/discover` result. */
  private final case class DiscoverInfo(
    supportedVersions: Chunk[String],
    capabilities: ServerCapabilities,
    serverInfo: Implementation,
    instructions: Option[String],
  )

  /** An outgoing request body plus the transport decisions for it: correlation
    * id, extra (modern routing) headers, and whether to attach the session. */
  private final case class Outgoing(
    body: String,
    id: RequestId,
    extraHeaders: Headers,
    sendSession: Boolean,
  )

  private enum Attempt:
    case Ok(result: Json)
    case Unauthorized(body: String)

  /** Encode any `JsonEncoder` value into a JSON object (empty object on failure). */
  private def asObj[A: JsonEncoder](a: A): Json.Obj =
    a.toJsonAST.toOption.flatMap(_.asObject).getOrElse(Json.Obj())

  /** Encode a `Schema`-typed argument value into a JSON object for `tools/call`. */
  private def encodeArgs[A](a: A)(using schema: Schema[A]): IO[McpClientError, Json.Obj] =
    val encoded = SchemaJsonCodec.jsonEncoder(schema).encodeJson(a, None).toString
    ZIO.fromEither(encoded.fromJson[Json.Obj])
      .mapError(e => McpClientError.Decode(s"Tool arguments did not encode to a JSON object: $e"))

  /** Concatenate the text content items of a tool result. */
  private def textOf(result: CallToolResult): String =
    result.content.collect { case ToolContent.Text(t, _) => t }.mkString("\n")

  /**
   * Decode a [[CallToolResult]] into `B` using `B`'s `zio-schema` codec. Prefers
   * `structuredContent`; falls back to parsing the text content as JSON. A successful
   * decode is the schema-conformance check.
   */
  private def decodeResultAs[B](result: CallToolResult)(using schema: Schema[B]): IO[McpClientError, B] =
    if result.isError.contains(true) then
      ZIO.fail(McpClientError.ToolFailed(textOf(result)))
    else
      val jsonStr = result.structuredContent match
        case Some(json) => json.toJson
        case None       => textOf(result)
      ZIO.fromEither(SchemaJsonCodec.jsonDecoder(schema).decodeJson(jsonStr))
        .mapError(e => McpClientError.Decode(s"Failed to decode tool result into expected type: $e"))

  // --- transport ---

  private final class Transport(
    config: McpClientConfig,
    extensions: McpClientExtensions,
    zclient: Client,
    stateRef: Ref[ClientState],
    idRef: Ref[Long],
  ):
    private val clientLayer: ULayer[Client] = ZLayer.succeed(zclient)

    /**
     * Negotiate the protocol era with the server. When the client prefers a
     * modern version it probes with `server/discover`: a success (or a
     * recognized `UnsupportedProtocolVersionError`) means a modern server and the
     * client stays stateless; anything else means a legacy server and the client
     * falls back to the `initialize` handshake.
     */
    def negotiate: IO[McpClientError, Negotiated] =
      if config.preferredVersion.isStateless then
        discover(config.preferredVersion).foldZIO(
          {
            case McpClientError.JsonRpc(code, _, data)
                if code == ErrorCode.UnsupportedProtocolVersion.code =>
              // Modern server that doesn't support our preferred version: pick
              // the highest one it lists and re-discover, else fall back.
              val supported = data.flatMap(_.asObject).flatMap(_.get("supported")).flatMap(_.asArray)
                .map(_.flatMap(_.asString)).getOrElse(Chunk.empty)
              chooseModern(supported) match
                case Some(v) => discover(v).flatMap(finishModern)
                case None    => legacyInit
            case _ =>
              // Not a recognized modern error → treat the server as legacy.
              legacyInit
          },
          d => finishModern(d),
        )
      else legacyInit

    /** Highest modern version both the client and `supported` (server) accept. */
    private def chooseModern(supported: Chunk[String]): Option[ProtocolVersion] =
      ProtocolVersion.all.filter(_.isStateless).find(v => supported.contains(v.wire))

    /**
     * Commit modern-era state from a discovery result and produce [[Negotiated]].
     * The chosen version — the highest one both peers support — is written back
     * to the client state so every subsequent request carries the *negotiated*
     * version in its `_meta` / `MCP-Protocol-Version` header, not the one we
     * probed with. (Sending the probed/requested version instead of the
     * negotiated one is a known real-world interop bug.)
     */
    private def finishModern(d: DiscoverInfo): UIO[Negotiated] =
      val chosen = chooseModern(d.supportedVersions).getOrElse(config.preferredVersion)
      stateRef
        .update(_.copy(protocolVersion = Some(chosen.wire)))
        .as(Negotiated(chosen, modern = true, d.serverInfo, d.capabilities, d.instructions))

    /** Probe / query `server/discover` and parse its result. Marks the client
      * modern for the duration so the request carries modern metadata/headers. */
    private def discover(version: ProtocolVersion): IO[McpClientError, DiscoverInfo] =
      for
        _      <- stateRef.update(_.copy(modern = true, protocolVersion = Some(version.wire)))
        result <- rpcRaw("server/discover", Json.Obj())
      yield
        val obj = result.asObject.getOrElse(Json.Obj())
        val supported = obj.get("supportedVersions").flatMap(_.asArray)
          .map(_.flatMap(_.asString)).getOrElse(Chunk(version.wire))
        val caps = obj.get("capabilities").flatMap(_.as[ServerCapabilities].toOption).getOrElse(ServerCapabilities())
        val serverInfo = obj.get("_meta").flatMap(_.asObject).flatMap(_.get(McpMeta.ServerInfo))
          .flatMap(_.as[Implementation].toOption).getOrElse(Implementation("unknown", "0"))
        val instructions = obj.get("instructions").flatMap(_.asString)
        DiscoverInfo(supported, caps, serverInfo, instructions)

    /** Legacy `initialize` handshake (2025-11-25 and earlier). */
    private def legacyInit: IO[McpClientError, Negotiated] =
      // Request the client's preferred version when it is itself a legacy
      // revision (e.g. pinned to 2025-06-18); when the preference is a modern
      // version but we fell back to the handshake, request our newest legacy one.
      val requestedVersion =
        if config.preferredVersion.isStateless then McpProtocol.Version else config.preferredVersion.wire
      for
        _      <- stateRef.update(_.copy(modern = false, sessionId = None))
        params  = asObj(InitializeParams(
                    requestedVersion,
                    McpExtensionCapabilities.toClientCapabilities(extensions.capabilities),
                    config.clientInfo,
                  ))
        result <- rpc[InitializeResult]("initialize", params)
        _      <- stateRef.update(_.copy(protocolVersion = Some(result.protocolVersion)))
        _      <- notifyInitialized
      yield Negotiated(
        ProtocolVersion.parse(result.protocolVersion).getOrElse(ProtocolVersion.default),
        modern = false, result.serverInfo, result.capabilities, result.instructions,
      )

    def notifyInitialized: IO[McpClientError, Unit] =
      notification("notifications/initialized", Json.Obj())

    def rpc[A: JsonDecoder](method: String, params: Json.Obj): IO[McpClientError, A] =
      rpcRaw(method, params).flatMap: result =>
        ZIO.fromEither(result.as[A])
          .mapError(e => McpClientError.Decode(s"Failed to decode result of '$method': $e"))

    /** Send a request and return its raw `result` JSON, applying modern
      * metadata/headers when the negotiated era is modern. */
    def rpcRaw(
      method: String,
      params: Json.Obj,
      routingName: Option[McpRoutingName] = None,
    ): IO[McpClientError, Json] =
      for
        st     <- stateRef.get
        n      <- idRef.updateAndGet(_ + 1)
        id      = RequestId.Num(n.toInt)
        version = st.protocolVersion.flatMap(ProtocolVersion.parse).getOrElse(config.preferredVersion)
        effParams = if st.modern then withModernMeta(params, version) else params
        body    = (JsonRpcMessage.Request(id, method, Some(effParams)): JsonRpcMessage).toJson
        extra   = if st.modern then modernHeaders(version, method, effParams, routingName) else Headers.empty
        result <- withAuthRetry(Outgoing(body, id, extra, sendSession = !st.modern))
      yield result

    /**
     * Modern `tools/call` with Multi Round-Trip Request handling. If the server
     * answers `input_required`, the configured `onInputRequest` handler fulfils
     * each request and the call is retried with `inputResponses` until it
     * completes. Legacy servers never return `input_required`, so this returns
     * their result on the first pass.
     */
    def toolsCall(params: Json.Obj): IO[McpClientError, CallToolResult] =
      def loop(prior: Chunk[InputResponse], depth: Int): IO[McpClientError, CallToolResult] =
        val callParams =
          if prior.isEmpty then params
          else Json.Obj(params.fields.filterNot(_._1 == "inputResponses") :+
            ("inputResponses" -> (Json.Arr(prior.map(_.toJsonAST.getOrElse(Json.Obj()))): Json)))
        rpcRaw("tools/call", callParams).flatMap: result =>
          val resultType = result.asObject.flatMap(_.get("resultType")).flatMap(_.asString)
          if resultType.contains(ModernEnvelope.ResultTypeInputRequired) then
            config.onInputRequest match
              case None =>
                ZIO.fail(McpClientError.Protocol("Server requested input (MRTR) but no onInputRequest handler is configured"))
              case Some(handler) if depth >= 32 =>
                ZIO.fail(McpClientError.Protocol("MRTR exceeded the maximum number of round trips"))
              case Some(handler) =>
                val requests = result.asObject.flatMap(_.get("inputRequests")).flatMap(_.asArray)
                  .map(_.flatMap(_.as[InputRequest].toOption)).getOrElse(Chunk.empty)
                for
                  answers <- ZIO.foreach(requests)(req => handler(req).map(r => InputResponse(req.id, r)))
                  out     <- loop(prior ++ answers, depth + 1)
                yield out
          else
            ZIO.fromEither(result.as[CallToolResult])
              .mapError(e => McpClientError.Decode(s"Failed to decode tool result: $e"))
      loop(Chunk.empty, 0)

    def close: IO[McpClientError, Unit] =
      stateRef.get.flatMap: st =>
        st.sessionId match
          case None => ZIO.unit
          case Some(sid) =>
            for
              token <- ensureToken(forceRefresh = false).catchAll(_ => ZIO.none)
              url   <- decodeUrl(config.serverUrl)
              base   = Request(method = Method.DELETE, url = url).addHeader("mcp-session-id", sid).addHeaders(config.headers)
              wProto = st.protocolVersion.fold(base)(v => base.addHeader("mcp-protocol-version", v))
              req    = token.fold(wProto)(t => wProto.addHeader(Header.Authorization.Bearer(t)))
              _     <- zclient.batched(req).ignore
            yield ()

    private def notification(method: String, params: Json.Obj): IO[McpClientError, Unit] =
      val body = (JsonRpcMessage.Notification(method, Some(params)): JsonRpcMessage).toJson
      // Notifications (only `notifications/initialized`, on the legacy path)
      // carry the session and legacy protocol-version header.
      val out = Outgoing(body, RequestId.Num(0), Headers.empty, sendSession = true)
      for
        token <- ensureToken(forceRefresh = false)
        _     <- ZIO.scoped:
                   for
                     req  <- buildRequest(out, token)
                     resp <- streaming(req)
                     _    <- captureSession(resp)
                     _    <- ZIO.unless(resp.status.code == 202 || resp.status.isSuccess)(
                               failStatus(resp, s"Notification '$method'")
                             )
                   yield ()
      yield ()

    private def withAuthRetry(o: Outgoing): IO[McpClientError, Json] =
      for
        token <- ensureToken(forceRefresh = false)
        res1  <- attempt(o, token)
        out   <- res1 match
                   case Attempt.Ok(json) => ZIO.succeed(json)
                   case Attempt.Unauthorized(b) =>
                     config.oauth match
                       case None => ZIO.fail(McpClientError.Auth(s"Server returned 401 Unauthorized: $b"))
                       case Some(_) =>
                         for
                           token2 <- ensureToken(forceRefresh = true)
                           res2   <- attempt(o, token2)
                           json   <- res2 match
                                       case Attempt.Ok(json)         => ZIO.succeed(json)
                                       case Attempt.Unauthorized(b2) => ZIO.fail(McpClientError.Auth(s"401 after refreshing token: $b2"))
                         yield json
      yield out

    private def attempt(o: Outgoing, token: Option[String]): IO[McpClientError, Attempt] =
      ZIO.scoped:
        for
          req  <- buildRequest(o, token)
          resp <- streaming(req)
          _    <- captureSession(resp)
          out  <- interpret(resp, o.id)
        yield out

    private def interpret(resp: Response, id: RequestId): ZIO[Scope, McpClientError, Attempt] =
      if resp.status.code == 401 then
        resp.body.asString.orElseSucceed("").map(Attempt.Unauthorized.apply)
      else if resp.status.code == 403 then
        // Authorization denial (insufficient scope): an HTTP-layer auth outcome,
        // the sibling of the 401 case above — not a JSON-RPC negotiation error.
        // Surface it as a protocol-level failure; the body still carries the
        // server's JSON-RPC -32003 "insufficient_scope" detail in the message.
        resp.body.asString.orElseSucceed("").flatMap: b =>
          ZIO.fail(McpClientError.Protocol(s"Request returned 403: $b"))
      else if !resp.status.isSuccess then
        // A modern server signals negotiation failures with a non-2xx status and
        // a JSON-RPC error body (UnsupportedProtocolVersion -32022, HeaderMismatch
        // -32020, method-not-found -32601). Surface those as McpClientError.JsonRpc
        // so negotiation can recognize a modern server; otherwise it is a
        // transport-level failure that identifies a legacy server.
        resp.body.asString.orElseSucceed("").flatMap: b =>
          b.fromJson[Json.Obj].toOption.flatMap(o => o.get("error").flatMap(_.asObject)) match
            case Some(errObj) =>
              val code = errObj.get("code").flatMap(_.asNumber).map(_.value.intValue).getOrElse(0)
              val msg  = errObj.get("message").flatMap(_.asString).getOrElse("Unknown error")
              val data = errObj.get("data")
              ZIO.fail(McpClientError.JsonRpc(code, msg, data))
            case None =>
              ZIO.fail(McpClientError.Protocol(s"Request returned ${resp.status.code}: $b"))
      else
        parseResponse(resp, id).map(Attempt.Ok.apply)

    private def ensureToken(forceRefresh: Boolean): IO[McpClientError, Option[String]] =
      config.oauth match
        case None => ZIO.none
        case Some(oauth) =>
          for
            now      <- Clock.instant
            st       <- stateRef.get
            resolved <- st.resolvedOAuth match
                          case Some(r) => ZIO.succeed(r)
                          case None    =>
                            ClientOAuth.resolve(zclient, config.serverUrl, oauth)
                              .tap(r => stateRef.update(_.copy(resolvedOAuth = Some(r))))
            token    <- st.token match
                          case Some(t) if !forceRefresh && t.isValid(now) => ZIO.succeed(t)
                          case _ =>
                            ClientOAuth.fetchToken(zclient, resolved, oauth, st.token)
                              .tap(t => stateRef.update(_.copy(token = Some(t))))
          yield Some(token.value)

    private def buildRequest(o: Outgoing, token: Option[String]): IO[McpClientError, Request] =
      for
        url <- decodeUrl(config.serverUrl)
        st  <- stateRef.get
      yield
        val base = Request.post(url, Body.fromString(o.body))
          .addHeader(Header.ContentType(MediaType.application.json))
          .addHeader("accept", "application/json, text/event-stream")
          .addHeaders(config.headers)
          .addHeaders(o.extraHeaders)
        // Modern requests are stateless (no `Mcp-Session-Id`) and carry their
        // `MCP-Protocol-Version` in `o.extraHeaders`; legacy requests echo the
        // negotiated session id and protocol-version header here.
        val wSession =
          if o.sendSession then st.sessionId.fold(base)(sid => base.addHeader("mcp-session-id", sid)) else base
        val wProto =
          if o.sendSession then st.protocolVersion.fold(wSession)(v => wSession.addHeader("mcp-protocol-version", v)) else wSession
        token.fold(wProto)(t => wProto.addHeader(Header.Authorization.Bearer(t)))

    /** Modern per-request routing headers: `MCP-Protocol-Version`, `Mcp-Method`,
      * and `Mcp-Name` (mirroring `params.name` / `params.uri`). */
    private def modernHeaders(
      version: ProtocolVersion,
      method: String,
      params: Json.Obj,
      routingName: Option[McpRoutingName],
    ): Headers =
      val nameValue = routingName.map(_.value).orElse:
        method match
          case "tools/call" | "prompts/get" => params.get("name").flatMap(_.asString)
          case "resources/read"             => params.get("uri").flatMap(_.asString)
          case _                            => None
      val base = Headers(Negotiation.ProtocolVersionHeader, version.wire) ++
        Headers(Negotiation.MethodHeader, method)
      nameValue.fold(base)(n => base ++ Headers(Negotiation.NameHeader, encodeHeaderValue(n)))

    /** Merge the modern `_meta` (protocol version, client info, client
      * capabilities) into a request's params, preserving any existing `_meta`. */
    private def withModernMeta(params: Json.Obj, version: ProtocolVersion): Json.Obj =
      val modernMeta = Chunk[(String, Json)](
        McpMeta.ProtocolVersion -> Json.Str(version.wire),
        McpMeta.ClientInfo -> asObj(config.clientInfo),
        McpMeta.ClientCapabilities -> McpExtensionCapabilities.toClientCapabilities(extensions.capabilities),
      )
      val reserved = modernMeta.map(_._1).toSet
      val existing = params.get("_meta").flatMap(_.asObject).map(_.fields).getOrElse(Chunk.empty)
      val merged = existing.filterNot((key, _) => reserved.contains(key)) ++ modernMeta
      Json.Obj(params.fields.filterNot(_._1 == "_meta") :+ ("_meta" -> (Json.Obj(merged): Json)))

    /** Encode a header value using the Base64 sentinel form when it is not a
      * safe plain-ASCII value (per the Streamable HTTP value-encoding rules). */
    private def encodeHeaderValue(value: String): String =
      val plainSafe = value.nonEmpty
        && value.forall(c => c >= 0x21 && c <= 0x7e)
        && !(value.startsWith("=?base64?") && value.endsWith("?="))
      if plainSafe then value
      else "=?base64?" + java.util.Base64.getEncoder.encodeToString(
        value.getBytes(java.nio.charset.StandardCharsets.UTF_8)) + "?="

    @nowarn("msg=deprecated")
    private def streaming(req: Request): ZIO[Scope, McpClientError, Response] =
      zclient.request(req)
        .mapError(t => McpClientError.Transport(s"Request failed: ${t.getMessage}", Some(t)))

    private def failStatus(resp: Response, what: String): IO[McpClientError, Nothing] =
      resp.body.asString.orElseSucceed("").flatMap: b =>
        ZIO.fail(McpClientError.Protocol(s"$what returned ${resp.status.code}: $b"))

    private def captureSession(resp: Response): UIO[Unit] =
      resp.rawHeader("mcp-session-id") match
        case Some(sid) => stateRef.update(_.copy(sessionId = Some(sid)))
        case None      => ZIO.unit

    /** Read the JSON-RPC result for `id`, whether the body is JSON or an SSE stream. */
    private def parseResponse(resp: Response, id: RequestId): ZIO[Scope, McpClientError, Json] =
      val contentType = resp.rawHeader("content-type").getOrElse("").toLowerCase
      if contentType.contains("text/event-stream") then parseSse(resp, id)
      else parseJsonBody(resp)

    private def parseJsonBody(resp: Response): IO[McpClientError, Json] =
      for
        body <- resp.body.asString
                  .mapError(t => McpClientError.Transport(s"Failed to read response body: ${t.getMessage}", Some(t)))
        obj  <- ZIO.fromEither(body.fromJson[Json.Obj])
                  .mapError(e => McpClientError.Protocol(s"Invalid JSON-RPC response: $e ($body)"))
        out  <- ZIO.fromEither(extractResult(obj))
      yield out

    private def parseSse(resp: Response, id: RequestId): ZIO[Scope, McpClientError, Json] =
      resp.body.asStream
        .via(ZPipeline.utf8Decode)
        .via(ZPipeline.splitLines)
        .mapAccum(Chunk.empty[String]): (buf, line) =>
          if line.isEmpty then (Chunk.empty[String], Some(buf))
          else (buf :+ line, None)
        .collect { case Some(lines) => lines }
        .map(extractSseData)
        .collectSome
        .map(_.fromJson[Json.Obj])
        .collect { case Right(obj) => obj }
        .filter(obj => isResponseFor(obj, id))
        .runHead
        .mapError(t => McpClientError.Transport(s"Failed reading SSE stream: ${t.getMessage}", Some(t)))
        .flatMap:
          case Some(obj) => ZIO.fromEither(extractResult(obj))
          case None      => ZIO.fail(McpClientError.Protocol("SSE stream ended before a matching JSON-RPC response"))

    /** True when the object is the JSON-RPC response (result or error) for `id`. */
    private def isResponseFor(obj: Json.Obj, id: RequestId): Boolean =
      val hasMethod = obj.get("method").isDefined
      val hasResult = obj.get("result").isDefined
      val hasError  = obj.get("error").isDefined
      val idMatches = obj.get("id").flatMap(_.as[RequestId].toOption).contains(id)
      !hasMethod && (hasResult || hasError) && idMatches

    /** Join `data:` lines of one SSE event; `None` if the event carries no data. */
    private def extractSseData(lines: Chunk[String]): Option[String] =
      val dataLines = lines.collect:
        case l if l.startsWith("data:") => l.stripPrefix("data:").stripPrefix(" ")
      if dataLines.isEmpty then None else Some(dataLines.mkString("\n"))

    /** Turn a JSON-RPC response object into its `result`, or a [[McpClientError.JsonRpc]]. */
    private def extractResult(obj: Json.Obj): Either[McpClientError, Json] =
      obj.get("error") match
        case Some(errJson) =>
          val errObj = errJson.asObject
          val code = errObj.flatMap(_.get("code")).flatMap(_.asNumber).map(_.value.intValue).getOrElse(0)
          val msg  = errObj.flatMap(_.get("message")).flatMap(_.asString).getOrElse("Unknown error")
          val data = errObj.flatMap(_.get("data"))
          Left(McpClientError.JsonRpc(code, msg, data))
        case None =>
          Right(obj.get("result").getOrElse(Json.Obj()))

    private def decodeUrl(s: String): IO[McpClientError, URL] =
      ZIO.fromEither(URL.decode(s))
        .mapError(e => McpClientError.Transport(s"Invalid URL '$s': ${e.getMessage}", Some(e)))

  // --- high-level client backed by the transport ---

  private final class Live(transport: Transport, negotiated: Negotiated) extends McpExtensionClient:
    def serverInfo: Implementation = negotiated.serverInfo
    def serverCapabilities: ServerCapabilities = negotiated.capabilities
    def protocolVersion: String = negotiated.version.wire
    def instructions: Option[String] = negotiated.instructions

    def ping: IO[McpClientError, Unit] =
      transport.rpc[Json.Obj]("ping", Json.Obj()).unit

    def listTools: IO[McpClientError, Chunk[ToolDefinition]] =
      transport.rpc[ToolsListResult]("tools/list", Json.Obj()).map(_.tools)

    def callTool(name: String, arguments: Json.Obj): IO[McpClientError, CallToolResult] =
      val argOpt = if arguments.fields.isEmpty then None else Some(arguments)
      transport.toolsCall(asObj(ToolCallParams(ToolName(name), argOpt)))

    def callTool(name: String): IO[McpClientError, CallToolResult] =
      callTool(name, Json.Obj())

    def callTool[A: Schema](name: String, arguments: A): IO[McpClientError, CallToolResult] =
      encodeArgs(arguments).flatMap(callTool(name, _))

    def callToolAs[B: Schema](name: String, arguments: Json.Obj): IO[McpClientError, B] =
      callTool(name, arguments).flatMap(decodeResultAs[B])

    def callToolAs[B: Schema](name: String): IO[McpClientError, B] =
      callToolAs[B](name, Json.Obj())

    def callToolAs[A: Schema, B: Schema](name: String, arguments: A): IO[McpClientError, B] =
      encodeArgs(arguments).flatMap(obj => callToolAs[B](name, obj))

    def listResources: IO[McpClientError, Chunk[ResourceDefinition]] =
      transport.rpc[ResourcesListResult]("resources/list", Json.Obj()).map(_.resources)

    def listResourceTemplates: IO[McpClientError, Chunk[ResourceTemplateDefinition]] =
      transport.rpc[ResourceTemplatesListResult]("resources/templates/list", Json.Obj()).map(_.resourceTemplates)

    def readResource(uri: String): IO[McpClientError, Chunk[ResourceContents]] =
      transport.rpc[ResourceReadResult]("resources/read", asObj(ResourceReadParams(uri))).map(_.contents)

    def complete(ref: CompletionRef, argument: CompletionArgument): IO[McpClientError, CompletionResult] =
      transport.rpc[CompletionResult]("completion/complete", asObj(CompletionCompleteParams(ref, argument)))

    def complete(params: Json.Obj): IO[McpClientError, Json.Obj] =
      transport.rpc[Json.Obj]("completion/complete", params)

    def listPrompts: IO[McpClientError, Chunk[PromptDefinition]] =
      transport.rpc[PromptsListResult]("prompts/list", Json.Obj()).map(_.prompts)

    def getPrompt(name: String, arguments: Map[String, String]): IO[McpClientError, PromptGetResult] =
      val argOpt = if arguments.isEmpty then None else Some(arguments)
      transport.rpc[PromptGetResult]("prompts/get", asObj(PromptGetParams(PromptName(name), argOpt)))

    def getPrompt(name: String): IO[McpClientError, PromptGetResult] =
      getPrompt(name, Map.empty)

    def request[Params, Result](
      operation: McpOperation[Params, Result],
      params: Params,
    ): IO[McpClientError, Result] =
      if !operation.protocolSupport.supports(negotiated.version) then
        ZIO.fail(McpClientError.Protocol(
          s"Extension method '${operation.method.value}' is unavailable for ${negotiated.version.wire}"
        ))
      else
        for
          paramsJson <- ZIO.fromEither(operation.paramsCodec.encode(params))
                          .mapError(message => McpClientError.Decode(
                            s"Failed to encode params of '${operation.method.value}': $message"
                          ))
          paramsObj  <- ZIO.fromOption(paramsJson.asObject)
                          .orElseFail(McpClientError.Decode("Extension params must encode to an object"))
          resultJson <- transport.rpcRaw(operation.method.value, paramsObj, operation.routingName(params))
          result     <- ZIO.fromEither(operation.resultCodec.decode(resultJson))
                          .mapError(message => McpClientError.Decode(
                            s"Failed to decode result of '${operation.method.value}': $message"
                          ))
        yield result

    def requestRaw(
      method: McpMethodName,
      params: Json.Obj,
      routingName: Option[McpRoutingName],
    ): IO[McpClientError, Json] =
      transport.rpcRaw(method.value, params, routingName)

    def close: IO[McpClientError, Unit] =
      transport.close
