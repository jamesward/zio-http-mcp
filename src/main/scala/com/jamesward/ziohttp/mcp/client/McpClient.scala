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

/**
 * Connection configuration.
 *
 * @param serverUrl   The MCP endpoint, e.g. `https://example.com/mcp`.
 * @param clientInfo  Identity reported to the server in `initialize`.
 * @param oauth       When set, the client runs the OAuth 2.1 `client_credentials`
 *                    flow (with automatic discovery) and attaches a bearer token.
 * @param headers     Static headers attached to every request (e.g. a fixed
 *                    `Authorization: Bearer <token>` or a custom auth header for an
 *                    upstream that authenticates by a pre-shared credential). Applied
 *                    in addition to — and before — any OAuth bearer token.
 */
final case class McpClientConfig(
  serverUrl: String,
  clientInfo: Implementation = Implementation("zio-http-mcp-client", "0.1.0"),
  oauth: Option[OAuthClientCredentials] = None,
  headers: Headers = Headers.empty,
)

object McpClient:

  /** Connect to an MCP server with full configuration. */
  def connect(config: McpClientConfig): ZIO[Client & Scope, McpClientError, McpClient] =
    for
      zclient    <- ZIO.service[Client]
      stateRef   <- Ref.make(ClientState.initial)
      idRef      <- Ref.make(0L)
      transport   = Transport(config, zclient, stateRef, idRef)
      initResult <- transport.initialize
      _          <- transport.notifyInitialized
      _          <- ZIO.addFinalizer(transport.close.ignore)
    yield Live(transport, initResult)

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
  )

  private object ClientState:
    val initial: ClientState = ClientState(None, None, None, None)

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
    zclient: Client,
    stateRef: Ref[ClientState],
    idRef: Ref[Long],
  ):
    private val clientLayer: ULayer[Client] = ZLayer.succeed(zclient)

    def initialize: IO[McpClientError, InitializeResult] =
      val params = asObj(InitializeParams(McpProtocol.Version, Json.Obj(), config.clientInfo))
      for
        result <- rpc[InitializeResult]("initialize", params)
        _      <- stateRef.update(_.copy(protocolVersion = Some(result.protocolVersion)))
      yield result

    def notifyInitialized: IO[McpClientError, Unit] =
      notification("notifications/initialized", Json.Obj())

    def rpc[A: JsonDecoder](method: String, params: Json.Obj): IO[McpClientError, A] =
      for
        n      <- idRef.updateAndGet(_ + 1)
        id      = RequestId.Num(n.toInt)
        body    = (JsonRpcMessage.Request(id, method, Some(params)): JsonRpcMessage).toJson
        result <- withAuthRetry(body, id)
        a      <- ZIO.fromEither(result.as[A])
                    .mapError(e => McpClientError.Decode(s"Failed to decode result of '$method': $e"))
      yield a

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
      for
        token <- ensureToken(forceRefresh = false)
        _     <- ZIO.scoped:
                   for
                     req  <- buildRequest(body, token)
                     resp <- streaming(req)
                     _    <- captureSession(resp)
                     _    <- ZIO.unless(resp.status.code == 202 || resp.status.isSuccess)(
                               failStatus(resp, s"Notification '$method'")
                             )
                   yield ()
      yield ()

    private def withAuthRetry(body: String, id: RequestId): IO[McpClientError, Json] =
      for
        token <- ensureToken(forceRefresh = false)
        res1  <- attempt(body, id, token)
        out   <- res1 match
                   case Attempt.Ok(json) => ZIO.succeed(json)
                   case Attempt.Unauthorized(b) =>
                     config.oauth match
                       case None => ZIO.fail(McpClientError.Auth(s"Server returned 401 Unauthorized: $b"))
                       case Some(_) =>
                         for
                           token2 <- ensureToken(forceRefresh = true)
                           res2   <- attempt(body, id, token2)
                           json   <- res2 match
                                       case Attempt.Ok(json)         => ZIO.succeed(json)
                                       case Attempt.Unauthorized(b2) => ZIO.fail(McpClientError.Auth(s"401 after refreshing token: $b2"))
                         yield json
      yield out

    private def attempt(body: String, id: RequestId, token: Option[String]): IO[McpClientError, Attempt] =
      ZIO.scoped:
        for
          req  <- buildRequest(body, token)
          resp <- streaming(req)
          _    <- captureSession(resp)
          out  <- interpret(resp, id)
        yield out

    private def interpret(resp: Response, id: RequestId): ZIO[Scope, McpClientError, Attempt] =
      if resp.status.code == 401 then
        resp.body.asString.orElseSucceed("").map(Attempt.Unauthorized.apply)
      else if !resp.status.isSuccess then
        failStatus(resp, "Request")
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
                            ClientOAuth.fetchToken(zclient, resolved, oauth)
                              .tap(t => stateRef.update(_.copy(token = Some(t))))
          yield Some(token.value)

    private def buildRequest(body: String, token: Option[String]): IO[McpClientError, Request] =
      for
        url <- decodeUrl(config.serverUrl)
        st  <- stateRef.get
      yield
        val base = Request.post(url, Body.fromString(body))
          .addHeader(Header.ContentType(MediaType.application.json))
          .addHeader("accept", "application/json, text/event-stream")
          .addHeaders(config.headers)
        val wSession = st.sessionId.fold(base)(sid => base.addHeader("mcp-session-id", sid))
        val wProto   = st.protocolVersion.fold(wSession)(v => wSession.addHeader("mcp-protocol-version", v))
        token.fold(wProto)(t => wProto.addHeader(Header.Authorization.Bearer(t)))

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

  private final class Live(transport: Transport, initResult: InitializeResult) extends McpClient:
    def serverInfo: Implementation = initResult.serverInfo
    def serverCapabilities: ServerCapabilities = initResult.capabilities
    def protocolVersion: String = initResult.protocolVersion

    def ping: IO[McpClientError, Unit] =
      transport.rpc[Json.Obj]("ping", Json.Obj()).unit

    def listTools: IO[McpClientError, Chunk[ToolDefinition]] =
      transport.rpc[ToolsListResult]("tools/list", Json.Obj()).map(_.tools)

    def callTool(name: String, arguments: Json.Obj): IO[McpClientError, CallToolResult] =
      val argOpt = if arguments.fields.isEmpty then None else Some(arguments)
      transport.rpc[CallToolResult]("tools/call", asObj(ToolCallParams(ToolName(name), argOpt)))

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

    def close: IO[McpClientError, Unit] =
      transport.close
