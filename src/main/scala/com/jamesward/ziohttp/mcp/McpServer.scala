package com.jamesward.ziohttp.mcp

import zio.*
import zio.http.*
import zio.json.*
import zio.json.ast.Json
import zio.stream.*

// --- Session State ---

enum SessionState:
  case Initializing
  case Active

object SessionState:
  given CanEqual[SessionState, SessionState] = CanEqual.derived

// --- MCP Server ---

final class McpServer[-R] private (
  val serverInfo: Implementation,
  val tools: Chunk[McpToolHandlerR[R]],
  val resources: Chunk[McpResourceHandler],
  val resourceTemplates: Chunk[McpResourceTemplateHandler],
  val prompts: Chunk[McpPromptHandler],
):
  def tool[R1](t: McpToolHandlerR[R1]): McpServer[R & R1] =
    new McpServer(serverInfo, tools :+ t, resources, resourceTemplates, prompts)

  def resource(r: McpResourceHandler): McpServer[R] =
    new McpServer(serverInfo, tools, resources :+ r, resourceTemplates, prompts)

  def resourceTemplate(rt: McpResourceTemplateHandler): McpServer[R] =
    new McpServer(serverInfo, tools, resources, resourceTemplates :+ rt, prompts)

  def prompt(p: McpPromptHandler): McpServer[R] =
    new McpServer(serverInfo, tools, resources, resourceTemplates, prompts :+ p)

  private val toolsByName: Map[ToolName, McpToolHandlerR[R]] =
    tools.map(t => t.name -> t).toMap

  private val promptsByName: Map[PromptName, McpPromptHandler] =
    prompts.map(p => p.definition.name -> p).toMap

  private val serverCapabilities: ServerCapabilities =
    ServerCapabilities(
      tools = if tools.nonEmpty then Some(Json.Obj()) else None,
      resources = if resources.nonEmpty || resourceTemplates.nonEmpty then Some(Json.Obj(Chunk("subscribe" -> Json.Bool(true)))) else None,
      prompts = if prompts.nonEmpty then Some(Json.Obj()) else None,
      logging = Some(Json.Obj()),
      completions = Some(Json.Obj()),
    )

  def routes: Routes[R & McpServer.State, Response] =
    Routes(
      Method.POST / "mcp" -> handler(postHandler),
      Method.GET / "mcp"  -> handler(getHandler),
      Method.GET / "mcp" / trailing -> Handler.notFound,
      Method.DELETE / "mcp" -> handler(deleteHandler),
    ).sandbox

  def statelessRoutes: Routes[R, Response] =
    Routes(
      Method.POST / "mcp" -> handler(statelessPostHandler),
      Method.GET / "mcp"  -> handler((_: Request) => ZIO.succeed(Response.status(Status.MethodNotAllowed))),
      Method.GET / "mcp" / trailing -> Handler.notFound,
      Method.DELETE / "mcp" -> handler((_: Request) => ZIO.succeed(Response.status(Status.MethodNotAllowed))),
    ).sandbox

  private def validateOrigin(request: Request): ZIO[Any, Response, Unit] =
    request.rawHeader("origin") match
      case Some(o) =>
        val originHost = o.replaceFirst("^https?://", "").toLowerCase
        ZIO.unless(McpServer.isLocalhostHost(originHost))(ZIO.fail(Response.status(Status.Forbidden))).unit
      case None =>
        ZIO.unit

  private def postHandler(request: Request): ZIO[R & McpServer.State, Response, Response] =
    for
      _        <- validateOrigin(request)
      state    <- ZIO.service[McpServer.State]
      body     <- request.body.asString.orElseFail(badRequest("Failed to read request body"))
      bodyJson <- ZIO.fromEither(body.fromJson[Json.Obj])
                    .mapError(e => jsonRpcErrorResponse(None, ErrorCode.ParseError, s"Parse error: $e"))
      response <- routeMessage(request, state.sessions, state.pendingRequests, bodyJson)
    yield response

  private def statelessPostHandler(request: Request): ZIO[R, Response, Response] =
    for
      _        <- validateOrigin(request)
      body     <- request.body.asString.orElseFail(badRequest("Failed to read request body"))
      bodyJson <- ZIO.fromEither(body.fromJson[Json.Obj])
                    .mapError(e => jsonRpcErrorResponse(None, ErrorCode.ParseError, s"Parse error: $e"))
      message  <- ZIO.fromEither(bodyJson.toJson.fromJson[JsonRpcMessage])
                    .mapError(e => jsonRpcErrorResponse(None, ErrorCode.ParseError, s"Parse error: $e"))
      response <- message match
        case JsonRpcMessage.Request(id, "initialize", params) =>
          parseInitializeParams(id, params).flatMap(r => jsonRpcResponse(id, r))
        case JsonRpcMessage.Request(id, method, params) =>
          McpDispatchMethod.parse(method) match
            case Some(dm) => dispatchMethod(id, dm, params, statelessHandleToolsCall)
            case None     => ZIO.fail(jsonRpcErrorResponse(Some(id), ErrorCode.MethodNotFound, s"Method not found: $method"))
        case JsonRpcMessage.Notification(_, _) =>
          ZIO.succeed(Response.ok)
    yield response

  // --- Shared method dispatch (used by both stateful and stateless) ---

  private def dispatchMethod[R1 <: R](
    id: RequestId,
    method: McpDispatchMethod,
    params: Option[Json.Obj],
    onToolsCall: (RequestId, Option[Json.Obj]) => ZIO[R1, Response, Response],
  ): ZIO[R1, Response, Response] =
    method match
      case McpDispatchMethod.Ping =>
        jsonRpcResponse(id, Json.Obj())
      case McpDispatchMethod.ToolsList =>
        handleToolsList(id, params)
      case McpDispatchMethod.ToolsCall =>
        onToolsCall(id, params)
      case McpDispatchMethod.ResourcesList =>
        handleResourcesList(id)
      case McpDispatchMethod.ResourceTemplatesList =>
        handleResourceTemplatesList(id)
      case McpDispatchMethod.ResourcesRead =>
        handleResourceRead(id, params)
      case McpDispatchMethod.ResourcesSubscribe =>
        jsonRpcResponse(id, Json.Obj())
      case McpDispatchMethod.ResourcesUnsubscribe =>
        jsonRpcResponse(id, Json.Obj())
      case McpDispatchMethod.PromptsList =>
        handlePromptsList(id)
      case McpDispatchMethod.PromptsGet =>
        handlePromptsGet(id, params)
      case McpDispatchMethod.LoggingSetLevel =>
        jsonRpcResponse(id, Json.Obj())
      case McpDispatchMethod.CompletionComplete =>
        handleCompletionComplete(id, params)

  private def routeMessage(
    request: Request,
    sessions: Ref[Map[SessionId, SessionState]],
    pendingReqs: Ref[Map[RequestId, Promise[Nothing, Json]]],
    bodyJson: Json.Obj,
  ): ZIO[R, Response, Response] =
    // Check if this is a JSON-RPC response (has "result" or "error", no "method")
    val hasResult = bodyJson.get("result").isDefined
    val hasError = bodyJson.get("error").isDefined
    val hasMethod = bodyJson.get("method").isDefined

    if (hasResult || hasError) && !hasMethod then
      handleClientResponse(pendingReqs, bodyJson)
    else
      val message = bodyJson.toJson.fromJson[JsonRpcMessage]
      ZIO.fromEither(message)
        .mapError(e => jsonRpcErrorResponse(None, ErrorCode.ParseError, s"Parse error: $e"))
        .flatMap:
          case JsonRpcMessage.Request(id, method, params) =>
            handleRequest(request, sessions, pendingReqs, id, method, params)
          case JsonRpcMessage.Notification(method, params) =>
            handleNotification(request, sessions, method, params)

  private def handleClientResponse(
    pendingReqs: Ref[Map[RequestId, Promise[Nothing, Json]]],
    bodyJson: Json.Obj,
  ): ZIO[Any, Response, Response] =
    val id = bodyJson.get("id").flatMap(_.as[RequestId].toOption)
    id match
      case None =>
        ZIO.succeed(Response.status(Status.Accepted))
      case Some(reqId) =>
        val result = bodyJson.get("result").getOrElse(Json.Obj())
        pendingReqs.get.flatMap: pending =>
          pending.get(reqId) match
            case None =>
              ZIO.succeed(Response.status(Status.Accepted))
            case Some(promise) =>
              promise.succeed(result).as(Response.status(Status.Accepted))

  private def handleRequest(
    request: Request,
    sessions: Ref[Map[SessionId, SessionState]],
    pendingReqs: Ref[Map[RequestId, Promise[Nothing, Json]]],
    id: RequestId,
    method: String,
    params: Option[Json.Obj],
  ): ZIO[R, Response, Response] =
    method match
      case "initialize" =>
        handleInitialize(sessions, id, params)
      case _ =>
        McpDispatchMethod.parse(method) match
          case Some(dm) =>
            withSession(request, sessions):
              dispatchMethod(id, dm, params, handleToolsCall(_, _, pendingReqs))
          case None =>
            ZIO.fail(jsonRpcErrorResponse(Some(id), ErrorCode.MethodNotFound, s"Method not found: $method"))

  private def handleNotification(
    request: Request,
    sessions: Ref[Map[SessionId, SessionState]],
    method: String,
    params: Option[Json.Obj],
  ): ZIO[Any, Response, Response] =
    McpNotificationMethod.parse(method) match
      case Some(McpNotificationMethod.Initialized) =>
        val sessionId = request.rawHeader("mcp-session-id").map(SessionId(_))
        sessionId match
          case Some(sid) =>
            sessions.update(_.updatedWith(sid):
              case Some(_) => Some(SessionState.Active)
              case None    => None
            ).as(Response.ok)
          case None =>
            ZIO.succeed(Response.ok)
      case Some(McpNotificationMethod.Cancelled) =>
        ZIO.succeed(Response.ok)
      case None =>
        ZIO.succeed(Response.ok)

  private def parseInitializeParams(
    id: RequestId,
    params: Option[Json.Obj],
  ): ZIO[Any, Response, InitializeResult] =
    val paramsJson = params.getOrElse(Json.Obj()).toJson
    ZIO.fromEither(paramsJson.fromJson[InitializeParams])
      .mapError(e => jsonRpcErrorResponse(Some(id), ErrorCode.InvalidParams, s"Invalid initialize params: $e"))
      .as:
        InitializeResult(
          protocolVersion = McpProtocol.Version,
          capabilities = serverCapabilities,
          serverInfo = serverInfo,
        )

  private def handleInitialize(
    sessions: Ref[Map[SessionId, SessionState]],
    id: RequestId,
    params: Option[Json.Obj],
  ): ZIO[Any, Response, Response] =
    parseInitializeParams(id, params).flatMap: result =>
      val sessionId = SessionId.generate
      sessions.update(_.updated(sessionId, SessionState.Initializing)) *>
        jsonRpcResponse(id, result).map(_.addHeader("mcp-session-id", sessionId.value))

  private def handleToolsList(
    id: RequestId,
    params: Option[Json.Obj],
  ): ZIO[Any, Response, Response] =
    jsonRpcResponse(id, ToolsListResult(tools = tools.map(_.definition)))

  private def resolveToolCall(
    id: RequestId,
    params: Option[Json.Obj],
  ): ZIO[Any, Response, (McpToolHandlerR[R], ToolCallParams)] =
    val paramsJson = params.getOrElse(Json.Obj()).toJson
    ZIO.fromEither(paramsJson.fromJson[ToolCallParams])
      .mapError(e => jsonRpcErrorResponse(Some(id), ErrorCode.InvalidParams, s"Invalid tool call params: $e"))
      .flatMap: callParams =>
        toolsByName.get(callParams.name) match
          case None =>
            ZIO.fail(jsonRpcErrorResponse(Some(id), ErrorCode.InvalidParams, s"Unknown tool: ${callParams.name.value}"))
          case Some(tool) =>
            ZIO.succeed((tool, callParams))

  private def handleToolsCall(
    id: RequestId,
    params: Option[Json.Obj],
    pendingReqs: Ref[Map[RequestId, Promise[Nothing, Json]]],
  ): ZIO[R, Response, Response] =
    resolveToolCall(id, params).flatMap: (tool, callParams) =>
      val progressToken = params.flatMap(_.get("_meta")).flatMap(_.asObject).flatMap(_.get("progressToken"))
      Queue.unbounded[JsonRpcMessage].flatMap: queue =>
        val ctx = McpToolContext.make(queue, pendingReqs, progressToken)
        val toolEffect = tool.callWithContext(callParams.arguments, ctx)

        // Fork the tool, stream messages + result as SSE
        Promise.make[Nothing, CallToolResult].flatMap: resultPromise =>
          val runTool = toolEffect
            .flatMap(resultPromise.succeed)
            .catchAllDefect: defect =>
              val errorResult = CallToolResult(
                content = Chunk(ToolContent.text(Option(defect.getMessage).getOrElse(defect.toString))),
                isError = Some(true),
              )
              resultPromise.succeed(errorResult)
            .ensuring(queue.shutdown)
          runTool.fork.as:
            sseToolCallResponse(id, queue, resultPromise)

  private def statelessHandleToolsCall(
    id: RequestId,
    params: Option[Json.Obj],
  ): ZIO[R, Response, Response] =
    resolveToolCall(id, params).flatMap: (tool, callParams) =>
      tool.callWithContext(callParams.arguments, McpToolContext.noop)
        .catchAllDefect: defect =>
          ZIO.succeed(CallToolResult(
            content = Chunk(ToolContent.text(Option(defect.getMessage).getOrElse(defect.toString))),
            isError = Some(true),
          ))
        .flatMap(result => jsonRpcResponse(id, result))

  private def handleResourcesList(id: RequestId): ZIO[Any, Response, Response] =
    jsonRpcResponse(id, ResourcesListResult(resources = resources.map(_.definition)))

  private def handleResourceTemplatesList(id: RequestId): ZIO[Any, Response, Response] =
    jsonRpcResponse(id, ResourceTemplatesListResult(resourceTemplates = resourceTemplates.map(_.definition)))

  private def handleResourceRead(id: RequestId, params: Option[Json.Obj]): ZIO[Any, Response, Response] =
    val paramsJson = params.getOrElse(Json.Obj()).toJson
    ZIO.fromEither(paramsJson.fromJson[ResourceReadParams])
      .mapError(e => jsonRpcErrorResponse(Some(id), ErrorCode.InvalidParams, s"Invalid resource read params: $e"))
      .flatMap: readParams =>
        val uri = readParams.uri
        val directMatch = resources.find(_.definition.uri == uri)
        val handler: Option[String => ZIO[Any, ToolError, Chunk[ResourceContents]]] =
          directMatch.map(r => r.read)
            .orElse(resourceTemplates.find(matchesTemplate(_, uri)).map(_.read))

        handler match
          case None =>
            ZIO.fail(jsonRpcErrorResponse(Some(id), ErrorCode.ResourceNotFound, s"Resource not found: $uri"))
          case Some(readFn) =>
            readFn(uri).foldZIO(
              err => ZIO.fail(jsonRpcErrorResponse(Some(id), ErrorCode.InternalError, err.message)),
              contents => jsonRpcResponse(id, ResourceReadResult(contents = contents)),
            )

  private def matchesTemplate(tmpl: McpResourceTemplateHandler, uri: String): Boolean =
    val pattern = tmpl.definition.uriTemplate
    val regex = pattern.replaceAll("\\{[^}]+}", "([^/]+)")
    uri.matches(regex)

  private def handlePromptsList(id: RequestId): ZIO[Any, Response, Response] =
    jsonRpcResponse(id, PromptsListResult(prompts = prompts.map(_.definition)))

  private def handlePromptsGet(id: RequestId, params: Option[Json.Obj]): ZIO[Any, Response, Response] =
    val paramsJson = params.getOrElse(Json.Obj()).toJson
    ZIO.fromEither(paramsJson.fromJson[PromptGetParams])
      .mapError(e => jsonRpcErrorResponse(Some(id), ErrorCode.InvalidParams, s"Invalid prompt get params: $e"))
      .flatMap: getParams =>
        promptsByName.get(getParams.name) match
          case None =>
            ZIO.fail(jsonRpcErrorResponse(Some(id), ErrorCode.InvalidParams, s"Unknown prompt: ${getParams.name.value}"))
          case Some(prompt) =>
            prompt.get(getParams.arguments.getOrElse(Map.empty)).foldZIO(
              err => ZIO.fail(jsonRpcErrorResponse(Some(id), ErrorCode.InternalError, err.message)),
              result => jsonRpcResponse(id, result),
            )

  private def handleCompletionComplete(id: RequestId, params: Option[Json.Obj]): ZIO[Any, Response, Response] =
    jsonRpcResponse(id, CompletionResult(completion = CompletionValues(values = Chunk.empty)))

  private def withSession[R0](request: Request, sessions: Ref[Map[SessionId, SessionState]])(
    effect: ZIO[R0, Response, Response]
  ): ZIO[R0, Response, Response] =
    val sessionId = request.rawHeader("mcp-session-id").map(SessionId(_))
    sessionId match
      case None =>
        ZIO.fail(Response.status(Status.BadRequest))
      case Some(sid) =>
        sessions.get.flatMap: m =>
          m.get(sid) match
            case None =>
              ZIO.fail(Response.status(Status.NotFound))
            case Some(_) =>
              effect

  private def getHandler(request: Request): ZIO[McpServer.State, Response, Response] =
    for
      _     <- validateOrigin(request)
      state <- ZIO.service[McpServer.State]
      _     <- withSession(request, state.sessions)(ZIO.succeed(Response.ok))
    yield
      Response(
        status = Status.Ok,
        headers = Headers(
          Header.ContentType(MediaType.text.`event-stream`),
          Header.CacheControl.NoCache,
        ),
        body = Body.fromCharSequenceStreamChunked(
          ZStream.tick(30.seconds).as(": keepalive\n\n")
        ),
      )

  private def deleteHandler(request: Request): ZIO[McpServer.State, Response, Response] =
    for
      _     <- validateOrigin(request)
      state <- ZIO.service[McpServer.State]
      _     <- request.rawHeader("mcp-session-id").map(SessionId(_)) match
        case Some(sid) => state.sessions.update(_ - sid)
        case None      => ZIO.unit
    yield Response.ok

  // --- SSE response for tool calls ---

  private def sseToolCallResponse(
    id: RequestId,
    queue: Queue[JsonRpcMessage],
    resultPromise: Promise[Nothing, CallToolResult],
  ): Response =
    val messageStream = ZStream.fromQueue(queue).map: msg =>
      sseEvent(msg.toJson)

    val resultStream = ZStream.fromZIO(resultPromise.await).map: result =>
      val json = result.toJsonAST.getOrElse(
        CallToolResult(content = Chunk(ToolContent.text("Internal error: failed to encode result")), isError = Some(true))
          .toJsonAST.getOrElse(Json.Obj())
      )
      sseEvent(JsonRpcResponse(id, json).toJson)

    val keepalive = ZStream.tick(30.seconds).as(": keepalive\n\n")

    val stream = (messageStream ++ resultStream).merge(keepalive)

    Response(
      status = Status.Ok,
      headers = Headers(
        Header.ContentType(MediaType.text.`event-stream`),
        Header.CacheControl.NoCache,
      ),
      body = Body.fromCharSequenceStreamChunked(stream),
    )

  private def sseEvent(json: String): String =
    s"event: message\ndata: $json\n\n"

  // --- Response helpers ---

  private def jsonRpcResponse[A: JsonEncoder](id: RequestId, result: A): ZIO[Any, Response, Response] =
    ZIO.fromEither(result.toJsonAST)
      .mapError(e => jsonRpcErrorResponse(None, ErrorCode.InternalError, s"JSON encoding failed: $e"))
      .map(json => Response.json(JsonRpcResponse(id, json).toJson))

  private def jsonRpcErrorResponse(id: Option[RequestId], code: ErrorCode, message: String): Response =
    val e = JsonRpcError.fromCode(id, code, message)
    Response.json(e.toJson)

  private def badRequest(message: String): Response =
    Response.json(
      JsonRpcError(None, ErrorDetail(ErrorCode.ParseError.code, message)).toJson
    ).status(Status.BadRequest)

object McpServer:
  def apply(name: String, version: String): McpServer[Any] =
    new McpServer(Implementation(name, version), Chunk.empty, Chunk.empty, Chunk.empty, Chunk.empty)

  trait State:
    def sessions: Ref[Map[SessionId, SessionState]]
    def pendingRequests: Ref[Map[RequestId, Promise[Nothing, Json]]]

  object State:
    val default: ULayer[State] = ZLayer.fromZIO:
      for
        s <- Ref.make(Map.empty[SessionId, SessionState])
        p <- Ref.make(Map.empty[RequestId, Promise[Nothing, Json]])
      yield new State:
        val sessions = s
        val pendingRequests = p

  private val localhostPatterns = Set("localhost", "127.0.0.1", "[::1]", "::1")

  private[mcp] def isLocalhostHost(hostWithPort: String): Boolean =
    val host = hostWithPort.split(':').head
    localhostPatterns.contains(host)
