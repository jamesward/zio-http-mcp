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

  private val toolsByName: Map[String, McpToolHandlerR[R]] =
    tools.map(t => t.name.value -> t).toMap

  private val promptsByName: Map[String, McpPromptHandler] =
    prompts.map(p => p.definition.name -> p).toMap

  private val serverCapabilities: ServerCapabilities =
    ServerCapabilities(
      tools = if tools.nonEmpty then Some(Json.Obj()) else None,
      resources = if resources.nonEmpty || resourceTemplates.nonEmpty then Some(Json.Obj(Chunk("subscribe" -> Json.Bool(true)))) else None,
      prompts = if prompts.nonEmpty then Some(Json.Obj()) else None,
      logging = Some(Json.Obj()),
      completions = Some(Json.Obj()),
    )

  def routes: Routes[R, Response] =
    Routes(
      Method.POST / "mcp" -> handler(postHandler),
      Method.GET / "mcp"  -> handler(getHandler),
      Method.DELETE / "mcp" -> handler(deleteHandler),
    ).sandbox

  private def validateOrigin(request: Request): ZIO[Any, Response, Unit] =
    request.rawHeader("origin") match
      case Some(o) =>
        val originHost = o.replaceFirst("^https?://", "").toLowerCase
        if McpServer.isLocalhostHost(originHost) then ZIO.unit
        else ZIO.fail(Response.status(Status.Forbidden))
      case None =>
        ZIO.unit

  private def postHandler(request: Request): ZIO[R, Response, Response] =
    for
      _              <- validateOrigin(request)
      sessions       <- McpServer.sessions
      pendingReqs    <- McpServer.pendingRequests
      body           <- request.body.asString.mapError(_ => badRequest("Failed to read request body"))
      bodyJson       <- ZIO.fromEither(body.fromJson[Json.Obj])
                          .mapError(e => jsonRpcErrorResponse(None, ErrorCode.ParseError, s"Parse error: $e"))
      response       <- routeMessage(request, sessions, pendingReqs, bodyJson)
    yield response

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
      // This is a JSON-RPC response from the client (replying to a server request)
      handleClientResponse(pendingReqs, bodyJson)
    else
      // Parse as a normal JSON-RPC message (request or notification)
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
      case "ping" =>
        withSession(request, sessions):
          ZIO.succeed(jsonRpcResponse(id, Json.Obj()))
      case "tools/list" =>
        withSession(request, sessions):
          handleToolsList(id, params)
      case "tools/call" =>
        withSession(request, sessions):
          handleToolsCall(id, params, pendingReqs)
      case "resources/list" =>
        withSession(request, sessions):
          handleResourcesList(id)
      case "resources/templates/list" =>
        withSession(request, sessions):
          handleResourceTemplatesList(id)
      case "resources/read" =>
        withSession(request, sessions):
          handleResourceRead(id, params)
      case "resources/subscribe" =>
        withSession(request, sessions):
          ZIO.succeed(jsonRpcResponse(id, Json.Obj()))
      case "resources/unsubscribe" =>
        withSession(request, sessions):
          ZIO.succeed(jsonRpcResponse(id, Json.Obj()))
      case "prompts/list" =>
        withSession(request, sessions):
          handlePromptsList(id)
      case "prompts/get" =>
        withSession(request, sessions):
          handlePromptsGet(id, params)
      case "logging/setLevel" =>
        withSession(request, sessions):
          ZIO.succeed(jsonRpcResponse(id, Json.Obj()))
      case "completion/complete" =>
        withSession(request, sessions):
          handleCompletionComplete(id, params)
      case _ =>
        ZIO.fail(jsonRpcErrorResponse(Some(id), ErrorCode.MethodNotFound, s"Method not found: $method"))

  private def handleNotification(
    request: Request,
    sessions: Ref[Map[SessionId, SessionState]],
    method: String,
    params: Option[Json.Obj],
  ): ZIO[Any, Response, Response] =
    method match
      case "notifications/initialized" =>
        val sessionId = request.rawHeader("mcp-session-id").map(SessionId(_))
        sessionId match
          case Some(sid) =>
            sessions.update(_.updatedWith(sid):
              case Some(_) => Some(SessionState.Active)
              case None    => None
            ) *> ZIO.succeed(Response.ok)
          case None =>
            ZIO.succeed(Response.ok)
      case "notifications/cancelled" =>
        ZIO.succeed(Response.ok)
      case _ =>
        ZIO.succeed(Response.ok)

  private def handleInitialize(
    sessions: Ref[Map[SessionId, SessionState]],
    id: RequestId,
    params: Option[Json.Obj],
  ): ZIO[Any, Response, Response] =
    val paramsJson = params.getOrElse(Json.Obj()).toJson
    ZIO.fromEither(paramsJson.fromJson[InitializeParams])
      .mapError(e => jsonRpcErrorResponse(Some(id), ErrorCode.InvalidParams, s"Invalid initialize params: $e"))
      .flatMap: initParams =>
        val sessionId = SessionId.generate
        val result = InitializeResult(
          protocolVersion = McpProtocol.Version,
          capabilities = serverCapabilities,
          serverInfo = serverInfo,
        )
        sessions.update(_.updated(sessionId, SessionState.Initializing)).as:
          jsonRpcResponse(id, result.toJsonAST.toOption.get)
            .addHeader("mcp-session-id", sessionId.value)

  private def handleToolsList(
    id: RequestId,
    params: Option[Json.Obj],
  ): ZIO[Any, Nothing, Response] =
    val toolDefs = tools.map(_.definition)
    val result = ToolsListResult(tools = toolDefs)
    ZIO.succeed(jsonRpcResponse(id, result.toJsonAST.toOption.get))

  private def handleToolsCall(
    id: RequestId,
    params: Option[Json.Obj],
    pendingReqs: Ref[Map[RequestId, Promise[Nothing, Json]]],
  ): ZIO[R, Response, Response] =
    val paramsJson = params.getOrElse(Json.Obj()).toJson
    ZIO.fromEither(paramsJson.fromJson[ToolCallParams])
      .mapError(e => jsonRpcErrorResponse(Some(id), ErrorCode.InvalidParams, s"Invalid tool call params: $e"))
      .flatMap: callParams =>
        toolsByName.get(callParams.name) match
          case None =>
            ZIO.fail(jsonRpcErrorResponse(Some(id), ErrorCode.InvalidParams, s"Unknown tool: ${callParams.name}"))
          case Some(tool) =>
            val progressToken = params.flatMap(_.get("_meta")).flatMap(_.asObject).flatMap(_.get("progressToken"))
            Queue.unbounded[JsonRpcMessage].flatMap: queue =>
              val ctx = McpToolContext.make(queue, pendingReqs, progressToken)
              val toolEffect = tool.callWithContext(callParams.arguments, ctx)

              // Fork the tool, stream messages + result as SSE
              Promise.make[Nothing, CallToolResult].flatMap: resultPromise =>
                val runTool = toolEffect.flatMap(resultPromise.succeed).ensuring(queue.shutdown)
                runTool.fork.as:
                  sseToolCallResponse(id, queue, resultPromise)

  private def handleResourcesList(id: RequestId): ZIO[Any, Nothing, Response] =
    val result = ResourcesListResult(resources = resources.map(_.definition))
    ZIO.succeed(jsonRpcResponse(id, result.toJsonAST.toOption.get))

  private def handleResourceTemplatesList(id: RequestId): ZIO[Any, Nothing, Response] =
    val result = ResourceTemplatesListResult(resourceTemplates = resourceTemplates.map(_.definition))
    ZIO.succeed(jsonRpcResponse(id, result.toJsonAST.toOption.get))

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
            readFn(uri).fold(
              err => jsonRpcErrorResponse(Some(id), ErrorCode.InternalError, err.message),
              contents =>
                val result = ResourceReadResult(contents = contents)
                jsonRpcResponse(id, result.toJsonAST.toOption.get)
            )

  private def matchesTemplate(tmpl: McpResourceTemplateHandler, uri: String): Boolean =
    val pattern = tmpl.definition.uriTemplate
    val regex = pattern.replaceAll("\\{[^}]+\\}", "([^/]+)")
    uri.matches(regex)

  private def handlePromptsList(id: RequestId): ZIO[Any, Nothing, Response] =
    val result = PromptsListResult(prompts = prompts.map(_.definition))
    ZIO.succeed(jsonRpcResponse(id, result.toJsonAST.toOption.get))

  private def handlePromptsGet(id: RequestId, params: Option[Json.Obj]): ZIO[Any, Response, Response] =
    val paramsJson = params.getOrElse(Json.Obj()).toJson
    ZIO.fromEither(paramsJson.fromJson[PromptGetParams])
      .mapError(e => jsonRpcErrorResponse(Some(id), ErrorCode.InvalidParams, s"Invalid prompt get params: $e"))
      .flatMap: getParams =>
        promptsByName.get(getParams.name) match
          case None =>
            ZIO.fail(jsonRpcErrorResponse(Some(id), ErrorCode.InvalidParams, s"Unknown prompt: ${getParams.name}"))
          case Some(prompt) =>
            prompt.get(getParams.arguments.getOrElse(Map.empty)).fold(
              err => jsonRpcErrorResponse(Some(id), ErrorCode.InternalError, err.message),
              result => jsonRpcResponse(id, result.toJsonAST.toOption.get)
            )

  private def handleCompletionComplete(id: RequestId, params: Option[Json.Obj]): ZIO[Any, Response, Response] =
    val result = CompletionResult(completion = CompletionValues(values = Chunk.empty))
    ZIO.succeed(jsonRpcResponse(id, result.toJsonAST.toOption.get))

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

  private def getHandler(request: Request): ZIO[Any, Response, Response] =
    for
      _        <- validateOrigin(request)
      sessions <- McpServer.sessions
      _        <- withSession(request, sessions)(ZIO.succeed(Response.ok))
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

  private def deleteHandler(request: Request): ZIO[Any, Response, Response] =
    for
      _        <- validateOrigin(request)
      sessions <- McpServer.sessions
      _        <- request.rawHeader("mcp-session-id").map(SessionId(_)) match
        case Some(sid) => sessions.update(_ - sid)
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
      val r = JsonRpcResponse(id, result.toJsonAST.toOption.get)
      sseEvent(r.toJson)

    val stream = messageStream ++ resultStream

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

  private def jsonRpcResponse(id: RequestId, result: Json): Response =
    val r = JsonRpcResponse(id, result)
    Response.json(r.toJson)

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

  private val localhostPatterns = Set("localhost", "127.0.0.1", "[::1]", "::1")

  private[mcp] def isLocalhostHost(hostWithPort: String): Boolean =
    val host = hostWithPort.split(':').head
    localhostPatterns.contains(host)

  private val sessionsRef: UIO[Ref[Map[SessionId, SessionState]]] =
    Ref.make(Map.empty[SessionId, SessionState])

  private lazy val sessionsMemo: Ref[Map[SessionId, SessionState]] =
    Unsafe.unsafe(implicit u => Runtime.default.unsafe.run(sessionsRef).getOrThrow())

  private[mcp] def sessions: UIO[Ref[Map[SessionId, SessionState]]] =
    ZIO.succeed(sessionsMemo)

  private val pendingRequestsRef: UIO[Ref[Map[RequestId, Promise[Nothing, Json]]]] =
    Ref.make(Map.empty[RequestId, Promise[Nothing, Json]])

  private lazy val pendingRequestsMemo: Ref[Map[RequestId, Promise[Nothing, Json]]] =
    Unsafe.unsafe(implicit u => Runtime.default.unsafe.run(pendingRequestsRef).getOrThrow())

  private[mcp] def pendingRequests: UIO[Ref[Map[RequestId, Promise[Nothing, Json]]]] =
    ZIO.succeed(pendingRequestsMemo)
