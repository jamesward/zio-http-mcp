package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.auth.{AuthError, AuthMiddleware, McpAuth, OauthScope, Principal, ProtectedResourceMetadata, ResourceUriResolver}
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
  val authConfig: Option[McpAuth[R]] = None,
  val mountPath: String = "/mcp",
  toolSrc: Option[McpToolSource[R]] = None,
  resourceSrc: Option[McpResourceSource[R]] = None,
  pathParamName: Option[String] = None,
  val instructions: Option[String] = None,
  instructionsSrc: Option[InstructionsSource[R]] = None,
):
  def tool[R1](t: McpToolHandlerR[R1]): McpServer[R & R1] =
    new McpServer(serverInfo, tools :+ t, resources, resourceTemplates, prompts, authConfig, mountPath, toolSrc, resourceSrc, pathParamName, instructions, instructionsSrc)

  def resource(r: McpResourceHandler): McpServer[R] =
    new McpServer(serverInfo, tools, resources :+ r, resourceTemplates, prompts, authConfig, mountPath, toolSrc, resourceSrc, pathParamName, instructions, instructionsSrc)

  def resourceTemplate(rt: McpResourceTemplateHandler): McpServer[R] =
    new McpServer(serverInfo, tools, resources, resourceTemplates :+ rt, prompts, authConfig, mountPath, toolSrc, resourceSrc, pathParamName, instructions, instructionsSrc)

  def prompt(p: McpPromptHandler): McpServer[R] =
    new McpServer(serverInfo, tools, resources, resourceTemplates, prompts :+ p, authConfig, mountPath, toolSrc, resourceSrc, pathParamName, instructions, instructionsSrc)

  /**
   * Register a dynamic [[McpToolSource]] consulted at request time. Its tools are merged
   * into `tools/list` (after the static tools), and a `tools/call` for a name no static
   * `.tool(...)` matched falls through to the source. Contravariant in the source's
   * environment, like [[tool]].
   */
  def toolSource[R1](src: McpToolSource[R1]): McpServer[R & R1] =
    new McpServer(serverInfo, tools, resources, resourceTemplates, prompts, authConfig, mountPath, Some(src), resourceSrc, pathParamName, instructions, instructionsSrc)

  /**
   * Register a dynamic [[McpResourceSource]] consulted at request time. Its resources and
   * templates merge into the corresponding list method, `resources/read` falls through to
   * it when no static resource/template matched, and `completion/complete` delegates to it.
   */
  def resourceSource[R1](src: McpResourceSource[R1]): McpServer[R & R1] =
    new McpServer(serverInfo, tools, resources, resourceTemplates, prompts, authConfig, mountPath, toolSrc, Some(src), pathParamName, instructions, instructionsSrc)

  /**
   * Mount the MCP HTTP routes at the given path. Defaults to `/mcp` (matching the
   * convention recommended by the MCP specification). Use `/` to mount at the root
   * — useful when the server only handles MCP and the path prefix is redundant.
   *
   * The same path is the source of truth for:
   *   - where `routes` / `statelessRoutes` register POST/GET/DELETE handlers,
   *   - the resource URI advertised in the RFC 9728 Protected Resource Metadata
   *     document (when the [[com.jamesward.ziohttp.mcp.auth.McpAuth]] config has no
   *     explicit `resourceUri` and the URI is derived from forwarded / host headers),
   *   - the audience the auth middleware checks tokens against.
   *
   * Setting it once here keeps those three uses from drifting apart.
   *
   * Multi-segment paths like `/api/v1/mcp` are supported. A leading slash is
   * implicit; `mountedAt("api/v1/mcp")` and `mountedAt("/api/v1/mcp")` are
   * equivalent.
   *
   * Mutually exclusive with [[mountedAtParam]]; the last one called wins.
   */
  def mountedAt(path: String): McpServer[R] =
    new McpServer(serverInfo, tools, resources, resourceTemplates, prompts, authConfig, path, toolSrc, resourceSrc, None, instructions, instructionsSrc)

  /**
   * Mount at a single path-parameter segment, so the server serves `/<value>` for any
   * value of the named parameter (e.g. one server serving `/<slug>` for every toolbook).
   * The captured segment is exposed to dynamic sources via `ctx.pathParams(paramName)`,
   * so a single [[toolSource]] / [[resourceSource]] can switch behavior per mount.
   *
   * The parameter captures exactly one segment at the server root. Auth-derived resource
   * URIs (when [[com.jamesward.ziohttp.mcp.auth.McpAuth.resourceUri]] is not set) resolve
   * to the host root rather than the per-value path, keeping the audience host-wide.
   *
   * Mutually exclusive with [[mountedAt]]; the last one called wins.
   */
  def mountedAtParam(paramName: String): McpServer[R] =
    new McpServer(serverInfo, tools, resources, resourceTemplates, prompts, authConfig, mountPath, toolSrc, resourceSrc, Some(paramName), instructions, instructionsSrc)

  /**
   * Enable opt-in OAuth 2.1 authorization for this server.
   *
   * When set:
   *   - `/.well-known/oauth-protected-resource` (and the path-suffixed form) serve an
   *     RFC 9728 Protected Resource Metadata document.
   *   - All `/mcp` requests require a valid bearer token.
   *   - Token verifier failures yield 401/403 with `WWW-Authenticate` challenges.
   *   - Tool handlers can read the [[com.jamesward.ziohttp.mcp.auth.Principal]] via
   *     `ctx.principal`.
   *
   * @see [[com.jamesward.ziohttp.mcp.auth.McpAuth]]
   */
  def auth[R1](a: McpAuth[R1]): McpServer[R & R1] =
    new McpServer[R & R1](serverInfo, tools, resources, resourceTemplates, prompts, Some(a), mountPath, toolSrc, resourceSrc, pathParamName, instructions, instructionsSrc)

  /**
   * Set a static `instructions` string returned in the `initialize` result.
   *
   * Per the MCP specification this is a hint to the client (and, transitively, to
   * the LLM) describing how to use the server and its capabilities — analogous to a
   * system prompt. Clients may surface it to the model. Unset by default.
   *
   * For instructions that vary per caller or per mount, pass an [[InstructionsSource]]
   * to the overload instead. The two forms are mutually exclusive — calling either
   * clears the other, last one wins — because `instructions` is a single value rather
   * than a combinable collection like tools/resources.
   */
  def instructions(text: String): McpServer[R] =
    new McpServer(serverInfo, tools, resources, resourceTemplates, prompts, authConfig, mountPath, toolSrc, resourceSrc, pathParamName, Some(text), None)

  /**
   * Set a dynamic [[InstructionsSource]] for the `initialize` result's `instructions`
   * string, consulted on every `initialize` with the request's [[McpToolContext]] in
   * hand — so the instructions can vary per caller (`ctx.principal`) and per mount
   * (`ctx.pathParams`). The dynamic analogue of the `String` overload.
   *
   * Mutually exclusive with the static `String` overload (last one wins). Contravariant
   * in the provider's environment, like [[tool]] / [[toolSource]]: registering one
   * widens the server's `R`.
   */
  def instructions[R1](source: InstructionsSource[R1]): McpServer[R & R1] =
    new McpServer[R & R1](serverInfo, tools, resources, resourceTemplates, prompts, authConfig, mountPath, toolSrc, resourceSrc, pathParamName, None, Some(source))

  private val toolsByName: Map[ToolName, McpToolHandlerR[R]] =
    tools.map(t => t.name -> t).toMap

  private val promptsByName: Map[PromptName, McpPromptHandler] =
    prompts.map(p => p.definition.name -> p).toMap

  private val serverCapabilities: ServerCapabilities =
    // Advertise the 2026-07-28 Tasks extension alongside any dynamic-source
    // extensions. The stateful `routes` fulfil it (they carry the task store).
    val extensionMap: Map[String, Json] =
      Map(TaskRecord.ExtensionId -> (Json.Obj(): Json)) ++ resourceSrc.map(_.capabilities).getOrElse(Map.empty)
    ServerCapabilities(
      tools = if tools.nonEmpty || toolSrc.isDefined then Some(Json.Obj()) else None,
      resources = if resources.nonEmpty || resourceTemplates.nonEmpty || resourceSrc.isDefined then Some(Json.Obj(Chunk("subscribe" -> Json.Bool(true)))) else None,
      prompts = if prompts.nonEmpty then Some(Json.Obj()) else None,
      logging = Some(Json.Obj()),
      completions = Some(Json.Obj()),
      extensions = if extensionMap.nonEmpty then Some(Json.Obj(Chunk.fromIterable(extensionMap.toSeq.map((k, v) => k -> (v: Json))))) else None,
    )

  def routes: Routes[R & McpServer.State, Response] =
    val baseRoutes: Routes[R & McpServer.State, Response] = pathParamName match
      case Some(name) =>
        val seg = zio.http.codec.PathCodec.string(name)
        Routes(
          Method.POST   / seg -> handler((slug: String, req: Request) => postHandler(Map(name -> slug), req)),
          Method.GET    / seg -> handler((slug: String, req: Request) => getHandler(Map(name -> slug), req)),
          Method.DELETE / seg -> handler((slug: String, req: Request) => deleteHandler(Map(name -> slug), req)),
        )
      case None =>
        Routes(
          Method.POST   / mountPathCodec -> handler((req: Request) => postHandler(Map.empty, req)),
          Method.GET    / mountPathCodec -> handler((req: Request) => getHandler(Map.empty, req)),
          Method.DELETE / mountPathCodec -> handler((req: Request) => deleteHandler(Map.empty, req)),
        )
    val mcpRoutes =
      if isRootMount || pathParamName.isDefined then baseRoutes
      else baseRoutes ++ Routes(Method.GET / mountPathCodec / trailing -> Handler.notFound)
    (prmRoutes ++ mcpRoutes).sandbox

  def statelessRoutes: Routes[R, Response] =
    val notAllowed = ZIO.succeed(Response.status(Status.MethodNotAllowed))
    val baseRoutes: Routes[R, Response] = pathParamName match
      case Some(name) =>
        val seg = zio.http.codec.PathCodec.string(name)
        Routes(
          Method.POST   / seg -> handler((slug: String, req: Request) => statelessPostHandler(Map(name -> slug), req)),
          Method.GET    / seg -> handler((_: String, _: Request) => notAllowed),
          Method.DELETE / seg -> handler((_: String, _: Request) => notAllowed),
        )
      case None =>
        Routes(
          Method.POST   / mountPathCodec -> handler((req: Request) => statelessPostHandler(Map.empty, req)),
          Method.GET    / mountPathCodec -> handler((_: Request) => notAllowed),
          Method.DELETE / mountPathCodec -> handler((_: Request) => notAllowed),
        )
    val mcpRoutes =
      if isRootMount || pathParamName.isDefined then baseRoutes
      else baseRoutes ++ Routes(Method.GET / mountPathCodec / trailing -> Handler.notFound)
    (prmRoutes ++ mcpRoutes).sandbox

  /** PathCodec for the mount point. `PathCodec.apply` parses leading/trailing
    * slashes the way you'd expect: `"/mcp"`, `"mcp"`, `"/mcp/"` all produce
    * a single-segment codec; `"/"` and `""` produce the empty codec. */
  private def mountPathCodec: zio.http.codec.PathCodec[Unit] =
    zio.http.codec.PathCodec(mountPath)

  /** True when the configured mount path is the server root (no segments).
    * In that case, the trailing-notFound handler is skipped because it
    * would otherwise catch every GET path including the `/.well-known/...`
    * PRM endpoints. Composing apps mounting their own GET routes get
    * default 404 behavior just like any other unmatched path. */
  private def isRootMount: Boolean =
    mountPath.split('/').forall(_.isEmpty)

  /** Path component used to derive the auth resource URI when no explicit one is set.
    * For a parameterised mount the per-value segment is omitted so the advertised
    * resource URI / audience stays host-wide rather than per-`<value>`. */
  private def authResourcePath: String =
    if pathParamName.isDefined then "" else mountPath

  /**
   * Routes that serve the RFC 9728 Protected Resource Metadata document at both
   * `/.well-known/oauth-protected-resource` and `/.well-known/oauth-protected-resource/<path>`
   * when [[auth]] is configured. The trailing-path form serves the same document at any
   * sub-path so it always matches the URL we publish in the `WWW-Authenticate` header,
   * regardless of where the MCP endpoint is mounted.
   *
   * Empty when auth is not configured.
   */
  private def prmRoutes: Routes[Any, Response] =
    authConfig match
      case None => Routes.empty
      case Some(a) =>
        def respondPRM(request: Request): UIO[Response] =
          val resourceUri = ResourceUriResolver.resolve(a.resourceUri, authResourcePath, request)
          val prm = ProtectedResourceMetadata.fromAuth(a, resourceUri)
          ZIO.log(s"PRM document requested: ${request.url.encode} (resource=${resourceUri.value})")
            .as(Response.json(prm.toJson).addHeader(Header.CacheControl.MaxAge(3600)))

        val handlerFn = handler { (req: Request) => respondPRM(req) }
        val trailingHandler = handler { (_: zio.http.Path, req: Request) => respondPRM(req) }
        Routes(
          Method.GET / ".well-known" / "oauth-protected-resource" -> handlerFn,
          Method.GET / ".well-known" / "oauth-protected-resource" / trailing -> trailingHandler,
        )

  /**
   * Run the auth middleware if [[auth]] is configured, otherwise yield `None`.
   *
   * On auth failure, fails with a `Response` carrying the appropriate 401/403/503 + `WWW-Authenticate`.
   */
  private def authenticate(request: Request): ZIO[R, Response, Option[Principal]] =
    authConfig match
      case None => ZIO.succeed(None)
      case Some(a) =>
        AuthMiddleware
          .authenticate(a, authResourcePath, request, additionalRequiredScopes = Set.empty)
          .tapBoth(
            err => ZIO.logWarning(s"Auth failed for ${request.method} ${request.url.encode}: ${err.description}"),
            principal => ZIO.logInfo(s"Auth ok for ${request.method} ${request.url.encode}: sub=${principal.subject.getOrElse("?")} client_id=${principal.clientId.getOrElse("?")} scopes=${principal.scopes.map(_.value).mkString(",")}"),
          )
          .mapBoth(
            err => AuthMiddleware.errorResponse(a, authResourcePath, request, err, a.requiredScopes),
            principal => Some(principal),
          )

  /**
   * Per-tool scope check. Called after a tool is resolved but before its handler runs.
   * Fails with a 403 response if the principal lacks required scopes.
   */
  private def enforceToolScopes(
    request: Request,
    principal: Option[Principal],
    tool: McpToolHandlerR[R],
  ): ZIO[Any, Response, Unit] =
    (authConfig, principal) match
      case (Some(a), Some(p)) if !p.hasAllScopes(tool.requiredScopes) =>
        val combined = a.requiredScopes ++ tool.requiredScopes
        val err = AuthError.InsufficientScope(combined, p.scopes)
        ZIO.logWarning(s"Per-tool scope check failed for ${tool.name.value}: required=${combined.map(_.value).mkString(",")} actual=${p.scopes.map(_.value).mkString(",")}") *>
          ZIO.fail(AuthMiddleware.errorResponse(a, authResourcePath, request, err, combined))
      case _ =>
        ZIO.unit

  private def validateOrigin(request: Request): ZIO[Any, Response, Unit] =
    request.rawHeader("origin") match
      case Some(o) =>
        val originHost = o.replaceFirst("^https?://", "").toLowerCase
        ZIO.unless(McpServer.isLocalhostHost(originHost))(ZIO.fail(Response.status(Status.Forbidden))).unit
      case None =>
        ZIO.unit

  private def postHandler(pathParams: Map[String, String], request: Request): ZIO[R & McpServer.State, Response, Response] =
    for
      _         <- validateOrigin(request)
      principal <- authenticate(request)
      state     <- ZIO.service[McpServer.State]
      body      <- request.body.asString.orElseFail(badRequest("Failed to read request body"))
      bodyJson  <- ZIO.fromEither(body.fromJson[Json.Obj])
                     .mapError(e => jsonRpcErrorResponse(None, ErrorCode.ParseError, s"Parse error: $e"))
      response  <- routeMessage(request, state.sessions, state.pendingRequests, state.tasks, bodyJson, principal, pathParams)
    yield response

  private def statelessPostHandler(pathParams: Map[String, String], request: Request): ZIO[R, Response, Response] =
    for
      _         <- validateOrigin(request)
      principal <- authenticate(request)
      body      <- request.body.asString.orElseFail(badRequest("Failed to read request body"))
      bodyJson  <- ZIO.fromEither(body.fromJson[Json.Obj])
                     .mapError(e => jsonRpcErrorResponse(None, ErrorCode.ParseError, s"Parse error: $e"))
      message   <- ZIO.fromEither(bodyJson.toJson.fromJson[JsonRpcMessage])
                     .mapError(e => jsonRpcErrorResponse(None, ErrorCode.ParseError, s"Parse error: $e"))
      response  <- message match
        case JsonRpcMessage.Request(id, method, params) =>
          negotiateEra(request, Some(id), method, params) match
            case Left(resp) =>
              ZIO.succeed(resp)
            case Right(ProtocolEra.Modern(version)) =>
              // The stateless routes have no task store, so tasks are unavailable here.
              dispatchModern(request, id, method, version, params, principal, pathParams, tasks = None)
            case Right(ProtocolEra.Legacy) =>
              method match
                case "initialize" =>
                  parseInitializeParams(id, params, principal, pathParams).flatMap(r => jsonRpcResponse(id, r))
                case _ =>
                  McpDispatchMethod.parse(method) match
                    case Some(dm) =>
                      dispatchMethod(id, dm, ProtocolVersion.default, params, principal, pathParams,
                        statelessHandleToolsCall(request, ProtocolVersion.default, _, _, principal, pathParams))
                    case None =>
                      ZIO.fail(jsonRpcErrorResponse(Some(id), ErrorCode.MethodNotFound, s"Method not found: $method"))
        case JsonRpcMessage.Notification(method, params) =>
          ZIO.log(s"MCP Notification: $method $params").as(Response.status(Status.Accepted))
    yield response

  /**
   * Classify a Streamable HTTP request and, for modern requests, validate its
   * routing headers and resolve the protocol version. Returns the negotiated
   * [[ProtocolEra]], or a ready-made `400 Bad Request` response on a modern
   * header/version failure.
   */
  private def negotiateEra(
    request: Request,
    id: Option[RequestId],
    method: String,
    params: Option[Json.Obj],
  ): Either[Response, ProtocolEra] =
    val protoHeader = request.rawHeader(Negotiation.ProtocolVersionHeader)
    if Negotiation.isModernRequest(method, params, protoHeader) then
      val methodHeader = request.rawHeader(Negotiation.MethodHeader)
      val nameHeader   = request.rawHeader(Negotiation.NameHeader)
      Negotiation.resolveModern(method, params, protoHeader, methodHeader, nameHeader) match
        case Left(err)      => Left(negotiationErrorResponse(id, err))
        case Right(version) => Right(ProtocolEra.Modern(version))
    else
      Right(ProtocolEra.Legacy)

  /**
   * Dispatch a modern (2026-07-28+) request statelessly: no session, no
   * `initialize`. Unknown methods yield `404 Not Found` with `-32601`; tool
   * calls are answered with a single augmented JSON result.
   */
  private def dispatchModern(
    request: Request,
    id: RequestId,
    method: String,
    version: ProtocolVersion,
    params: Option[Json.Obj],
    principal: Option[Principal],
    pathParams: Map[String, String],
    tasks: Option[Ref[Map[TaskId, TaskRecord]]],
  ): ZIO[R, Response, Response] =
    McpDispatchMethod.parse(method) match
      case Some(McpDispatchMethod.TasksGet)    => handleTasksGet(id, params, tasks)
      case Some(McpDispatchMethod.TasksCancel) => handleTasksCancel(id, params, tasks)
      case Some(McpDispatchMethod.TasksUpdate) => handleTasksUpdate(id, params, tasks)
      case Some(dm) =>
        dispatchMethod(id, dm, version, params, principal, pathParams,
          modernHandleToolsCall(request, version, tasks, _, _, principal, pathParams))
      case None =>
        ZIO.fail(methodNotFoundResponse(id, version, method))

  /**
   * Modern (2026-07-28) `tools/call`. The tool runs against a
   * [[McpToolContext.modern]] context: any `sample` / `elicit` the handler
   * performs is answered from the `inputResponses` the client sent on its retry,
   * or — when an answer is not yet available — the handler aborts and the server
   * returns an [[InputRequiredResult]] (Multi Round-Trip Requests). Answered in
   * a single JSON result.
   */
  private def modernHandleToolsCall(
    request: Request,
    version: ProtocolVersion,
    tasks: Option[Ref[Map[TaskId, TaskRecord]]],
    id: RequestId,
    params: Option[Json.Obj],
    principal: Option[Principal],
    pathParams: Map[String, String],
  ): ZIO[R, Response, Response] =
    parseToolCallParams(id, params).flatMap: callParams =>
      toolsByName.get(callParams.name) match
        case None =>
          // Dynamic tool sources do not participate in MRTR.
          dispatchToSource(id, version, callParams, principal, pathParams)
        case Some(tool) =>
          val taskRequested = McpMeta.of(params).flatMap(_.get(TaskRecord.ExtensionId)).isDefined
          (taskRequested, tasks) match
            case (true, Some(store)) =>
              runToolAsTask(id, tool, callParams, principal, pathParams, store)
            case _ =>
              val inputResponses = params
                .flatMap(_.get("inputResponses")).flatMap(_.asArray)
                .map(_.flatMap(_.as[InputResponse].toOption))
                .getOrElse(Chunk.empty)
              enforceToolScopes(request, principal, tool) *> {
                val ctx = McpToolContext.modern(inputResponses, principal, pathParams)
                tool.callWithContext(callParams.arguments, ctx)
                  .foldCauseZIO(
                    cause =>
                      cause.defects.collectFirst { case s: McpToolContext.InputRequiredSignal => s } match
                        case Some(signal) =>
                          val ir = InputRequiredResult(Chunk(signal.request))
                          ZIO.succeed(rawResultResponse(id, ir.toResultJson(serverInfo)))
                        case None =>
                          resultResponse(id, version, CallToolResult(
                            content = Chunk(ToolContent.text("Tool execution failed")),
                            isError = Some(true),
                          )),
                    result => resultResponse(id, version, result),
                  )
              }

  /**
   * Execute a tool as a Tasks-extension task: create a `working` task, run the
   * tool on a background fiber that records the terminal result/error, and
   * return the task handle immediately (`resultType: "task"`). The client polls
   * `tasks/get` and cancels with `tasks/cancel`.
   */
  private def runToolAsTask(
    id: RequestId,
    tool: McpToolHandlerR[R],
    callParams: ToolCallParams,
    principal: Option[Principal],
    pathParams: Map[String, String],
    store: Ref[Map[TaskId, TaskRecord]],
  ): ZIO[R, Response, Response] =
    for
      now    <- Clock.instant.map(_.toEpochMilli)
      record  = TaskRecord.create(now)
      taskId  = record.task.taskId
      ctx     = McpToolContext.modern(Chunk.empty, principal, pathParams)
      run     = tool.callWithContext(callParams.arguments, ctx).flatMap: result =>
                  Clock.instant.map(_.toEpochMilli).flatMap: t =>
                    store.update(_.updatedWith(taskId)(_.map(r => r.copy(
                      task = r.task.copy(status = TaskStatus.Completed, lastUpdatedAt = t),
                      result = result.toJsonAST.toOption,
                    ))))
      _      <- store.update(_.updated(taskId, record))
      fiber  <- run.forkDaemon
      _      <- store.update(_.updatedWith(taskId)(_.map(_.copy(fiber = Some(fiber)))))
    yield rawResultResponse(id, ModernEnvelope.withServerInfo(
      Json.Obj(Chunk("resultType" -> Json.Str("task"), "task" -> (record.task.toJson: Json))),
      serverInfo,
    ))

  /** `tasks/get` — return the current task state, including the finished result. */
  private def handleTasksGet(
    id: RequestId,
    params: Option[Json.Obj],
    tasks: Option[Ref[Map[TaskId, TaskRecord]]],
  ): ZIO[Any, Response, Response] =
    withTaskStore(id, tasks): store =>
      taskIdParam(id, params).flatMap: taskId =>
        store.get.flatMap: m =>
          m.get(taskId) match
            case Some(record) => ZIO.succeed(rawResultResponse(id, record.toResultJson(serverInfo)))
            case None         => ZIO.fail(jsonRpcErrorResponse(Some(id), ErrorCode.InvalidParams, s"Unknown task: ${taskId.value}"))

  /** `tasks/cancel` — interrupt the task's fiber, mark it cancelled, ack empty. */
  private def handleTasksCancel(
    id: RequestId,
    params: Option[Json.Obj],
    tasks: Option[Ref[Map[TaskId, TaskRecord]]],
  ): ZIO[Any, Response, Response] =
    withTaskStore(id, tasks): store =>
      taskIdParam(id, params).flatMap: taskId =>
        for
          m   <- store.get
          now <- Clock.instant.map(_.toEpochMilli)
          _   <- m.get(taskId).flatMap(_.fiber).fold(ZIO.unit)(_.interrupt.unit)
          _   <- store.update(_.updatedWith(taskId)(_.map(r =>
                   if r.task.status.isTerminal then r
                   else r.copy(task = r.task.copy(status = TaskStatus.Cancelled, lastUpdatedAt = now)))))
        yield rawResultResponse(id, ModernEnvelope.withServerInfo(Json.Obj(), serverInfo))

  /** `tasks/update` — provide client-to-server input for an `input_required`
    * task. Not applicable to this server's tasks, so it acks with the task state. */
  private def handleTasksUpdate(
    id: RequestId,
    params: Option[Json.Obj],
    tasks: Option[Ref[Map[TaskId, TaskRecord]]],
  ): ZIO[Any, Response, Response] =
    handleTasksGet(id, params, tasks)

  private def withTaskStore(id: RequestId, tasks: Option[Ref[Map[TaskId, TaskRecord]]])(
    f: Ref[Map[TaskId, TaskRecord]] => ZIO[Any, Response, Response]
  ): ZIO[Any, Response, Response] =
    tasks match
      case Some(store) => f(store)
      case None =>
        ZIO.fail(jsonRpcErrorResponseWith(Some(id), ErrorCode.MissingRequiredClientCapability,
          "Tasks extension is not available on this endpoint", Status.Ok))

  private def taskIdParam(id: RequestId, params: Option[Json.Obj]): ZIO[Any, Response, TaskId] =
    params.flatMap(_.get("taskId")).flatMap(_.asString) match
      case Some(t) => ZIO.succeed(TaskId(t))
      case None    => ZIO.fail(jsonRpcErrorResponse(Some(id), ErrorCode.InvalidParams, "Missing 'taskId'"))

  // --- Shared method dispatch (used by both stateful and stateless) ---

  private def dispatchMethod[R1 <: R](
    id: RequestId,
    method: McpDispatchMethod,
    version: ProtocolVersion,
    params: Option[Json.Obj],
    principal: Option[Principal],
    pathParams: Map[String, String],
    onToolsCall: (RequestId, Option[Json.Obj]) => ZIO[R1, Response, Response],
  ): ZIO[R1, Response, Response] =
    if !McpDispatchMethod.isAvailable(method, version) then
      // e.g. `ping`/`logging/setLevel` on a modern request, or a modern-only
      // method on a legacy request: the method does not exist in this revision.
      ZIO.fail(methodNotFoundResponse(id, version, method.toString))
    else method match
      case McpDispatchMethod.Ping =>
        jsonRpcResponse(id, Json.Obj())
      case McpDispatchMethod.ServerDiscover =>
        handleServerDiscover(id, principal, pathParams)
      case McpDispatchMethod.ToolsList =>
        handleToolsList(id, version, params, principal, pathParams)
      case McpDispatchMethod.ToolsCall =>
        onToolsCall(id, params)
      case McpDispatchMethod.ResourcesList =>
        handleResourcesList(id, version, principal, pathParams)
      case McpDispatchMethod.ResourceTemplatesList =>
        handleResourceTemplatesList(id, version, principal, pathParams)
      case McpDispatchMethod.ResourcesRead =>
        handleResourceRead(id, version, params, principal, pathParams)
      case McpDispatchMethod.ResourcesDirectoryRead =>
        handleResourceDirectoryRead(id, version, params, principal, pathParams)
      case McpDispatchMethod.ResourcesSubscribe =>
        jsonRpcResponse(id, Json.Obj())
      case McpDispatchMethod.ResourcesUnsubscribe =>
        jsonRpcResponse(id, Json.Obj())
      case McpDispatchMethod.PromptsList =>
        handlePromptsList(id, version)
      case McpDispatchMethod.PromptsGet =>
        handlePromptsGet(id, version, params)
      case McpDispatchMethod.LoggingSetLevel =>
        jsonRpcResponse(id, Json.Obj())
      case McpDispatchMethod.CompletionComplete =>
        handleCompletionComplete(id, version, params, principal, pathParams)
      case McpDispatchMethod.SubscriptionsListen =>
        handleSubscriptionsListen(id, version, params)
      case McpDispatchMethod.TasksGet | McpDispatchMethod.TasksUpdate | McpDispatchMethod.TasksCancel =>
        // The Tasks extension is advertised but not yet implemented; report it
        // as an unsupported capability rather than a hard method-not-found.
        ZIO.fail(jsonRpcErrorResponseWith(
          Some(id), ErrorCode.MissingRequiredClientCapability,
          "Tasks extension is not supported by this server", Status.Ok))

  /**
   * `server/discover` (2026-07-28): advertise supported protocol versions,
   * capabilities, and identity in one round-trip. Servers MUST implement this.
   */
  private def handleServerDiscover(
    id: RequestId,
    principal: Option[Principal],
    pathParams: Map[String, String],
  ): ZIO[R, Response, Response] =
    resolveInstructions(principal, pathParams).map: instr =>
      val result = DiscoverResult(
        supportedVersions = ProtocolVersion.supportedWire,
        capabilities = serverCapabilities,
        serverInfo = serverInfo,
        instructions = instr,
      )
      rawResultResponse(id, result.toResultJson)

  private def routeMessage(
    request: Request,
    sessions: Ref[Map[SessionId, SessionState]],
    pendingReqs: Ref[Map[RequestId, Promise[Nothing, Json]]],
    taskStore: Ref[Map[TaskId, TaskRecord]],
    bodyJson: Json.Obj,
    principal: Option[Principal],
    pathParams: Map[String, String],
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
            handleRequest(request, sessions, pendingReqs, taskStore, id, method, params, principal, pathParams)
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
    taskStore: Ref[Map[TaskId, TaskRecord]],
    id: RequestId,
    method: String,
    params: Option[Json.Obj],
    principal: Option[Principal],
    pathParams: Map[String, String],
  ): ZIO[R, Response, Response] =
    negotiateEra(request, Some(id), method, params) match
      // A modern request is served statelessly on the same endpoint — no
      // session lookup, no `initialize`. This is what makes the server dual-era.
      case Left(resp) =>
        ZIO.succeed(resp)
      case Right(ProtocolEra.Modern(version)) =>
        dispatchModern(request, id, method, version, params, principal, pathParams, tasks = Some(taskStore))
      case Right(ProtocolEra.Legacy) =>
        method match
          case "initialize" =>
            handleInitialize(sessions, id, params, principal, pathParams)
          case _ =>
            McpDispatchMethod.parse(method) match
              case Some(dm) =>
                withSession(request, sessions):
                  dispatchMethod(id, dm, ProtocolVersion.default, params, principal, pathParams,
                    handleToolsCall(request, _, _, pendingReqs, principal, pathParams))
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
            ).as(Response.status(Status.Accepted))
          case None =>
            ZIO.succeed(Response.status(Status.Accepted))
      case Some(McpNotificationMethod.Cancelled) =>
        ZIO.succeed(Response.status(Status.Accepted))
      case None =>
        ZIO.succeed(Response.status(Status.Accepted))

  private def parseInitializeParams(
    id: RequestId,
    params: Option[Json.Obj],
    principal: Option[Principal],
    pathParams: Map[String, String],
  ): ZIO[R, Response, InitializeResult] =
    val paramsJson = params.getOrElse(Json.Obj()).toJson
    for
      init  <- ZIO.fromEither(paramsJson.fromJson[InitializeParams])
                 .mapError(e => jsonRpcErrorResponse(Some(id), ErrorCode.InvalidParams, s"Invalid initialize params: $e"))
      instr <- resolveInstructions(principal, pathParams)
    yield
      InitializeResult(
        // Echo the client's requested version when it is a supported legacy
        // revision (2025-03-26 … 2025-11-25); otherwise fall back to our newest
        // legacy revision so older/unknown clients still get a usable session.
        protocolVersion = ProtocolVersion.negotiateLegacy(init.protocolVersion).wire,
        capabilities = serverCapabilities,
        serverInfo = serverInfo,
        instructions = instr,
      )

  /** Resolve the `initialize` instructions: either the dynamic [[InstructionsSource]]
    * provider (with the request's principal/pathParams), or the static value — never
    * both, since the two `instructions` overloads are mutually exclusive. */
  private def resolveInstructions(
    principal: Option[Principal],
    pathParams: Map[String, String],
  ): ZIO[R, Nothing, Option[String]] =
    instructionsSrc match
      case Some(source) =>
        source.instructions(McpToolContext.noopWith(principal, pathParams))
      case None =>
        ZIO.succeed(instructions)

  private def handleInitialize(
    sessions: Ref[Map[SessionId, SessionState]],
    id: RequestId,
    params: Option[Json.Obj],
    principal: Option[Principal],
    pathParams: Map[String, String],
  ): ZIO[R, Response, Response] =
    parseInitializeParams(id, params, principal, pathParams).flatMap: result =>
      val sessionId = SessionId.generate
      sessions.update(_.updated(sessionId, SessionState.Initializing)) *>
        jsonRpcResponse(id, result).map(_.addHeader("mcp-session-id", sessionId.value))

  private def handleToolsList(
    id: RequestId,
    version: ProtocolVersion,
    params: Option[Json.Obj],
    principal: Option[Principal],
    pathParams: Map[String, String],
  ): ZIO[R, Response, Response] =
    val visible = authConfig match
      case None =>
        // No server-wide auth configured — every tool is visible.
        tools
      case Some(a) =>
        principal match
          case Some(p) =>
            // Hide tools whose combined (server-wide + per-tool)
            // required scopes the caller doesn't have. Tools with no
            // extra scope requirements (only the server-wide ones)
            // remain visible because the caller already cleared
            // server-wide auth to get here.
            tools.filter(t => p.hasAllScopes(a.requiredScopes ++ t.requiredScopes))
          case None =>
            // Auth is configured but the request reached us without
            // a Principal. Shouldn't happen — the auth middleware
            // upstream should have rejected. Hide everything as a
            // defensive default.
            Chunk.empty
    // Append the dynamic source's tools (already access-scoped by the source,
    // names unchanged). The static slot is filtered by scope as above.
    val ctx = McpToolContext.noopWith(principal, pathParams)
    val dynamic = toolSrc.fold[ZIO[R, Nothing, Chunk[ToolDefinition]]](ZIO.succeed(Chunk.empty))(_.listTools(ctx))
    dynamic.flatMap: extra =>
      resultResponse(id, version, ToolsListResult(tools = visible.map(_.definition) ++ extra), cacheable = true)

  private def parseToolCallParams(
    id: RequestId,
    params: Option[Json.Obj],
  ): ZIO[Any, Response, ToolCallParams] =
    val paramsJson = params.getOrElse(Json.Obj()).toJson
    ZIO.fromEither(paramsJson.fromJson[ToolCallParams])
      .mapError(e => jsonRpcErrorResponse(Some(id), ErrorCode.InvalidParams, s"Invalid tool call params: $e"))

  /** Dispatch a `tools/call` whose name matched no static tool to the dynamic source.
    * With no source configured this is an `InvalidParams` (unknown tool); with a source
    * the source returns the result (an `isError` result for unknown/forbidden names). */
  private def dispatchToSource(
    id: RequestId,
    version: ProtocolVersion,
    callParams: ToolCallParams,
    principal: Option[Principal],
    pathParams: Map[String, String],
  ): ZIO[R, Response, Response] =
    toolSrc match
      case Some(src) =>
        src.callTool(callParams.name, callParams.arguments, McpToolContext.noopWith(principal, pathParams))
          .flatMap(result => resultResponse(id, version, result))
      case None =>
        ZIO.fail(jsonRpcErrorResponse(Some(id), ErrorCode.InvalidParams, s"Unknown tool: ${callParams.name.value}"))

  private def handleToolsCall(
    request: Request,
    id: RequestId,
    params: Option[Json.Obj],
    pendingReqs: Ref[Map[RequestId, Promise[Nothing, Json]]],
    principal: Option[Principal],
    pathParams: Map[String, String],
  ): ZIO[R, Response, Response] =
    parseToolCallParams(id, params).flatMap: callParams =>
      toolsByName.get(callParams.name) match
        case None =>
          dispatchToSource(id, ProtocolVersion.default, callParams, principal, pathParams)
        case Some(tool) =>
          enforceToolScopes(request, principal, tool) *> {
            val progressToken = params.flatMap(_.get("_meta")).flatMap(_.asObject).flatMap(_.get("progressToken"))
            Queue.unbounded[JsonRpcMessage].flatMap: queue =>
              val ctx = McpToolContext.make(queue, pendingReqs, progressToken, principal, pathParams)
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
          }

  private def statelessHandleToolsCall(
    request: Request,
    version: ProtocolVersion,
    id: RequestId,
    params: Option[Json.Obj],
    principal: Option[Principal],
    pathParams: Map[String, String],
  ): ZIO[R, Response, Response] =
    parseToolCallParams(id, params).flatMap: callParams =>
      toolsByName.get(callParams.name) match
        case None =>
          dispatchToSource(id, version, callParams, principal, pathParams)
        case Some(tool) =>
          enforceToolScopes(request, principal, tool) *>
            tool.callWithContext(callParams.arguments, McpToolContext.noopWith(principal, pathParams))
              .catchAllDefect: defect =>
                ZIO.succeed(CallToolResult(
                  content = Chunk(ToolContent.text(Option(defect.getMessage).getOrElse(defect.toString))),
                  isError = Some(true),
                ))
              .flatMap(result => resultResponse(id, version, result))

  private def handleResourcesList(
    id: RequestId,
    version: ProtocolVersion,
    principal: Option[Principal],
    pathParams: Map[String, String],
  ): ZIO[R, Response, Response] =
    val ctx = McpToolContext.noopWith(principal, pathParams)
    val dynamic = resourceSrc.fold[ZIO[R, Nothing, Chunk[ResourceDefinition]]](ZIO.succeed(Chunk.empty))(_.listResources(ctx))
    dynamic.flatMap: extra =>
      resultResponse(id, version, ResourcesListResult(resources = resources.map(_.definition) ++ extra), cacheable = true)

  private def handleResourceTemplatesList(
    id: RequestId,
    version: ProtocolVersion,
    principal: Option[Principal],
    pathParams: Map[String, String],
  ): ZIO[R, Response, Response] =
    val ctx = McpToolContext.noopWith(principal, pathParams)
    val dynamic = resourceSrc.fold[ZIO[R, Nothing, Chunk[ResourceTemplateDefinition]]](ZIO.succeed(Chunk.empty))(_.listResourceTemplates(ctx))
    dynamic.flatMap: extra =>
      resultResponse(id, version, ResourceTemplatesListResult(resourceTemplates = resourceTemplates.map(_.definition) ++ extra), cacheable = true)

  /** Resource-not-found error, using the code appropriate to the negotiated
    * version (`-32002` legacy, `-32602` modern). */
  private def resourceNotFoundResponse(id: RequestId, version: ProtocolVersion, message: String): Response =
    val code = ErrorCode.resourceNotFound(version)
    Response.json(JsonRpcError(Some(id), ErrorDetail(code, message)).toJson)

  private def handleResourceRead(
    id: RequestId,
    version: ProtocolVersion,
    params: Option[Json.Obj],
    principal: Option[Principal],
    pathParams: Map[String, String],
  ): ZIO[R, Response, Response] =
    val paramsJson = params.getOrElse(Json.Obj()).toJson
    ZIO.fromEither(paramsJson.fromJson[ResourceReadParams])
      .mapError(e => jsonRpcErrorResponse(Some(id), ErrorCode.InvalidParams, s"Invalid resource read params: $e"))
      .flatMap: readParams =>
        val uri = readParams.uri
        val directMatch = resources.find(_.definition.uri == uri)
        val staticHandler: Option[String => ZIO[Any, ToolError, Chunk[ResourceContents]]] =
          directMatch.map(r => r.read)
            .orElse(resourceTemplates.find(matchesTemplate(_, uri)).map(_.read))

        staticHandler match
          case Some(readFn) =>
            readFn(uri).foldZIO(
              err => ZIO.fail(jsonRpcErrorResponse(Some(id), ErrorCode.InternalError, err.message)),
              contents => resultResponse(id, version, ResourceReadResult(contents = contents), cacheable = true),
            )
          case None =>
            // No static resource/template matched — fall through to the dynamic source.
            resourceSrc match
              case None =>
                ZIO.fail(resourceNotFoundResponse(id, version, s"Resource not found: $uri"))
              case Some(src) =>
                src.readResource(uri, McpToolContext.noopWith(principal, pathParams)).foldZIO(
                  err => ZIO.fail(resourceNotFoundResponse(id, version, err.message)),
                  contents => resultResponse(id, version, ResourceReadResult(contents = contents), cacheable = true),
                )

  private def matchesTemplate(tmpl: McpResourceTemplateHandler, uri: String): Boolean =
    val pattern = tmpl.definition.uriTemplate
    val regex = pattern.replaceAll("\\{[^}]+}", "([^/]+)")
    uri.matches(regex)

  private def handleResourceDirectoryRead(
    id: RequestId,
    version: ProtocolVersion,
    params: Option[Json.Obj],
    principal: Option[Principal],
    pathParams: Map[String, String],
  ): ZIO[R, Response, Response] =
    val paramsJson = params.getOrElse(Json.Obj()).toJson
    ZIO.fromEither(paramsJson.fromJson[ResourceDirectoryReadParams])
      .mapError(e => jsonRpcErrorResponse(Some(id), ErrorCode.InvalidParams, s"Invalid directory read params: $e"))
      .flatMap: rp =>
        resourceSrc match
          case None =>
            // Per SEP-2640: an unknown / non-directory URI is InvalidParams.
            ZIO.fail(jsonRpcErrorResponse(Some(id), ErrorCode.InvalidParams, s"Not a directory resource: ${rp.uri}"))
          case Some(src) =>
            src.readDirectory(rp.uri, McpToolContext.noopWith(principal, pathParams)).foldZIO(
              err      => ZIO.fail(jsonRpcErrorResponse(Some(id), ErrorCode.InvalidParams, err.message)),
              children => resultResponse(id, version, ResourcesListResult(resources = children), cacheable = true),
            )

  private def handlePromptsList(id: RequestId, version: ProtocolVersion): ZIO[Any, Response, Response] =
    resultResponse(id, version, PromptsListResult(prompts = prompts.map(_.definition)), cacheable = true)

  private def handlePromptsGet(id: RequestId, version: ProtocolVersion, params: Option[Json.Obj]): ZIO[Any, Response, Response] =
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
              result => resultResponse(id, version, result),
            )

  private def handleCompletionComplete(
    id: RequestId,
    version: ProtocolVersion,
    params: Option[Json.Obj],
    principal: Option[Principal],
    pathParams: Map[String, String],
  ): ZIO[R, Response, Response] =
    resourceSrc match
      case None =>
        resultResponse(id, version, CompletionResult(completion = CompletionValues(values = Chunk.empty)))
      case Some(src) =>
        val paramsJson = params.getOrElse(Json.Obj()).toJson
        ZIO.fromEither(paramsJson.fromJson[CompletionCompleteParams])
          .mapError(e => jsonRpcErrorResponse(Some(id), ErrorCode.InvalidParams, s"Invalid completion params: $e"))
          .flatMap: cp =>
            src.complete(cp.ref, cp.argument, McpToolContext.noopWith(principal, pathParams))
              .flatMap(result => resultResponse(id, version, result))

  /**
   * `subscriptions/listen` (2026-07-28): a single long-lived SSE stream carrying
   * the change-notification types the client opts into. This server currently
   * emits no change notifications, so the stream acknowledges the subscription
   * and stays open with keep-alives until the client disconnects.
   */
  private def handleSubscriptionsListen(
    id: RequestId,
    version: ProtocolVersion,
    params: Option[Json.Obj],
  ): ZIO[Any, Response, Response] =
    val subscriptionId = SessionId.generate.value
    val ackParams = Json.Obj(Chunk(
      McpMeta.SubscriptionId -> Json.Str(subscriptionId),
    ))
    val ack = (JsonRpcMessage.Notification("notifications/subscriptions/acknowledged", Some(ackParams)): JsonRpcMessage).toJson
    val ackEvent = ZStream.succeed(sseEvent(ack))
    val keepalive = ZStream.tick(30.seconds).as(":\n\n")
    ZIO.succeed(Response(
      status = Status.Ok,
      headers = Headers(
        Header.ContentType(MediaType.text.`event-stream`),
        Header.CacheControl.NoCache,
      ),
      body = Body.fromCharSequenceStreamChunked(ackEvent ++ keepalive),
    ))

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

  private def getHandler(pathParams: Map[String, String], request: Request): ZIO[R & McpServer.State, Response, Response] =
    val _ = pathParams
    for
      _     <- validateOrigin(request)
      _     <- authenticate(request)
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

  private def deleteHandler(pathParams: Map[String, String], request: Request): ZIO[R & McpServer.State, Response, Response] =
    val _ = pathParams
    for
      _     <- validateOrigin(request)
      _     <- authenticate(request)
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

  /**
   * Version-aware result response. For a legacy version the result JSON is sent
   * verbatim (byte-for-byte the same as [[jsonRpcResponse]]); for a modern
   * (2026-07-28+) version it is wrapped in the modern envelope — `resultType`,
   * `_meta.serverInfo`, and, for cacheable methods, `ttlMs` / `cacheScope`.
   */
  private def resultResponse[A: JsonEncoder](
    id: RequestId,
    version: ProtocolVersion,
    result: A,
    cacheable: Boolean = false,
  ): ZIO[Any, Response, Response] =
    ZIO.fromEither(result.toJsonAST)
      .mapError(e => jsonRpcErrorResponse(None, ErrorCode.InternalError, s"JSON encoding failed: $e"))
      .map: json =>
        val obj = json.asObject.getOrElse(Json.Obj())
        val finalObj =
          if version.isStateless then ModernEnvelope.complete(obj, serverInfo, cacheable) else obj
        Response.json(JsonRpcResponse(id, finalObj).toJson)

  /** Response for a raw already-built result object (used by `server/discover`,
    * whose result carries its own `_meta`/`resultType`). */
  private def rawResultResponse(id: RequestId, resultObj: Json.Obj): Response =
    Response.json(JsonRpcResponse(id, resultObj).toJson)

  private def jsonRpcErrorResponse(id: Option[RequestId], code: ErrorCode, message: String): Response =
    val e = JsonRpcError.fromCode(id, code, message)
    Response.json(e.toJson)

  /** A JSON-RPC error carrying explicit `data`, at a specified HTTP status. */
  private def jsonRpcErrorResponseWith(
    id: Option[RequestId],
    code: ErrorCode,
    message: String,
    status: Status,
    data: Option[Json] = None,
  ): Response =
    val e = JsonRpcError(id, ErrorDetail(code.code, message, data))
    Response.json(e.toJson).status(status)

  /** Map a [[NegotiationError]] onto its HTTP response: `400` with a specific
    * JSON-RPC error code (`-32020` header mismatch, `-32022` unsupported version). */
  private def negotiationErrorResponse(id: Option[RequestId], err: NegotiationError): Response =
    err match
      case NegotiationError.HeaderMismatch(message) =>
        jsonRpcErrorResponseWith(id, ErrorCode.HeaderMismatch, message, Status.BadRequest)
      case NegotiationError.UnsupportedVersion(requested) =>
        jsonRpcErrorResponseWith(
          id,
          ErrorCode.UnsupportedProtocolVersion,
          "Unsupported protocol version",
          Status.BadRequest,
          Some(Negotiation.unsupportedVersionData(requested)),
        )

  /** Method-not-found response. Modern requests get HTTP `404` (per the
    * Streamable HTTP spec); legacy requests keep the JSON-RPC-only `200`. */
  private def methodNotFoundResponse(id: RequestId, version: ProtocolVersion, method: String): Response =
    val base = jsonRpcErrorResponse(Some(id), ErrorCode.MethodNotFound, s"Method not found: $method")
    if version.isStateless then base.status(Status.NotFound) else base

  private def badRequest(message: String): Response =
    Response.json(
      JsonRpcError(None, ErrorDetail(ErrorCode.ParseError.code, message)).toJson
    ).status(Status.BadRequest)

object McpServer:
  def apply(name: String, version: String): McpServer[Any] =
    new McpServer(Implementation(name, version), Chunk.empty, Chunk.empty, Chunk.empty, Chunk.empty, None)

  trait State:
    def sessions: Ref[Map[SessionId, SessionState]]
    def pendingRequests: Ref[Map[RequestId, Promise[Nothing, Json]]]
    /** In-memory store for the 2026-07-28 Tasks extension. */
    def tasks: Ref[Map[TaskId, TaskRecord]]

  object State:
    val default: ULayer[State] = ZLayer.fromZIO:
      for
        s <- Ref.make(Map.empty[SessionId, SessionState])
        p <- Ref.make(Map.empty[RequestId, Promise[Nothing, Json]])
        t <- Ref.make(Map.empty[TaskId, TaskRecord])
      yield new State:
        val sessions = s
        val pendingRequests = p
        val tasks = t

  private val localhostPatterns = Set("localhost", "127.0.0.1", "[::1]", "::1")

  private[mcp] def isLocalhostHost(hostWithPort: String): Boolean =
    val host = hostWithPort.split(':').head
    localhostPatterns.contains(host)
