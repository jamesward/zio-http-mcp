package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.auth.Principal
import zio.*
import zio.json.*
import zio.json.ast.Json

// --- Log Level ---

enum LogLevel:
  case Debug, Info, Warning, Error, Critical, Alert, Emergency

object LogLevel:
  given CanEqual[LogLevel, LogLevel] = CanEqual.derived

  extension (l: LogLevel) def asString: String = l match
    case LogLevel.Debug     => "debug"
    case LogLevel.Info      => "info"
    case LogLevel.Warning   => "warning"
    case LogLevel.Error     => "error"
    case LogLevel.Critical  => "critical"
    case LogLevel.Alert     => "alert"
    case LogLevel.Emergency => "emergency"

  given JsonEncoder[LogLevel] = JsonEncoder.string.contramap(_.asString)

  given JsonDecoder[LogLevel] = JsonDecoder.string.mapOrFail:
    case "debug"     => Right(LogLevel.Debug)
    case "info"      => Right(LogLevel.Info)
    case "warning"   => Right(LogLevel.Warning)
    case "error"     => Right(LogLevel.Error)
    case "critical"  => Right(LogLevel.Critical)
    case "alert"     => Right(LogLevel.Alert)
    case "emergency" => Right(LogLevel.Emergency)
    case other       => Left(s"Unknown log level: $other")

// --- Sampling result ---

case class SamplingResult(
  role: String,
  content: ToolContent,
  model: String,
  stopReason: Option[String] = None,
)

object SamplingResult:
  given CanEqual[SamplingResult, SamplingResult] = CanEqual.derived

// --- Elicitation result ---

case class ElicitationResult(
  action: String,
  content: Option[Map[String, Json]] = None,
)

object ElicitationResult:
  given CanEqual[ElicitationResult, ElicitationResult] = CanEqual.derived

// --- Tool context for emitting notifications during tool execution ---

trait McpToolContext extends McpRequestContext:
  def log(level: LogLevel, message: String): UIO[Unit]
  def progress(current: Double, total: Double, message: Option[String] = None): UIO[Unit]
  def sample(prompt: String, maxTokens: Int = 100): ZIO[Any, ToolError, SamplingResult]
  def elicit(message: String, schema: Json.Obj): ZIO[Any, ToolError, ElicitationResult]
  /** The authenticated principal for this request, if `.auth(...)` is configured. */
  override def principal: Option[Principal] = None
  /**
   * Path parameters captured by a parameterised mount (see `McpServer.mountedAtParam`).
   * Empty for a fixed-path mount. A dynamic source reads e.g. `pathParams("slug")` to
   * learn which mount the request arrived on.
   */
  override def pathParams: Map[String, String] = Map.empty

object McpToolContext:
  private val requestIdCounter = new java.util.concurrent.atomic.AtomicInteger(0)

  private[mcp] def make(
    outQueue: Queue[JsonRpcMessage],
    pendingRequests: Ref[Map[RequestId, Promise[Nothing, Json]]],
    progressToken: Option[Json],
    callerPrincipal: Option[Principal] = None,
    callerPathParams: Map[String, String] = Map.empty,
  ): McpToolContext =
    new McpToolContext:
      override val principal: Option[Principal] = callerPrincipal
      override val pathParams: Map[String, String] = callerPathParams

      def log(level: LogLevel, message: String): UIO[Unit] =
        val params = Json.Obj(Chunk(
          "level" -> Json.Str(level.asString),
          "data" -> Json.Str(message),
        ))
        outQueue.offer(JsonRpcMessage.Notification("notifications/message", Some(params))).unit

      def progress(current: Double, total: Double, message: Option[String]): UIO[Unit] =
        progressToken match
          case None => ZIO.unit
          case Some(token) =>
            val fields = Chunk(
              "progressToken" -> token,
              "progress" -> Json.Num(current),
              "total" -> Json.Num(total),
            ) ++ message.fold(Chunk.empty[(String, Json)])(m => Chunk("message" -> Json.Str(m)))
            outQueue.offer(JsonRpcMessage.Notification("notifications/progress", Some(Json.Obj(fields)))).unit

      def sample(prompt: String, maxTokens: Int): ZIO[Any, ToolError, SamplingResult] =
        val reqId = RequestId.Num(requestIdCounter.incrementAndGet())
        val params = Json.Obj(Chunk(
          "messages" -> Json.Arr(Chunk(Json.Obj(Chunk(
            "role" -> Json.Str("user"),
            "content" -> Json.Obj(Chunk(
              "type" -> Json.Str("text"),
              "text" -> Json.Str(prompt),
            )),
          )))),
          "maxTokens" -> Json.Num(maxTokens),
        ))
        sendServerRequest(reqId, "sampling/createMessage", params).flatMap: responseJson =>
          val role = responseJson.asObject.flatMap(_.get("role")).flatMap(_.asString).getOrElse("assistant")
          val model = responseJson.asObject.flatMap(_.get("model")).flatMap(_.asString).getOrElse("unknown")
          val stopReason = responseJson.asObject.flatMap(_.get("stopReason")).flatMap(_.asString)
          val content = responseJson.asObject.flatMap(_.get("content")) match
            case Some(c) => c.as[ToolContent].toOption.getOrElse(ToolContent.text(""))
            case None    => ToolContent.text("")
          ZIO.succeed(SamplingResult(role, content, model, stopReason))

      def elicit(message: String, schema: Json.Obj): ZIO[Any, ToolError, ElicitationResult] =
        val reqId = RequestId.Num(requestIdCounter.incrementAndGet())
        val params = Json.Obj(Chunk(
          "message" -> Json.Str(message),
          "requestedSchema" -> (schema: Json),
        ))
        sendServerRequest(reqId, "elicitation/create", params).flatMap: responseJson =>
          val action = responseJson.asObject.flatMap(_.get("action")).flatMap(_.asString).getOrElse("decline")
          val content = responseJson.asObject.flatMap(_.get("content")).flatMap(_.asObject).map: obj =>
            obj.fields.map((k, v) => k -> v).toMap
          ZIO.succeed(ElicitationResult(action, content))

      private def sendServerRequest(reqId: RequestId, method: String, params: Json.Obj): ZIO[Any, ToolError, Json] =
        for
          promise <- Promise.make[Nothing, Json]
          _       <- pendingRequests.update(_ + (reqId -> promise))
          _       <- outQueue.offer(JsonRpcMessage.Request(reqId, method, Some(params)))
          result  <- promise.await
          _       <- pendingRequests.update(_ - reqId)
        yield result

  /**
   * Defect raised by a modern (2026-07-28) tool context when the handler needs
   * input the client has not yet supplied. The dispatcher catches it and turns
   * it into an [[InputRequiredResult]] (Multi Round-Trip Requests). The tool is
   * re-executed on the client's retry with the answer available, so handlers
   * must be safe to replay up to the point of each input request.
   */
  private[mcp] final case class InputRequiredSignal(request: InputRequest)
    extends RuntimeException(s"MCP input required: ${request.method}")

  /**
   * A modern tool context. Server-initiated interactions do not stream as
   * JSON-RPC requests; instead each `sample` / `elicit` call is answered from
   * the `inputResponses` the client sent on its retry (matched by call order),
   * or — when no answer is available yet — aborts the handler with an
   * [[InputRequiredSignal]] so the server can ask for it via MRTR.
   *
   * Request-scoped notifications flow on the response stream of the request
   * they relate to (2026-07-28 Streamable HTTP): when `notifications` is set
   * the dispatcher answers the call with an SSE stream and this context feeds
   * it. `progress` emits only when the request carried a `_meta.progressToken`,
   * and `log` emits only when the request opted in via
   * `_meta.io.modelcontextprotocol/logLevel` (at or above that level) — servers
   * MUST NOT emit `notifications/message` for requests that did not include it.
   * With no `notifications` queue (a plain single-JSON call) both are no-ops.
   */
  private[mcp] def modern(
    inputResponses: Chunk[InputResponse],
    callerPrincipal: Option[Principal] = None,
    callerPathParams: Map[String, String] = Map.empty,
    notifications: Option[Queue[JsonRpcMessage]] = None,
    progressToken: Option[Json] = None,
    minLogLevel: Option[LogLevel] = None,
  ): McpToolContext =
    new McpToolContext:
      private val counter = new java.util.concurrent.atomic.AtomicInteger(0)

      override val principal: Option[Principal] = callerPrincipal
      override val pathParams: Map[String, String] = callerPathParams

      def log(level: LogLevel, message: String): UIO[Unit] =
        (notifications, minLogLevel) match
          case (Some(queue), Some(min)) if level.ordinal >= min.ordinal =>
            val params = Json.Obj(Chunk(
              "level" -> Json.Str(level.asString),
              "data" -> Json.Str(message),
            ))
            queue.offer(JsonRpcMessage.Notification("notifications/message", Some(params))).unit
          case _ => ZIO.unit

      def progress(current: Double, total: Double, message: Option[String]): UIO[Unit] =
        (notifications, progressToken) match
          case (Some(queue), Some(token)) =>
            val fields = Chunk(
              "progressToken" -> token,
              "progress" -> Json.Num(current),
              "total" -> Json.Num(total),
            ) ++ message.fold(Chunk.empty[(String, Json)])(m => Chunk("message" -> Json.Str(m)))
            queue.offer(JsonRpcMessage.Notification("notifications/progress", Some(Json.Obj(fields)))).unit
          case _ => ZIO.unit

      def sample(prompt: String, maxTokens: Int): ZIO[Any, ToolError, SamplingResult] =
        val params = Json.Obj(Chunk(
          "messages" -> Json.Arr(Chunk(Json.Obj(Chunk(
            "role" -> Json.Str("user"),
            "content" -> Json.Obj(Chunk("type" -> Json.Str("text"), "text" -> Json.Str(prompt))),
          )))),
          "maxTokens" -> Json.Num(maxTokens),
        ))
        nextInput("sampling/createMessage", params).map: responseJson =>
          val role = responseJson.asObject.flatMap(_.get("role")).flatMap(_.asString).getOrElse("assistant")
          val model = responseJson.asObject.flatMap(_.get("model")).flatMap(_.asString).getOrElse("unknown")
          val stopReason = responseJson.asObject.flatMap(_.get("stopReason")).flatMap(_.asString)
          val content = responseJson.asObject.flatMap(_.get("content")) match
            case Some(c) => c.as[ToolContent].toOption.getOrElse(ToolContent.text(""))
            case None    => ToolContent.text("")
          SamplingResult(role, content, model, stopReason)

      def elicit(message: String, schema: Json.Obj): ZIO[Any, ToolError, ElicitationResult] =
        val params = Json.Obj(Chunk(
          "message" -> Json.Str(message),
          "requestedSchema" -> (schema: Json),
        ))
        nextInput("elicitation/create", params).map: responseJson =>
          val action = responseJson.asObject.flatMap(_.get("action")).flatMap(_.asString).getOrElse("decline")
          val content = responseJson.asObject.flatMap(_.get("content")).flatMap(_.asObject).map: obj =>
            obj.fields.map((k, v) => k -> v).toMap
          ElicitationResult(action, content)

      /** Answer the next input request from the replayed responses, or abort
        * with an [[InputRequiredSignal]] when the client has not provided it. */
      private def nextInput(method: String, params: Json.Obj): ZIO[Any, ToolError, Json] =
        val idx = counter.getAndIncrement()
        val correlationId = s"input-$idx"
        if idx < inputResponses.length then
          ZIO.succeed(inputResponses(idx).result)
        else
          ZIO.die(InputRequiredSignal(InputRequest(correlationId, method, params)))

  private[mcp] val noop: McpToolContext = noopWith(None)

  private[mcp] def noopWith(
    callerPrincipal: Option[Principal],
    callerPathParams: Map[String, String] = Map.empty,
  ): McpToolContext = new McpToolContext:
    override val principal: Option[Principal] = callerPrincipal
    override val pathParams: Map[String, String] = callerPathParams
    def log(level: LogLevel, message: String): UIO[Unit] = ZIO.unit
    def progress(current: Double, total: Double, message: Option[String]): UIO[Unit] = ZIO.unit
    def sample(prompt: String, maxTokens: Int): ZIO[Any, ToolError, SamplingResult] =
      ZIO.fail(ToolError("Sampling not available"))
    def elicit(message: String, schema: Json.Obj): ZIO[Any, ToolError, ElicitationResult] =
      ZIO.fail(ToolError("Elicitation not available"))
