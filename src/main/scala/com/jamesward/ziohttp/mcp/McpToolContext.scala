package com.jamesward.ziohttp.mcp

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

trait McpToolContext:
  def log(level: LogLevel, message: String): UIO[Unit]
  def progress(current: Double, total: Double, message: Option[String] = None): UIO[Unit]
  def sample(prompt: String, maxTokens: Int = 100): ZIO[Any, ToolError, SamplingResult]
  def elicit(message: String, schema: Json.Obj): ZIO[Any, ToolError, ElicitationResult]

object McpToolContext:
  private val requestIdCounter = new java.util.concurrent.atomic.AtomicInteger(0)

  private[mcp] def make(
    outQueue: Queue[JsonRpcMessage],
    pendingRequests: Ref[Map[RequestId, Promise[Nothing, Json]]],
    progressToken: Option[Json],
  ): McpToolContext =
    new McpToolContext:
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

  private[mcp] val noop: McpToolContext = new McpToolContext:
    def log(level: LogLevel, message: String): UIO[Unit] = ZIO.unit
    def progress(current: Double, total: Double, message: Option[String]): UIO[Unit] = ZIO.unit
    def sample(prompt: String, maxTokens: Int): ZIO[Any, ToolError, SamplingResult] =
      ZIO.fail(ToolError("Sampling not available"))
    def elicit(message: String, schema: Json.Obj): ZIO[Any, ToolError, ElicitationResult] =
      ZIO.fail(ToolError("Elicitation not available"))
