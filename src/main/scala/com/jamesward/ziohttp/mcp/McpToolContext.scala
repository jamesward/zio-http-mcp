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

// --- Roots ---

/** One filesystem root the client exposes, as returned by `roots/list`. */
case class Root(uri: String, name: Option[String] = None)

object Root:
  given CanEqual[Root, Root] = CanEqual.derived
  given JsonCodec[Root] = DeriveJsonCodec.gen[Root]

// --- Input requests ---

/**
 * One input a handler needs from the client: an elicitation, a sampling
 * request, or the client's roots.
 *
 * The `id` is the handler's correlation id for that input. Under the modern
 * (2026-07-28) revision it is the key the server puts in `inputRequests` and
 * the client echoes in `inputResponses`, so a handler that cares what the key
 * looks like — a conformance fixture, or a tool whose client expects stable
 * names — chooses it here. Pass several to [[McpToolContext.inputs]] to ask for
 * them in a single round trip.
 */
sealed trait InputSpec:
  def id: String
  private[mcp] def toRequest: InputRequest

object InputSpec:
  given CanEqual[InputSpec, InputSpec] = CanEqual.derived

  /** Ask the user a question, answered against `schema` (`elicitation/create`). */
  def elicit(id: String, message: String, schema: Json.Obj): InputSpec =
    Elicit(id, message, schema)

  /** Ask the client's model to sample a completion (`sampling/createMessage`). */
  def sample(id: String, prompt: String, maxTokens: Int = 100): InputSpec =
    Sample(id, prompt, maxTokens)

  /** Ask the client for the roots it exposes (`roots/list`). */
  def listRoots(id: String): InputSpec = ListRoots(id)

  private[mcp] final case class Elicit(id: String, message: String, schema: Json.Obj) extends InputSpec:
    def toRequest: InputRequest = InputRequest(id, "elicitation/create", Json.Obj(Chunk(
      "message" -> Json.Str(message),
      "requestedSchema" -> (schema: Json),
    )))

  private[mcp] final case class Sample(id: String, prompt: String, maxTokens: Int) extends InputSpec:
    def toRequest: InputRequest = InputRequest(id, "sampling/createMessage", Json.Obj(Chunk(
      "messages" -> Json.Arr(Chunk(Json.Obj(Chunk(
        "role" -> Json.Str("user"),
        "content" -> Json.Obj(Chunk("type" -> Json.Str("text"), "text" -> Json.Str(prompt))),
      )))),
      "maxTokens" -> Json.Num(maxTokens),
    )))

  private[mcp] final case class ListRoots(id: String) extends InputSpec:
    def toRequest: InputRequest = InputRequest(id, "roots/list", Json.Obj())

/**
 * The answers to a batch of [[InputSpec]]s, keyed by the id each was asked
 * under. Every requested id is present — [[McpToolContext.inputs]] only
 * completes once the client has answered them all — so the accessors decode
 * directly rather than returning an effect.
 */
final class InputResults private[mcp] (answers: Map[String, Json]):
  /** The raw answer JSON for `id`, if the client sent one. */
  def json(id: String): Option[Json] = answers.get(id)

  /** The answer to an [[InputSpec.elicit]], defaulting to a decline. */
  def elicitation(id: String): ElicitationResult =
    answers.get(id).fold(ElicitationResult("decline"))(McpToolContext.decodeElicitation)

  /** The answer to an [[InputSpec.sample]]. */
  def sampling(id: String): SamplingResult =
    answers.get(id).fold(SamplingResult("assistant", ToolContent.text(""), "unknown"))(McpToolContext.decodeSampling)

  /** The answer to an [[InputSpec.listRoots]], empty when the client has none. */
  def roots(id: String): Chunk[Root] =
    answers.get(id).fold(Chunk.empty)(McpToolContext.decodeRoots)

// --- Tool context for emitting notifications during tool execution ---

trait McpToolContext extends McpRequestContext:
  def log(level: LogLevel, message: String): UIO[Unit]
  def progress(current: Double, total: Double, message: Option[String] = None): UIO[Unit]
  def sample(prompt: String, maxTokens: Int = 100): ZIO[Any, ToolError, SamplingResult]
  def elicit(message: String, schema: Json.Obj): ZIO[Any, ToolError, ElicitationResult]
  /** The client's roots (`roots/list`). */
  def listRoots: ZIO[Any, ToolError, Chunk[Root]]
  /** As [[sample]], under a correlation id the handler chooses. */
  def sample(id: String, prompt: String, maxTokens: Int): ZIO[Any, ToolError, SamplingResult]
  /** As [[elicit]], under a correlation id the handler chooses. */
  def elicit(id: String, message: String, schema: Json.Obj): ZIO[Any, ToolError, ElicitationResult]
  /** As [[listRoots]], under a correlation id the handler chooses. */
  def listRoots(id: String): ZIO[Any, ToolError, Chunk[Root]]
  /**
   * Ask for several inputs at once. Under the modern (2026-07-28) revision the
   * unanswered ones travel as a single `input_required` result, so the client
   * can fulfil them together and retry once; a legacy connection asks for them
   * one after another over the SSE back-channel.
   */
  def inputs(specs: InputSpec*): ZIO[Any, ToolError, InputResults]
  /**
   * The opaque state this server attached to the `input_required` result the
   * client is now retrying, verified as untampered. `None` on a first call, and
   * always `None` on a legacy connection, which resumes on the open stream
   * rather than through the client.
   */
  def requestState: Option[String] = None
  /**
   * Set the opaque state to carry on the next `input_required` result. The
   * server signs it on the way out and verifies it on the way back, so a
   * handler can trust what it reads from [[requestState]] but should still
   * treat it as visible to the client.
   */
  def setRequestState(state: String): UIO[Unit] = ZIO.unit
  /**
   * The capabilities the client declared for this request
   * (`_meta.io.modelcontextprotocol/clientCapabilities`), if any. A handler
   * asks for input the client can actually answer — see [[clientSupports]].
   */
  def clientCapabilities: Option[Json.Obj] = None
  /**
   * Whether the client declared `capability` (e.g. `"sampling"`,
   * `"elicitation"`, `"roots"`). A client that declared nothing at all is
   * treated as supporting everything: absence of the `_meta` key means the
   * client did not say, not that it refuses.
   */
  final def clientSupports(capability: String): Boolean =
    clientCapabilities.forall(_.get(capability).isDefined)
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

  // --- Shared decoders for input answers ---
  // The payloads are whatever the client would have returned from the
  // corresponding server-to-client request, so decoding stays lenient: a
  // missing field falls back rather than failing the whole tool call.

  private[mcp] def decodeSampling(json: Json): SamplingResult =
    val fields = json.asObject
    SamplingResult(
      role = fields.flatMap(_.get("role")).flatMap(_.asString).getOrElse("assistant"),
      content = fields.flatMap(_.get("content")).flatMap(_.as[ToolContent].toOption).getOrElse(ToolContent.text("")),
      model = fields.flatMap(_.get("model")).flatMap(_.asString).getOrElse("unknown"),
      stopReason = fields.flatMap(_.get("stopReason")).flatMap(_.asString),
    )

  private[mcp] def decodeElicitation(json: Json): ElicitationResult =
    val fields = json.asObject
    ElicitationResult(
      action = fields.flatMap(_.get("action")).flatMap(_.asString).getOrElse("decline"),
      content = fields.flatMap(_.get("content")).flatMap(_.asObject).map(_.fields.toMap),
    )

  private[mcp] def decodeRoots(json: Json): Chunk[Root] =
    json.asObject.flatMap(_.get("roots")).flatMap(_.asArray)
      .fold(Chunk.empty)(_.flatMap(_.as[Root].toOption))

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

      // A legacy connection has an open back-channel, so every input is a
      // server-to-client JSON-RPC request answered on the spot. The correlation
      // ids of the modern flow have no counterpart here: the JSON-RPC request
      // id already pairs question with answer, so the id-bearing overloads
      // ignore theirs and a batch is simply asked one request at a time.

      def sample(prompt: String, maxTokens: Int): ZIO[Any, ToolError, SamplingResult] =
        ask(InputSpec.sample("", prompt, maxTokens)).map(decodeSampling)

      def sample(id: String, prompt: String, maxTokens: Int): ZIO[Any, ToolError, SamplingResult] =
        sample(prompt, maxTokens)

      def elicit(message: String, schema: Json.Obj): ZIO[Any, ToolError, ElicitationResult] =
        ask(InputSpec.elicit("", message, schema)).map(decodeElicitation)

      def elicit(id: String, message: String, schema: Json.Obj): ZIO[Any, ToolError, ElicitationResult] =
        elicit(message, schema)

      def listRoots: ZIO[Any, ToolError, Chunk[Root]] =
        ask(InputSpec.listRoots("")).map(decodeRoots)

      def listRoots(id: String): ZIO[Any, ToolError, Chunk[Root]] = listRoots

      def inputs(specs: InputSpec*): ZIO[Any, ToolError, InputResults] =
        ZIO.foreach(Chunk.fromIterable(specs))(spec => ask(spec).map(spec.id -> _))
          .map(answers => InputResults(answers.toMap))

      private def ask(spec: InputSpec): ZIO[Any, ToolError, Json] =
        val request = spec.toRequest
        sendServerRequest(RequestId.Num(requestIdCounter.incrementAndGet()), request.method, request.params)

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
   * it into an [[InputRequiredResult]] (Multi Round-Trip Requests). The handler
   * is re-executed on the client's retry with the answers available, so it must
   * be safe to replay up to the point of each input request — or carry what it
   * has already done in [[McpToolContext.setRequestState]].
   *
   * @param requests the inputs still unanswered, all of them, so one round trip
   *                 can carry a whole batch
   * @param requestState the state the handler set for the next round, unsigned
   */
  private[mcp] final case class InputRequiredSignal(
    requests: Chunk[InputRequest],
    requestState: Option[String],
  ) extends RuntimeException(s"MCP input required: ${requests.map(_.method).mkString(", ")}")

  /**
   * A modern tool context. Server-initiated interactions do not stream as
   * JSON-RPC requests; instead each input is answered from the `inputResponses`
   * the client sent on its retry, matched by correlation id, or — when an
   * answer is missing — the handler aborts with an [[InputRequiredSignal]] so
   * the server can ask for it via MRTR.
   *
   * Handlers that do not name their inputs get positional ids (`input-0`,
   * `input-1`, …) assigned in call order, which replay reproduces.
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
    inputResponses: Map[String, Json],
    callerPrincipal: Option[Principal] = None,
    callerPathParams: Map[String, String] = Map.empty,
    notifications: Option[Queue[JsonRpcMessage]] = None,
    progressToken: Option[Json] = None,
    minLogLevel: Option[LogLevel] = None,
    incomingState: Option[String] = None,
    declaredCapabilities: Option[Json.Obj] = None,
  ): McpToolContext =
    new McpToolContext:
      private val counter = new java.util.concurrent.atomic.AtomicInteger(0)
      private val nextState = new java.util.concurrent.atomic.AtomicReference[Option[String]](None)

      override val principal: Option[Principal] = callerPrincipal
      override val pathParams: Map[String, String] = callerPathParams
      override val requestState: Option[String] = incomingState
      override val clientCapabilities: Option[Json.Obj] = declaredCapabilities

      override def setRequestState(state: String): UIO[Unit] =
        ZIO.succeed(nextState.set(Some(state)))

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
        sample(nextId(), prompt, maxTokens)

      def sample(id: String, prompt: String, maxTokens: Int): ZIO[Any, ToolError, SamplingResult] =
        inputs(InputSpec.sample(id, prompt, maxTokens)).map(_.sampling(id))

      def elicit(message: String, schema: Json.Obj): ZIO[Any, ToolError, ElicitationResult] =
        elicit(nextId(), message, schema)

      def elicit(id: String, message: String, schema: Json.Obj): ZIO[Any, ToolError, ElicitationResult] =
        inputs(InputSpec.elicit(id, message, schema)).map(_.elicitation(id))

      def listRoots: ZIO[Any, ToolError, Chunk[Root]] = listRoots(nextId())

      def listRoots(id: String): ZIO[Any, ToolError, Chunk[Root]] =
        inputs(InputSpec.listRoots(id)).map(_.roots(id))

      /**
       * Answer the whole batch from the replayed responses, or abort with an
       * [[InputRequiredSignal]] carrying every input the client has not
       * answered — one round trip, however many inputs the handler wants.
       */
      def inputs(specs: InputSpec*): ZIO[Any, ToolError, InputResults] =
        val asked = Chunk.fromIterable(specs)
        val missing = asked.filterNot(spec => inputResponses.contains(spec.id))
        if missing.nonEmpty then
          ZIO.die(InputRequiredSignal(missing.map(_.toRequest), nextState.get))
        else
          ZIO.succeed(InputResults(asked.map(spec => spec.id -> inputResponses(spec.id)).toMap))

      private def nextId(): String = s"input-${counter.getAndIncrement()}"

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
    def sample(id: String, prompt: String, maxTokens: Int): ZIO[Any, ToolError, SamplingResult] =
      sample(prompt, maxTokens)
    def elicit(message: String, schema: Json.Obj): ZIO[Any, ToolError, ElicitationResult] =
      ZIO.fail(ToolError("Elicitation not available"))
    def elicit(id: String, message: String, schema: Json.Obj): ZIO[Any, ToolError, ElicitationResult] =
      elicit(message, schema)
    def listRoots: ZIO[Any, ToolError, Chunk[Root]] =
      ZIO.fail(ToolError("Roots not available"))
    def listRoots(id: String): ZIO[Any, ToolError, Chunk[Root]] = listRoots
    def inputs(specs: InputSpec*): ZIO[Any, ToolError, InputResults] =
      ZIO.fail(ToolError("Client input not available"))
