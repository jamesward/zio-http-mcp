package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.auth.Principal
import zio.*
import zio.json.*
import zio.json.ast.Json

import java.time.Instant

// --- MCP Tasks extension (io.modelcontextprotocol/tasks, ext-tasks 2026-07-28) ---

opaque type TaskId = String
object TaskId:
  def apply(s: String): TaskId = s
  /** A random (122-bit) id: task ids act as handles to server state, so they
    * must not be guessable. */
  def generate: TaskId = java.util.UUID.randomUUID().toString
  extension (t: TaskId) def value: String = t
  given CanEqual[TaskId, TaskId] = CanEqual.derived

/**
 * Lifecycle status of a task. `working` and `input_required` are non-terminal;
 * `completed`, `failed`, and `cancelled` are terminal — once reached, a task's
 * state does not change.
 */
enum TaskStatus(val wire: String):
  case Working       extends TaskStatus("working")
  case InputRequired extends TaskStatus("input_required")
  case Completed     extends TaskStatus("completed")
  case Failed        extends TaskStatus("failed")
  case Cancelled     extends TaskStatus("cancelled")

  def isTerminal: Boolean = this match
    case Completed | Failed | Cancelled => true
    case Working | InputRequired        => false

object TaskStatus:
  given CanEqual[TaskStatus, TaskStatus] = CanEqual.derived

  def fromWire(s: String): Option[TaskStatus] = values.find(_.wire == s)

/**
 * Whether a tool runs as a task. Task creation is server-directed: the client
 * only declares that it can handle tasks (the `io.modelcontextprotocol/tasks`
 * extension in its per-request capabilities), and the tool's policy decides.
 */
enum TaskExecution:
  /** Always answer synchronously. The default. */
  case Never
  /** Run as a task when the client declared the extension, otherwise synchronously. */
  case WhenSupported
  /** Run as a task; a client that did not declare the extension gets `-32021`. */
  case Required

object TaskExecution:
  given CanEqual[TaskExecution, TaskExecution] = CanEqual.derived

/**
 * How a tool participates in the Tasks extension.
 *
 * @param execution    when the server creates a task for a call
 * @param ttl          how long the server keeps the task, from creation; `None`
 *                     keeps it until the server restarts (`ttlMs: null`)
 * @param pollInterval the polling interval suggested to the client
 */
final case class TaskPolicy(
  execution: TaskExecution,
  ttl: Option[Duration] = TaskPolicy.DefaultTtl,
  pollInterval: Duration = TaskPolicy.DefaultPollInterval,
)

object TaskPolicy:
  given CanEqual[TaskPolicy, TaskPolicy] = CanEqual.derived

  val DefaultTtl: Option[Duration] = Some(1.hour)
  val DefaultPollInterval: Duration = 500.millis

  val never: TaskPolicy = TaskPolicy(TaskExecution.Never)

/**
 * A task's state as `tasks/get` reports it (the spec's `DetailedTask`): the
 * common `Task` fields plus the payload of its current status — the
 * outstanding `inputRequests` while `input_required`, the original request's
 * `result` once `completed`, the JSON-RPC `error` once `failed`.
 *
 * A `CreateTaskResult` carries the same fields (without a payload) under
 * `resultType: "task"`.
 */
final case class McpTask(
  taskId: TaskId,
  status: TaskStatus,
  statusMessage: Option[String],
  createdAt: Instant,
  lastUpdatedAt: Instant,
  ttlMs: Option[Long],
  pollIntervalMs: Option[Long],
  inputRequests: Chunk[InputRequest] = Chunk.empty,
  result: Option[Json.Obj] = None,
  error: Option[ErrorDetail] = None,
):
  def isTerminal: Boolean = status.isTerminal

  /** The flat wire object: task fields, plus the payload for the current status. */
  def toJson: Json.Obj =
    val base = Chunk[(String, Json)](
      "taskId"        -> Json.Str(taskId.value),
      "status"        -> Json.Str(status.wire),
      "createdAt"     -> Json.Str(createdAt.toString),
      "lastUpdatedAt" -> Json.Str(lastUpdatedAt.toString),
      "ttlMs"         -> ttlMs.fold[Json](Json.Null)(Json.Num(_)),
    )
    val optional = Chunk[Option[(String, Json)]](
      statusMessage.map(m => "statusMessage" -> Json.Str(m)),
      pollIntervalMs.map(p => "pollIntervalMs" -> Json.Num(p)),
      Option.when(status == TaskStatus.InputRequired)("inputRequests" -> Json.Obj(inputRequests.map(_.toEntry))),
      result.filter(_ => status == TaskStatus.Completed).map(r => "result" -> r),
      error.filter(_ => status == TaskStatus.Failed).flatMap(e => e.toJsonAST.toOption.map("error" -> _)),
    ).flatten
    Json.Obj(base ++ optional)

object McpTask:
  given CanEqual[McpTask, McpTask] = CanEqual.derived

  /** Read a `CreateTaskResult` / `GetTaskResult` / `notifications/tasks` object. */
  def fromJson(json: Json): Either[String, McpTask] =
    for
      obj    <- json.asObject.toRight("task must be an object")
      id     <- obj.get("taskId").flatMap(_.asString).toRight("task is missing 'taskId'")
      raw    <- obj.get("status").flatMap(_.asString).toRight("task is missing 'status'")
      status <- TaskStatus.fromWire(raw).toRight(s"unknown task status '$raw'")
      created <- instant(obj, "createdAt")
      updated <- instant(obj, "lastUpdatedAt")
    yield McpTask(
      taskId = TaskId(id),
      status = status,
      statusMessage = obj.get("statusMessage").flatMap(_.asString),
      createdAt = created,
      lastUpdatedAt = updated,
      ttlMs = obj.get("ttlMs").flatMap(_.asNumber).map(_.value.longValue),
      pollIntervalMs = obj.get("pollIntervalMs").flatMap(_.asNumber).map(_.value.longValue),
      inputRequests = obj.get("inputRequests").fold(Chunk.empty)(InputRequest.parseAll),
      result = obj.get("result").flatMap(_.asObject),
      error = obj.get("error").flatMap(_.as[ErrorDetail].toOption),
    )

  private def instant(obj: Json.Obj, field: String): Either[String, Instant] =
    obj.get(field).flatMap(_.asString).toRight(s"task is missing '$field'").flatMap: s =>
      try Right(Instant.parse(s))
      catch case _: java.time.format.DateTimeParseException => Left(s"task '$field' is not an ISO 8601 timestamp: $s")

/**
 * Who a task belongs to: the verified caller that created it. Every task
 * request is checked against it, so one caller cannot read, answer, or cancel
 * another's task even with its id. `None` when the server has no `.auth(...)`.
 */
final case class McpTaskOwner(issuer: Option[String], subject: Option[String], clientId: Option[String])

object McpTaskOwner:
  given CanEqual[McpTaskOwner, McpTaskOwner] = CanEqual.derived

  private[mcp] def of(principal: Option[Principal]): Option[McpTaskOwner] =
    principal.map(p => McpTaskOwner(p.issuer, p.subject, p.clientId))

/**
 * What an [[McpTaskStore]] keeps for one task: its current state, who owns it,
 * when it expires, and the answers the client has sent for its input requests.
 * Plain data, so a store can serialize it anywhere.
 */
final case class McpTaskRecord(
  task: McpTask,
  owner: Option[McpTaskOwner],
  expiresAt: Option[Instant],
  inputResponses: Map[String, Json] = Map.empty,
):
  def isExpired(now: Instant): Boolean = expiresAt.exists(now.isAfter)

object McpTaskRecord:
  given CanEqual[McpTaskRecord, McpTaskRecord] = CanEqual.derived

/**
 * Where the Tasks extension keeps task state. [[McpTaskStore.inMemory]] is the
 * default; implement this over shared storage (Redis, a database) so that any
 * server replica can answer `tasks/get`, `tasks/update`, and `tasks/cancel`.
 *
 * The running handler itself stays in the process that started it. A shared
 * store still lets other replicas serve a task's requests: a handler waiting
 * for input re-reads the store at the task's poll interval, so an answer that
 * lands on another replica resumes it, and a cancellation recorded elsewhere
 * keeps the task `cancelled` whatever its handler does next.
 *
 * Store failures are defects: the request that hit one fails with an internal
 * error.
 */
trait McpTaskStore:
  /** Record a newly created task. */
  def create(record: McpTaskRecord): UIO[Unit]

  /** The task, or `None` if the store does not have it. Returning an expired
    * record is fine: the server checks `expiresAt` and deletes it. */
  def get(taskId: TaskId): UIO[Option[McpTaskRecord]]

  /**
   * Atomically replace the task with `f` applied to its current record, and
   * return what was stored; `None` if the store does not have it. `f` is pure,
   * so a store with optimistic concurrency may apply it more than once.
   */
  def update(taskId: TaskId)(f: McpTaskRecord => McpTaskRecord): UIO[Option[McpTaskRecord]]

  /** Forget the task. */
  def delete(taskId: TaskId): UIO[Unit]

object McpTaskStore:
  /** A store in a `Ref` for this process. Expired tasks are dropped whenever a
    * task is created; tasks with no TTL stay until the process ends. */
  def inMemory: UIO[McpTaskStore] =
    Ref.make(Map.empty[TaskId, McpTaskRecord]).map(InMemory(_))

  private final class InMemory(ref: Ref[Map[TaskId, McpTaskRecord]]) extends McpTaskStore:
    def create(record: McpTaskRecord): UIO[Unit] =
      Clock.instant.flatMap: now =>
        ref.update(_.filterNot((_, r) => r.isExpired(now)).updated(record.task.taskId, record))

    def get(taskId: TaskId): UIO[Option[McpTaskRecord]] =
      ref.get.map(_.get(taskId))

    def update(taskId: TaskId)(f: McpTaskRecord => McpTaskRecord): UIO[Option[McpTaskRecord]] =
      ref.modify: m =>
        m.get(taskId) match
          case Some(r) =>
            val next = f(r)
            (Some(next), m.updated(taskId, next))
          case None => (None, m)

    def delete(taskId: TaskId): UIO[Unit] =
      ref.update(_ - taskId)

/**
 * The Tasks extension (`io.modelcontextprotocol/tasks`, 2026-07-28). Register
 * it with `withExtensions` to enable tasks: the server then advertises the
 * extension, serves `tasks/get`, `tasks/update`, and `tasks/cancel`, and runs
 * tools that opted in with `McpTool.taskExecution` as tasks for clients that
 * declare the extension.
 *
 * {{{
 * for
 *   tasks <- McpTasks.inMemory
 * yield McpServer("my-server", "1.0.0").withExtensions(tasks).tool(deepResearch)
 * }}}
 */
object McpTasks:
  val Id: McpExtensionId = McpExtensionId.fromValid(McpMeta.Tasks)

  /** The extension, keeping tasks in `store`. */
  def apply(store: McpTaskStore): UIO[McpExtensions[Any]] =
    for
      fibers  <- Ref.make(Map.empty[TaskId, Fiber.Runtime[Nothing, Unit]])
      signals <- Ref.make(Map.empty[TaskId, Promise[Nothing, Unit]])
    yield McpExtensions.trustedVertical(McpServerExtension(
      Id, Chunk.empty, Settings(TaskRuntime(store, fibers, signals)),
    ))

  /** The extension, keeping tasks in memory ([[McpTaskStore.inMemory]]). */
  def inMemory: UIO[McpExtensions[Any]] =
    McpTaskStore.inMemory.flatMap(apply)

  /** No extension-specific settings are defined; an empty object means support.
    * Carries the runtime so the server can find it in its registry. */
  private final class Settings(val runtime: TaskRuntime) extends McpExtensionSettings[Any]:
    def resolve(ctx: McpRequestContext): UIO[Json] = ZIO.succeed(Json.Obj())

  /** The runtime of the tasks extension in `extensions`, if it is registered. */
  private[mcp] def runtime(extensions: McpExtensions[?]): Option[TaskRuntime] =
    extensions.values.map(_.settings).collectFirst { case s: Settings => s.runtime }

  private[mcp] val ExtensionId: String = McpMeta.Tasks

  /** `error.data` for a `-32021` naming this extension. */
  private[mcp] val requiredCapabilityData: Json =
    Json.Obj("requiredCapabilities" -> Json.Obj("extensions" -> Json.Obj(ExtensionId -> Json.Obj())))

  /** Whether the client declared the extension on this request. */
  private[mcp] def clientDeclares(params: Option[Json.Obj]): Boolean =
    McpMeta.raw(McpMeta.of(params), McpMeta.ClientCapabilities)
      .flatMap(_.asObject)
      .flatMap(_.get("extensions"))
      .flatMap(_.asObject)
      .exists(_.get(ExtensionId).isDefined)

/**
 * The task runtime behind the `tasks/...` methods: creating a task and running a
 * tool on it, recording its outcome, the input round trip, cancellation, and
 * TTL expiry. Task state lives in the [[McpTaskStore]]; what cannot leave this
 * process — the running fibers, and the signals that wake a handler waiting
 * for input — lives here.
 */
private[mcp] final class TaskRuntime(
  store: McpTaskStore,
  fibers: Ref[Map[TaskId, Fiber.Runtime[Nothing, Unit]]],
  signals: Ref[Map[TaskId, Promise[Nothing, Unit]]],
):

  /**
   * Create a `working` task and run `work` on a daemon fiber that records its
   * outcome. The task is in the store before this returns, so a `tasks/get`
   * for it resolves as soon as the client has the id.
   *
   * `work` gets the task's id so it can build a context bound to it. A result
   * — including one with `isError: true` — completes the task; a defect fails
   * it with `-32603`; interruption leaves the state `tasks/cancel` set.
   */
  def start[R](
    policy: TaskPolicy,
    owner: Option[McpTaskOwner],
    work: TaskId => ZIO[R, Nothing, CallToolResult],
    encode: CallToolResult => Json,
  ): URIO[R, McpTask] =
    for
      now    <- Clock.instant
      task    = McpTask(
                  taskId = TaskId.generate,
                  status = TaskStatus.Working,
                  statusMessage = None,
                  createdAt = now,
                  lastUpdatedAt = now,
                  ttlMs = policy.ttl.map(_.toMillis),
                  pollIntervalMs = Some(policy.pollInterval.toMillis),
                )
      id      = task.taskId
      _      <- store.create(McpTaskRecord(task, owner, policy.ttl.map(d => now.plusMillis(d.toMillis))))
      outcome = work(id).foldCauseZIO(
                  cause =>
                    if cause.isInterruptedOnly then ZIO.unit
                    else
                      val message = cause.dieOption.map(t => Option(t.getMessage).getOrElse(t.toString))
                        .getOrElse("Tool execution failed")
                      finish(id, TaskStatus.Failed, Some(s"Tool execution failed: $message")):
                        _.copy(error = Some(ErrorDetail(ErrorCode.InternalError.code, message)))
                  ,
                  result =>
                    val json = encode(result).asObject.getOrElse(Json.Obj())
                    finish(id, TaskStatus.Completed, None)(_.copy(result = Some(json))),
                ).ensuring(forget(id))
      fiber  <- outcome.forkDaemon
      _      <- fibers.update(_.updated(id, fiber))
      // The work may already have finished, and forgotten, before it was registered.
      _      <- fiber.poll.flatMap(done => ZIO.when(done.isDefined)(forget(id)))
    yield task

  private def forget(taskId: TaskId): UIO[Unit] =
    fibers.update(_ - taskId) *> signals.update(_ - taskId)

  /** Move a non-terminal task to a terminal status. A task that is already
    * terminal (e.g. cancelled while the work finished) keeps its state. */
  private def finish(taskId: TaskId, status: TaskStatus, message: Option[String])(
    payload: McpTask => McpTask,
  ): UIO[Unit] =
    Clock.instant.flatMap: now =>
      store.update(taskId): r =>
        if r.task.isTerminal then r
        else r.copy(task = payload(r.task.copy(
          status = status, statusMessage = message.orElse(r.task.statusMessage),
          lastUpdatedAt = now, inputRequests = Chunk.empty,
        )))
      .unit

  /**
   * The caller's task, or `None` when there is no such task, it has expired, or
   * it belongs to someone else. The three are indistinguishable on purpose: a
   * caller must not learn that someone else's task id exists. An expired task
   * is deleted, and its work stopped.
   */
  def lookup(taskId: TaskId, owner: Option[McpTaskOwner]): UIO[Option[McpTaskRecord]] =
    for
      now    <- Clock.instant
      found  <- store.get(taskId)
      live   <- found match
                  case Some(r) if r.isExpired(now) =>
                    store.delete(taskId) *> interrupt(taskId).as(None)
                  case other => ZIO.succeed(other)
    yield live.filter(_.owner == owner)

  /**
   * Accept answers for a task's outstanding input requests. Answers for keys
   * that are not outstanding are ignored; once none remain outstanding the task
   * returns to `working`, and a handler waiting on them wakes up.
   */
  def update(taskId: TaskId, responses: Map[String, Json]): UIO[Unit] =
    Clock.instant.flatMap: now =>
      store.update(taskId): r =>
        val outstanding = r.task.inputRequests.map(_.id).toSet
        val accepted = responses.filter((key, _) => outstanding.contains(key))
        if r.task.isTerminal || accepted.isEmpty then r
        else
          val remaining = r.task.inputRequests.filterNot(req => accepted.contains(req.id))
          val status = if remaining.isEmpty then TaskStatus.Working else TaskStatus.InputRequired
          r.copy(
            task = r.task.copy(status = status, inputRequests = remaining, lastUpdatedAt = now),
            inputResponses = r.inputResponses ++ accepted,
          )
    *> wake(taskId)

  /** Mark the task `cancelled`, unless already terminal, and stop its work if
    * it runs in this process. */
  def cancel(taskId: TaskId): UIO[Unit] =
    Clock.instant.flatMap: now =>
      store.update(taskId): r =>
        if r.task.isTerminal then r
        else r.copy(task = r.task.copy(
          status = TaskStatus.Cancelled, statusMessage = Some("Cancelled by the client"),
          lastUpdatedAt = now, inputRequests = Chunk.empty,
        ))
    *> interrupt(taskId) *> wake(taskId)

  // Interrupt in the background: cancellation is cooperative, and the ack must
  // not wait for a handler that is slow to stop.
  private def interrupt(taskId: TaskId): UIO[Unit] =
    fibers.get.flatMap(m => ZIO.foreachDiscard(m.get(taskId))(_.interrupt.forkDaemon))

  private def wake(taskId: TaskId): UIO[Unit] =
    signals.modify(m => (m.get(taskId), m - taskId)).flatMap(ZIO.foreachDiscard(_)(_.succeed(())))

  /**
   * The context a tool runs with inside a task. There is no response stream to
   * carry notifications on, so `log` is dropped and `progress` becomes the
   * task's `statusMessage`. Input is asked for through the task itself: the
   * handler blocks, the task moves to `input_required` with the request in
   * `inputRequests`, and it resumes when `tasks/update` delivers the answer —
   * a live fiber, so unlike an MRTR retry nothing is replayed.
   */
  def context(
    taskId: TaskId,
    callerPrincipal: Option[Principal],
    callerPathParams: Map[String, String],
    declaredCapabilities: Option[Json.Obj],
    recheckEvery: Duration,
  ): UIO[McpToolContext] =
    Ref.make(0).map: inputIds =>
      new McpToolContext.InputDriven(inputIds):
        override val principal: Option[Principal] = callerPrincipal
        override val pathParams: Map[String, String] = callerPathParams
        override val clientCapabilities: Option[Json.Obj] = declaredCapabilities

        def log(level: com.jamesward.ziohttp.mcp.LogLevel, message: String): UIO[Unit] = ZIO.unit

        def progress(current: Double, total: Double, message: Option[String]): UIO[Unit] =
          val text = message.getOrElse(s"${TaskRuntime.fmt(current)}/${TaskRuntime.fmt(total)}")
          Clock.instant.flatMap: now =>
            store.update(taskId): r =>
              if r.task.isTerminal then r
              else r.copy(task = r.task.copy(statusMessage = Some(text), lastUpdatedAt = now))
            .unit

        def inputs(specs: InputSpec*): ZIO[Any, ToolError, InputResults] =
          awaitInputs(taskId, Chunk.fromIterable(specs), recheckEvery)

  /**
   * Answer `specs` from what the client has sent, or publish the missing ones
   * as `inputRequests` and wait — for `tasks/update` in this process to wake
   * the handler, or, should the answer land on another replica, until the next
   * re-read of the store. A task that has ended or gone stops the handler.
   */
  private def awaitInputs(
    taskId: TaskId,
    specs: Chunk[InputSpec],
    recheckEvery: Duration,
  ): ZIO[Any, ToolError, InputResults] =
    for
      fresh   <- Promise.make[Nothing, Unit]
      // Register before reading, so an answer arriving in between still wakes us.
      signal  <- signals.modify: m =>
                   m.get(taskId).fold((fresh, m.updated(taskId, fresh)))(existing => (existing, m))
      now     <- Clock.instant
      current <- store.update(taskId): r =>
                   val missing = specs.filterNot(spec => r.inputResponses.contains(spec.id))
                   if r.task.isTerminal || missing.isEmpty then r
                   else
                     // Union with what is already outstanding, so concurrent asks
                     // do not hide each other's requests.
                     val asked = missing.map(_.toRequest)
                     val known = r.task.inputRequests.map(_.id).toSet
                     val added = asked.filterNot(req => known.contains(req.id))
                     if added.isEmpty && r.task.status == TaskStatus.InputRequired then r
                     else r.copy(task = r.task.copy(
                       status = TaskStatus.InputRequired,
                       inputRequests = r.task.inputRequests ++ added,
                       lastUpdatedAt = now,
                     ))
      out     <- current match
                   case Some(r) if !r.task.isTerminal =>
                     if specs.forall(spec => r.inputResponses.contains(spec.id)) then
                       ZIO.succeed(InputResults(specs.map(spec => spec.id -> r.inputResponses(spec.id)).toMap))
                     else signal.await.timeout(recheckEvery) *> awaitInputs(taskId, specs, recheckEvery)
                   case _ => ZIO.interrupt
    yield out

private object TaskRuntime:
  def fmt(d: Double): String =
    if d.isWhole then d.toLong.toString else d.toString
