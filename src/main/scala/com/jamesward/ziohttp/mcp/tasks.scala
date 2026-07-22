package com.jamesward.ziohttp.mcp

import zio.*
import zio.json.*
import zio.json.ast.Json

// --- MCP Tasks extension (io.modelcontextprotocol/tasks, SEP-1686) ---

opaque type TaskId = String
object TaskId:
  def apply(s: String): TaskId = s
  def generate: TaskId = java.util.UUID.randomUUID().toString
  extension (t: TaskId) def value: String = t
  given CanEqual[TaskId, TaskId] = CanEqual.derived

/**
 * Lifecycle status of a task. `submitted`, `working`, and `input_required` are
 * non-terminal; `completed`, `failed`, and `cancelled` are terminal.
 */
enum TaskStatus(val wire: String):
  case Submitted     extends TaskStatus("submitted")
  case Working       extends TaskStatus("working")
  case InputRequired extends TaskStatus("input_required")
  case Completed     extends TaskStatus("completed")
  case Failed        extends TaskStatus("failed")
  case Cancelled     extends TaskStatus("cancelled")

  def isTerminal: Boolean = this match
    case Completed | Failed | Cancelled => true
    case _                              => false

object TaskStatus:
  given CanEqual[TaskStatus, TaskStatus] = CanEqual.derived

/**
 * A durable task handle returned when a request is executed asynchronously. The
 * client polls `tasks/get` (no more often than `pollIntervalMs`) until a
 * terminal status, then retrieves the original result.
 */
final case class Task(
  taskId: TaskId,
  status: TaskStatus,
  statusMessage: Option[String],
  createdAt: Long,
  lastUpdatedAt: Long,
  ttlMs: Long,
  pollIntervalMs: Long,
):
  def toJson: Json.Obj =
    val base = Chunk[(String, Json)](
      "taskId"         -> Json.Str(taskId.value),
      "status"         -> Json.Str(status.wire),
      "createdAt"      -> Json.Num(createdAt),
      "lastUpdatedAt"  -> Json.Num(lastUpdatedAt),
      "ttlMs"          -> Json.Num(ttlMs),
      "pollIntervalMs" -> Json.Num(pollIntervalMs),
    )
    Json.Obj(base ++ statusMessage.fold(Chunk.empty[(String, Json)])(m => Chunk("statusMessage" -> Json.Str(m))))

object Task:
  given CanEqual[Task, Task] = CanEqual.derived

  val DefaultTtlMs: Long          = 3600000L
  val DefaultPollIntervalMs: Long = 500L

/**
 * Server-side record for a task: its current handle, the finished result (a
 * `CallToolResult` JSON) or error once terminal, and the running fiber so a
 * `tasks/cancel` can interrupt it.
 */
private[mcp] final case class TaskRecord(
  task: Task,
  result: Option[Json] = None,
  error: Option[ErrorDetail] = None,
  fiber: Option[Fiber.Runtime[Nothing, Unit]] = None,
):
  /** JSON for a `tasks/get` result: the task, plus `result` / `error` when terminal. */
  def toResultJson(serverInfo: Implementation): Json.Obj =
    val fields = Chunk[(String, Json)]("task" -> (task.toJson: Json)) ++
      result.fold(Chunk.empty[(String, Json)])(r => Chunk("result" -> r)) ++
      error.fold(Chunk.empty[(String, Json)]): e =>
        Chunk("error" -> Json.Obj(Chunk(
          "code" -> Json.Num(e.code), "message" -> Json.Str(e.message),
        ) ++ e.data.fold(Chunk.empty[(String, Json)])(d => Chunk("data" -> d))))
    ModernEnvelope.withServerInfo(Json.Obj(fields), serverInfo)

object TaskRecord:
  /** A freshly-created task in the `working` state. */
  def create(now: Long): TaskRecord =
    TaskRecord(Task(
      taskId = TaskId.generate,
      status = TaskStatus.Working,
      statusMessage = None,
      createdAt = now,
      lastUpdatedAt = now,
      ttlMs = Task.DefaultTtlMs,
      pollIntervalMs = Task.DefaultPollIntervalMs,
    ))

  /** The extension identifier, used as the capability key and the request-`_meta`
    * marker that asks for task execution. */
  val ExtensionId: String = McpMeta.Tasks
