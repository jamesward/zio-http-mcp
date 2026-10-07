package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.auth.*
import com.jamesward.ziohttp.mcp.client.*
import zio.*
import zio.http.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*
import zio.test.TestAspect.*
import zio.schema.{Schema, derived}

import java.time.Instant

given canEqualStatusTasks: CanEqual[Status, Status] = CanEqual.derived

/**
 * The Tasks extension (`io.modelcontextprotocol/tasks`, ext-tasks 2026-07-28),
 * on the wire and through [[McpClient]]: server-directed task creation gated on
 * the client's declared capability, the flat `CreateTaskResult` / `tasks/get`
 * shapes, `completed` / `failed` / `cancelled`, `input_required` answered with
 * `tasks/update`, `statusMessage` from progress, TTL expiry, `-32021`, routing
 * headers, scope enforcement, and binding a task to the caller that created it.
 */
object TasksSpec extends ZIOSpecDefault:

  private val Modern = ProtocolVersion.V2026_07_28.wire

  private val confirmSchema = Json.Obj(
    "type"       -> Json.Str("object"),
    "properties" -> Json.Obj("confirm" -> Json.Obj("type" -> Json.Str("boolean"))),
  )

  case class ResearchInput(topic: String) derives Schema

  // The README example.
  val deepResearch: McpToolHandler = McpTool("deep_research")
    .description("Researches a topic. Slow, so it runs as a task.")
    .taskExecution(TaskExecution.WhenSupported)
    .handleWithContext[Any, ToolError, ResearchInput, String]: (in, ctx) =>
      ZIO.foreachDiscard(1 to 3): step =>
        ctx.progress(step, 3, Some(s"researching ${in.topic} ($step/3)")) *> ZIO.sleep(100.millis)
      .as(s"Research on ${in.topic}: done")

  val quickTool: McpToolHandler = McpTool("quick")
    .taskExecution(TaskExecution.WhenSupported)
    .handle(ZIO.succeed("quick done"))

  val syncTool: McpToolHandler = McpTool("sync")
    .handle(ZIO.succeed("sync done"))

  val requiredTool: McpToolHandler = McpTool("required")
    .taskExecution(TaskExecution.Required)
    .handle(ZIO.succeed("required done"))

  val slowTool: McpToolHandler = McpTool("slow")
    .taskExecution(TaskExecution.WhenSupported)
    .handle(ZIO.sleep(30.seconds).as("slow done"))

  val erroringTool: McpToolHandler = McpTool("erroring")
    .taskExecution(TaskExecution.WhenSupported)
    .handle[Any, ToolError, String](ZIO.fail(ToolError("invalid input")))

  // A progress update the test can observe before the task finishes.
  val progressTool: McpToolHandler = McpTool("progress")
    .taskExecution(TaskExecution.WhenSupported)
    .handleWithContext[Any, Nothing, String]: ctx =>
      ctx.progress(1, 2, Some("halfway")) *> ZIO.sleep(30.seconds).as("never")

  val shortLivedTool: McpToolHandler = McpTool("short_lived")
    .taskExecution(TaskExecution.WhenSupported, ttl = Some(300.millis))
    .handle(ZIO.succeed("gone soon"))

  // The README example.
  val bookFlight: McpToolHandler = McpTool("book_flight")
    .description("Books a flight once the user confirms")
    .taskExecution(TaskExecution.WhenSupported)
    .handleWithContext[Any, ToolError, String]: ctx =>
      ctx.elicit("confirm", "Book the flight for $420?", confirmSchema).map: answer =>
        if answer.content.flatMap(_.get("confirm")).contains(Json.Bool(true)) then "Booked"
        else s"Not booked (${answer.action})"

  /**
   * A custom [[McpTaskStore]]: the in-memory one, plus a record of every task
   * created, so a test can find a task whose id the client never handed back.
   */
  final class RecordingStore(delegate: McpTaskStore, created: Ref[Chunk[TaskId]]) extends McpTaskStore:
    def create(record: McpTaskRecord): UIO[Unit] =
      created.update(_ :+ record.task.taskId) *> delegate.create(record)
    def get(taskId: TaskId): UIO[Option[McpTaskRecord]] = delegate.get(taskId)
    def update(taskId: TaskId)(f: McpTaskRecord => McpTaskRecord): UIO[Option[McpTaskRecord]] =
      delegate.update(taskId)(f)
    def delete(taskId: TaskId): UIO[Unit] = delegate.delete(taskId)
    def all: UIO[Chunk[McpTaskRecord]] =
      created.get.flatMap(ids => ZIO.foreach(ids)(get)).map(_.flatten)

  private def unsafeRun[A](effect: UIO[A]): A =
    Unsafe.unsafe(implicit unsafe => Runtime.default.unsafe.run(effect).getOrThrowFiberFailure())

  val store: RecordingStore =
    unsafeRun(McpTaskStore.inMemory.zipWith(Ref.make(Chunk.empty[TaskId]))(RecordingStore(_, _)))

  // The README example: tasks are enabled by registering the extension.
  val tasks: McpExtensions[Any] = unsafeRun(McpTasks(store))

  val server = McpServer("tasks-server", "1.0.0")
    .withExtensions(tasks)
    .tool(deepResearch).tool(quickTool).tool(syncTool).tool(requiredTool).tool(slowTool)
    .tool(erroringTool).tool(progressTool).tool(shortLivedTool).tool(bookFlight)

  // --- wire helpers ---

  private def meta(withTasks: Boolean): Json.Obj =
    val extensions = if withTasks then Json.Obj(McpMeta.Tasks -> Json.Obj()) else Json.Obj()
    Json.Obj(
      McpMeta.ProtocolVersion    -> Json.Str(Modern),
      McpMeta.ClientCapabilities -> Json.Obj("extensions" -> extensions),
    )

  private def body(id: Int, method: String, params: Json.Obj, withTasks: Boolean): String =
    val full = Json.Obj(params.fields :+ ("_meta" -> (meta(withTasks): Json)))
    s"""{"jsonrpc":"2.0","id":$id,"method":"$method","params":${(full: Json).toJson}}"""

  private def post(
    port: Int,
    method: String,
    params: Json.Obj,
    name: Option[String],
    withTasks: Boolean = true,
    token: Option[String] = None,
    path: String = "mcp",
  ): ZIO[Client & Scope, Throwable, Response] =
    val url = URL.decode(s"http://localhost:$port/$path").toOption.get
    val base = Request.post(url, Body.fromString(body(1, method, params, withTasks)))
      .addHeader(Header.ContentType(MediaType.application.json))
      .addHeader("accept", "application/json, text/event-stream")
      .addHeader(Negotiation.ProtocolVersionHeader, Modern)
      .addHeader(Negotiation.MethodHeader, method)
    val named = name.fold(base)(n => base.addHeader(Negotiation.NameHeader, n))
    ZClient.batched(token.fold(named)(t => named.addHeader(Header.Authorization.Bearer(t))))

  private def call(port: Int, tool: String, withTasks: Boolean = true, token: Option[String] = None) =
    post(port, "tools/call", Json.Obj("name" -> Json.Str(tool), "arguments" -> Json.Obj()), Some(tool), withTasks, token)

  private def taskRequest(port: Int, method: String, taskId: String, withTasks: Boolean = true,
                          token: Option[String] = None, extra: Json.Obj = Json.Obj()) =
    post(port, method, Json.Obj(Chunk("taskId" -> (Json.Str(taskId): Json)) ++ extra.fields), Some(taskId), withTasks, token)

  private def json(r: Response): ZIO[Any, Throwable, Json.Obj] =
    r.body.asString.flatMap(s => ZIO.fromEither(s.fromJson[Json.Obj]).mapError(e => RuntimeException(s"$e: $s")))

  private def result(b: Json.Obj): Json.Obj = b.get("result").flatMap(_.asObject).getOrElse(Json.Obj())
  private def str(o: Json.Obj, key: String): Option[String] = o.get(key).flatMap(_.asString)
  private def errorCode(b: Json.Obj): Option[Int] =
    b.get("error").flatMap(_.asObject).flatMap(_.get("code")).flatMap(_.asNumber).map(_.value.intValue)
  /** The first text content of a `CallToolResult` object. */
  private def contentText(r: Json.Obj): Option[String] =
    r.get("content").flatMap(_.asArray).flatMap(_.headOption)
      .flatMap(_.asObject).flatMap(_.get("text")).flatMap(_.asString)
  /** The first text content of a completed task's inlined `result`. */
  private def resultText(task: Json.Obj): Option[String] =
    task.get("result").flatMap(_.asObject).flatMap(contentText)

  /** Create a task for `tool` and return its id. */
  private def createTask(port: Int, tool: String, token: Option[String] = None) =
    call(port, tool, token = token).flatMap(json).map(b => str(result(b), "taskId").getOrElse(""))

  private def getTask(port: Int, taskId: String, token: Option[String] = None) =
    taskRequest(port, "tasks/get", taskId, token = token).flatMap(json)

  /** Poll `tasks/get` until `done` holds for the result. */
  private def pollUntil(port: Int, taskId: String)(done: Json.Obj => Boolean) =
    getTask(port, taskId).map(result).repeat(Schedule.recurUntil(done) && Schedule.spaced(50.millis)).map(_._1)

  private def isTerminal(task: Json.Obj): Boolean =
    str(task, "status").exists(Set("completed", "failed", "cancelled"))

  // --- auth fixtures ---

  private val resourceUri = ResourceUri.parse("http://localhost:0/mcp").toOption.get
  private val adminScope = OauthScope("admin")

  private def principal(subject: String, scopes: OauthScope*): Principal =
    Principal(
      subject = Some(subject), clientId = Some("client"), scopes = scopes.toSet,
      audience = Set(resourceUri.value), issuer = Some("https://auth.example.com"),
      expiresAt = Some(Instant.now().plusSeconds(3600)), raw = subject, claims = Json.Obj(),
    )

  // "alice" and "bob" are two callers; only "admin" holds the admin scope.
  private val verifier = TokenVerifier.fromFunction[Any]:
    case "alice" => ZIO.succeed(principal("alice"))
    case "bob"   => ZIO.succeed(principal("bob"))
    case "admin" => ZIO.succeed(principal("admin", adminScope))
    case _       => ZIO.fail(AuthError.Invalid("unknown token"))

  private val adminTool: McpToolHandler = McpTool("admin_task")
    .requireScopes(adminScope)
    .taskExecution(TaskExecution.WhenSupported)
    .handle(ZIO.succeed("admin done"))

  private val authedServer = McpServer("tasks-auth", "1.0.0")
    .withExtensions(unsafeRun(McpTasks.inMemory))
    .tool(slowTool).tool(adminTool)
    .auth(McpAuth(
      resourceUri = Some(resourceUri),
      authorizationServers = NonEmptyChunk(AuthorizationServer("https://auth.example.com")),
      verifier = verifier,
    ))

  // --- client helpers ---

  private def connect(port: Int, onInput: Option[InputRequest => IO[McpClientError, Json]] = None) =
    McpClient.connect(McpClientConfig(s"http://localhost:$port/mcp", onInputRequest = onInput))

  private def text(r: CallToolResult): String =
    r.content.collect { case ToolContent.Text(t, _) => t }.mkString

  private val acceptConfirm: InputRequest => IO[McpClientError, Json] = _ =>
    ZIO.succeed(Json.Obj("action" -> Json.Str("accept"), "content" -> Json.Obj("confirm" -> Json.Bool(true))))

  override def spec =
    suite("TasksSpec")(

      suite("capability")(
        test("server/discover advertises the extension when a tool runs as a task"):
          for
            port <- Server.install(server.routes)
            b    <- post(port, "server/discover", Json.Obj(), None).flatMap(json)
          yield
            val ext = result(b).get("capabilities").flatMap(_.asObject).flatMap(_.get("extensions")).flatMap(_.asObject)
            assertTrue(ext.flatMap(_.get(McpMeta.Tasks)).isDefined)
        ,
        test("without the extension registered there are no tasks, even for task tools"):
          for
            port <- Server.install(McpServer("plain", "1.0.0").tool(syncTool).tool(quickTool).tool(requiredTool).routes)
            b    <- post(port, "server/discover", Json.Obj(), None).flatMap(json)
            sync <- call(port, "quick").flatMap(json)
            req  <- call(port, "required", withTasks = false).flatMap(json)
            get  <- post(port, "tasks/get", Json.Obj("taskId" -> Json.Str("t")), Some("t"))
          yield
            val ext = result(b).get("capabilities").flatMap(_.asObject).flatMap(_.get("extensions")).flatMap(_.asObject)
            assertTrue(
              ext.flatMap(_.get(McpMeta.Tasks)).isEmpty,
              str(result(sync), "resultType").contains("complete"),
              contentText(result(req)).contains("required done"),
              get.status == Status.NotFound,
            )
        ,
        test("a legacy session is not offered the extension"):
          ZIO.scoped:
            for
              port   <- Server.install(server.routes)
              client <- McpClient.connect(McpClientConfig(s"http://localhost:$port/mcp",
                          preferredVersion = ProtocolVersion.V2025_11_25))
            yield assertTrue(!client.serverCapabilities.extensions.exists(_.contains(McpMeta.Tasks)))
        ,
        test("the tasks extension combines with others into one registry"):
          val other = McpExtensions(McpServerExtension.capability(McpExtensionId.parse("dev.example/other").toOption.get, Json.Obj()))
          val combined = other.flatMap(_ ++ tasks)
          assertTrue(
            combined.map(_.values.map(_.id.value).toSet) == Right(Set("dev.example/other", McpMeta.Tasks)),
            (tasks ++ tasks).isLeft,
          )
        ,
      ),

      suite("task creation is server-directed")(
        test("a declaring client gets a flat CreateTaskResult"):
          for
            port <- Server.install(server.routes)
            resp <- call(port, "quick")
            b    <- json(resp)
          yield
            val r = result(b)
            assertTrue(
              resp.status == Status.Ok,
              str(r, "resultType").contains("task"),
              str(r, "taskId").exists(_.nonEmpty),
              str(r, "status").contains("working"),
              str(r, "createdAt").exists(s => scala.util.Try(Instant.parse(s)).isSuccess),
              str(r, "lastUpdatedAt").exists(s => scala.util.Try(Instant.parse(s)).isSuccess),
              r.get("ttlMs").flatMap(_.asNumber).map(_.value.longValue).contains(3600000L),
              r.get("pollIntervalMs").flatMap(_.asNumber).map(_.value.longValue).contains(500L),
              r.get("task").isEmpty,
            )
        ,
        test("a client that did not declare the extension gets the result synchronously"):
          for
            port <- Server.install(server.routes)
            b    <- call(port, "quick", withTasks = false).flatMap(json)
          yield assertTrue(
            str(result(b), "resultType").contains("complete"),
            contentText(result(b)).contains("quick done"),
          )
        ,
        test("a tool that does not opt in answers synchronously even to a declaring client"):
          for
            port <- Server.install(server.routes)
            b    <- call(port, "sync").flatMap(json)
          yield assertTrue(
            str(result(b), "resultType").contains("complete"),
            contentText(result(b)).contains("sync done"),
          )
        ,
        test("a Required tool rejects a non-declaring client with -32021 naming the extension"):
          for
            port <- Server.install(server.routes)
            b    <- call(port, "required", withTasks = false).flatMap(json)
          yield
            val required = b.get("error").flatMap(_.asObject).flatMap(_.get("data")).flatMap(_.asObject)
              .flatMap(_.get("requiredCapabilities")).flatMap(_.asObject)
              .flatMap(_.get("extensions")).flatMap(_.asObject)
            assertTrue(
              errorCode(b).contains(ErrorCode.MissingRequiredClientCapability.code),
              required.flatMap(_.get(McpMeta.Tasks)).isDefined,
            )
        ,
        test("a legacy session runs a task tool synchronously"):
          for
            port <- Server.install(server.routes)
            out  <- ZIO.scoped:
                      McpClient.connect(McpClientConfig(s"http://localhost:$port/mcp",
                        preferredVersion = ProtocolVersion.V2025_11_25)).flatMap(_.callTool("quick"))
          yield assertTrue(text(out) == "quick done")
        ,
      ),

      suite("tasks/get")(
        test("polls to completed, with the tool result inlined and resultType complete"):
          for
            port  <- Server.install(server.routes)
            tid   <- createTask(port, "quick")
            task  <- pollUntil(port, tid)(isTerminal)
          yield assertTrue(
            str(task, "resultType").contains("complete"),
            str(task, "taskId").contains(tid),
            str(task, "status").contains("completed"),
            resultText(task).contains("quick done"),
          )
        ,
        test("a tool error is completed with isError, not failed"):
          for
            port <- Server.install(server.routes)
            tid  <- createTask(port, "erroring")
            task <- pollUntil(port, tid)(isTerminal)
          yield assertTrue(
            str(task, "status").contains("completed"),
            task.get("result").flatMap(_.asObject).flatMap(_.get("isError")).contains(Json.Bool(true)),
            task.get("error").isEmpty,
          )
        ,
        test("progress becomes the task's statusMessage"):
          for
            port <- Server.install(server.routes)
            tid  <- createTask(port, "progress")
            task <- pollUntil(port, tid)(t => str(t, "statusMessage").isDefined)
          yield assertTrue(
            str(task, "status").contains("working"),
            str(task, "statusMessage").contains("halfway"),
          )
        ,
        test("an unknown task is -32602"):
          for
            port <- Server.install(server.routes)
            b    <- getTask(port, "nope")
          yield assertTrue(errorCode(b).contains(ErrorCode.InvalidParams.code))
        ,
        test("a non-declaring client gets -32021"):
          for
            port <- Server.install(server.routes)
            tid  <- createTask(port, "quick")
            b    <- taskRequest(port, "tasks/get", tid, withTasks = false).flatMap(json)
          yield assertTrue(errorCode(b).contains(ErrorCode.MissingRequiredClientCapability.code))
        ,
        test("an Mcp-Name that does not match the taskId is -32020"):
          for
            port <- Server.install(server.routes)
            tid  <- createTask(port, "quick")
            resp <- post(port, "tasks/get", Json.Obj("taskId" -> Json.Str(tid)), Some("other-task"))
            b    <- json(resp)
          yield assertTrue(
            resp.status == Status.BadRequest,
            errorCode(b).contains(ErrorCode.HeaderMismatch.code),
          )
        ,
        test("an expired task is gone"):
          for
            port <- Server.install(server.routes)
            tid  <- createTask(port, "short_lived")
            done <- pollUntil(port, tid)(isTerminal)
            _    <- ZIO.sleep(500.millis)
            b    <- getTask(port, tid)
          yield assertTrue(
            done.get("ttlMs").flatMap(_.asNumber).map(_.value.longValue).contains(300L),
            errorCode(b).contains(ErrorCode.InvalidParams.code),
          )
        ,
        test("the stateless routes run tasks too"):
          for
            port <- Server.install(server.statelessRoutes)
            tid  <- createTask(port, "quick")
            task <- pollUntil(port, tid)(isTerminal)
          yield assertTrue(resultText(task).contains("quick done"))
        ,
      ),

      suite("input_required")(
        test("an elicitation surfaces in inputRequests and resumes on tasks/update"):
          for
            port    <- Server.install(server.routes)
            tid     <- createTask(port, "book_flight")
            waiting <- pollUntil(port, tid)(t => str(t, "status").contains("input_required"))
            ack     <- taskRequest(port, "tasks/update", tid, extra = Json.Obj("inputResponses" -> Json.Obj(
                         "confirm" -> Json.Obj("action" -> Json.Str("accept"), "content" -> Json.Obj("confirm" -> Json.Bool(true))),
                       ))).flatMap(json)
            done    <- pollUntil(port, tid)(isTerminal)
          yield
            val request = waiting.get("inputRequests").flatMap(_.asObject).flatMap(_.get("confirm")).flatMap(_.asObject)
            assertTrue(
              str(request.getOrElse(Json.Obj()), "method").contains("elicitation/create"),
              str(result(ack), "resultType").contains("complete"),
              result(ack).fields.map(_._1).toSet.subsetOf(Set("resultType", "_meta")),
              resultText(done).contains("Booked"),
            )
        ,
        test("answers for keys that are not outstanding are ignored"):
          for
            port  <- Server.install(server.routes)
            tid   <- createTask(port, "book_flight")
            _     <- pollUntil(port, tid)(t => str(t, "status").contains("input_required"))
            _     <- taskRequest(port, "tasks/update", tid, extra = Json.Obj("inputResponses" -> Json.Obj(
                       "unknown" -> Json.Obj("action" -> Json.Str("accept")),
                     )))
            still <- getTask(port, tid).map(result)
          yield assertTrue(str(still, "status").contains("input_required"))
        ,
      ),

      suite("tasks/cancel")(
        test("acknowledges empty and the task ends cancelled"):
          for
            port   <- Server.install(server.routes)
            tid    <- createTask(port, "slow")
            ack    <- taskRequest(port, "tasks/cancel", tid).flatMap(json)
            task   <- getTask(port, tid).map(result)
          yield assertTrue(
            str(result(ack), "resultType").contains("complete"),
            str(task, "status").contains("cancelled"),
          )
        ,
      ),

      suite("subscriptions/listen")(
        test("asking for task notifications without the capability is -32021"):
          for
            port <- Server.install(server.routes)
            b    <- post(port, "subscriptions/listen",
                      Json.Obj("notifications" -> Json.Obj("taskIds" -> Json.Arr(Json.Str("t")))), None,
                      withTasks = false).flatMap(json)
          yield assertTrue(errorCode(b).contains(ErrorCode.MissingRequiredClientCapability.code))
        ,
      ),

      suite("auth")(
        test("a task is visible only to the caller that created it"):
          for
            port  <- Server.install(authedServer.routes)
            tid   <- createTask(port, "slow", token = Some("alice"))
            mine  <- getTask(port, tid, token = Some("alice"))
            other <- getTask(port, tid, token = Some("bob"))
            stop  <- taskRequest(port, "tasks/cancel", tid, token = Some("bob")).flatMap(json)
            still <- getTask(port, tid, token = Some("alice")).map(result)
          yield assertTrue(
            str(result(mine), "status").contains("working"),
            errorCode(other).contains(ErrorCode.InvalidParams.code),
            errorCode(stop).contains(ErrorCode.InvalidParams.code),
            str(still, "status").contains("working"),
          )
        ,
        test("per-tool scopes are enforced before a task is created"):
          for
            port   <- Server.install(authedServer.routes)
            denied <- call(port, "admin_task", token = Some("alice"))
            ok     <- call(port, "admin_task", token = Some("admin")).flatMap(json)
          yield assertTrue(
            denied.status == Status.Forbidden,
            str(result(ok), "resultType").contains("task"),
          )
        ,
      ),

      suite("client")(
        test("callTool follows a task to its result"):
          for
            port <- Server.install(server.routes)
            out  <- ZIO.scoped(connect(port).flatMap(_.callTool("deep_research", Json.Obj("topic" -> Json.Str("Oslo")))))
          yield assertTrue(text(out) == "Research on Oslo: done")
        ,
        test("callTool answers a task's input request with onInputRequest"):
          for
            port <- Server.install(server.routes)
            out  <- ZIO.scoped(connect(port, Some(acceptConfirm)).flatMap(_.callTool("book_flight")))
          yield assertTrue(text(out) == "Booked")
        ,
        test("startTool returns the task, which getTask and awaitTask follow"):
          ZIO.scoped:
            for
              port    <- Server.install(server.routes)
              client  <- connect(port)
              started <- client.startTool("deep_research", Json.Obj("topic" -> Json.Str("Rome")))
              task    <- started match
                           case ToolCallOutcome.Started(t)   => ZIO.succeed(t)
                           case ToolCallOutcome.Completed(_) => ZIO.dieMessage("expected a task")
              polled  <- client.getTask(task.taskId)
              out     <- client.awaitTask(task.taskId)
            yield assertTrue(
              task.status == TaskStatus.Working,
              polled.taskId == task.taskId,
              text(out) == "Research on Rome: done",
            )
        ,
        test("startTool on a synchronous tool completes"):
          ZIO.scoped:
            for
              port <- Server.install(server.routes)
              out  <- connect(port).flatMap(_.startTool("sync", Json.Obj()))
            yield assertTrue(out match
              case ToolCallOutcome.Completed(r) => text(r) == "sync done"
              case ToolCallOutcome.Started(_)   => false
            )
        ,
        test("cancelTask cancels, and awaitTask reports it"):
          ZIO.scoped:
            for
              port   <- Server.install(server.routes)
              client <- connect(port)
              task   <- client.startTool("slow", Json.Obj()).flatMap:
                          case ToolCallOutcome.Started(t)   => ZIO.succeed(t)
                          case ToolCallOutcome.Completed(_) => ZIO.dieMessage("expected a task")
              _      <- client.cancelTask(task.taskId)
              out    <- client.awaitTask(task.taskId).either
            yield assertTrue(out match
              case Left(McpClientError.TaskCancelled(id, _)) => id == task.taskId.value
              case _                                         => false
            )
        ,
        test("interrupting callTool cancels the task on the server"):
          ZIO.scoped:
            for
              port   <- Server.install(server.routes)
              client <- connect(port)
              fiber  <- client.callTool("slow").fork
              _      <- ZIO.sleep(300.millis)
              _      <- fiber.interrupt
              _      <- ZIO.sleep(200.millis)
              // The interrupted call never handed back the task id, so look at the store.
              all    <- store.all
            yield assertTrue(all.exists(r =>
              r.task.status == TaskStatus.Cancelled && r.task.createdAt.isAfter(Instant.now().minusSeconds(5))))
        ,
      ),

    ).provide(Server.defaultWith(_.onAnyOpenPort), Client.default, Scope.default, McpServer.State.default) @@
      withLiveClock @@ timeout(2.minutes) @@ sequential
