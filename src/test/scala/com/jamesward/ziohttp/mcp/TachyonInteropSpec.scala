package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.client.*
import zio.*
import zio.http.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*
import zio.test.TestAspect.*

import dev.tachyonmcp.api.server.domain.{FormInputRequest, InputRequestBundle, TaskResult}
import dev.tachyonmcp.api.server.features.tasks.{TaskConnector, TaskNotFoundException, TaskSnapshot, TaskState, TaskSupport}
import dev.tachyonmcp.api.server.features.tools.ToolResult
import dev.tachyonmcp.core.server.TachyonServer
import dev.tachyonmcp.extensions.tasks.TasksExtension

import java.time.{Duration as JDuration, Instant}
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters.*

given canEqualStatusTachyon: CanEqual[Status, Status] = CanEqual.derived

/**
 * Cross-implementation interop against [[https://github.com/kpavlov/tachyon
 * kpavlov/tachyon]], a standalone pure-Java MCP server runtime. Tachyon is
 * dual-era: it implements the modern `2026-07-28` `server/discover` and also
 * answers the legacy `2025-11-25` `initialize` handshake. These tests prove our
 * client negotiates the modern era against a real third-party server, and that
 * pinning to the legacy era interoperates too.
 */
object TachyonInteropSpec extends ZIOSpecDefault:

  /** Start a tachyon server with a `greet` tool on an ephemeral port. */
  private def tachyonServer: ZIO[Scope, Throwable, TachyonServer] =
    ZIO.acquireRelease(
      ZIO.attemptBlocking:
        val server = TachyonServer.builder()
          .name("tachyon-server")
          .version("1.0.0")
          .withTools: tools =>
            tools.register(
              b =>
                b.name("greet")
                b.description("Greets the caller")
              ,
              (_, _) => ToolResult.text("hello from tachyon")
            )
          .port(0)
          .build()
        server.start()
        server
    )(server => ZIO.attemptBlocking(server.close()).ignore)

  /**
   * A tachyon server whose tools run as Tasks-extension tasks (SEP-2663). Tachyon
   * leaves task storage to the application through a `TaskConnector`; this one
   * keeps snapshots in a map. `research` finishes on a background thread;
   * `confirm` waits in `input_required` until `tasks/update` answers it.
   */
  private def tachyonTaskServer: ZIO[Scope, Throwable, TachyonServer] =
    ZIO.acquireRelease(
      ZIO.attemptBlocking:
        val tasks = ConcurrentHashMap[String, TaskSnapshot]()
        def lookup(id: String): TaskSnapshot =
          Option(tasks.get(id)).getOrElse(throw TaskNotFoundException(id))
        def advance(id: String)(f: TaskSnapshot.Builder => TaskSnapshot.Builder): Unit =
          tasks.computeIfPresent(id, (_, s) =>
            if s.status.isTerminal then s
            else f(TaskSnapshot.builder().from(s).lastUpdatedAt(Instant.now()).pendingInput(null)
              .revision(s.revision + 1)).build())
        def created(id: String, status: TaskState, pending: InputRequestBundle | Null): TaskSnapshot =
          val now = Instant.now()
          val snapshot = TaskSnapshot.builder().taskId(id).status(status).createdAt(now).lastUpdatedAt(now)
            .ttl(JDuration.ofMinutes(5)).pollInterval(JDuration.ofMillis(100))
            .pendingInput(pending).revision(0).build()
          tasks.put(id, snapshot)
          snapshot
        val confirmSchema: java.util.Map[String, Object] = Map[String, Object](
          "type" -> "object",
          "properties" -> Map("confirm" -> Map("type" -> "boolean").asJava).asJava,
        ).asJava

        val connector = TaskConnector.builder()
          .get((_, req) => lookup(req.taskId))
          .cancel((_, req) => { lookup(req.taskId); advance(req.taskId)(_.status(TaskState.CANCELLED)) })
          .update: (_, req) =>
            lookup(req.taskId)
            val answer = Option(req.inputResponses.get("confirm")).map(_.toString).getOrElse("none")
            advance(req.taskId)(_.status(TaskState.COMPLETED)
              .result(TaskResult.completed(ToolResult.text(s"confirmed: $answer"))))
          .build()

        val server = TachyonServer.builder()
          .name("tachyon-tasks")
          .version("1.0.0")
          .withExtension(classOf[TasksExtension], _.connector(connector))
          .withTools: tools =>
            tools.register(
              b =>
                b.name("research")
                b.description("Researches in the background")
                b.taskSupport(TaskSupport.OPTIONAL)
              ,
              (_, _) =>
                val id = java.util.UUID.randomUUID().toString
                val snapshot = created(id, TaskState.WORKING, null)
                val worker = Thread: () =>
                  Thread.sleep(300)
                  advance(id)(_.status(TaskState.COMPLETED)
                    .result(TaskResult.completed(ToolResult.text("research done"))))
                worker.setDaemon(true)
                worker.start()
                ToolResult.task(snapshot)
            )
            tools.register(
              b =>
                b.name("confirm")
                b.description("Asks for confirmation through the task")
                b.taskSupport(TaskSupport.OPTIONAL)
              ,
              (_, _) =>
                val pending = InputRequestBundle(
                  Map("confirm" -> FormInputRequest.of("Proceed?", dev.tachyonmcp.api.json.JsonSchema.from(confirmSchema))).asJava, null)
                ToolResult.task(created(java.util.UUID.randomUUID().toString, TaskState.INPUT_REQUIRED, pending))
            )
          .port(0)
          .build()
        server.start()
        server
    )(server => ZIO.attemptBlocking(server.close()).ignore)

  private val Modern = ProtocolVersion.V2026_07_28.wire

  /** Send a raw modern (2026-07-28) request to tachyon and parse the response body.
    * Includes the full modern `_meta` — tachyon requires `clientInfo`, which our
    * own client also always sends. */
  private def modernPost(port: Int, id: Int, method: String): ZIO[Client & Scope, Throwable, Json.Obj] =
    val meta = Json.Obj(
      McpMeta.ProtocolVersion    -> Json.Str(Modern),
      McpMeta.ClientInfo         -> Json.Obj("name" -> Json.Str("probe"), "version" -> Json.Str("1.0")),
      McpMeta.ClientCapabilities -> Json.Obj(),
    )
    val params = Json.Obj("_meta" -> (meta: Json))
    val body = s"""{"jsonrpc":"2.0","id":$id,"method":"$method","params":${(params: Json).toJson}}"""
    val url = URL.decode(s"http://localhost:$port/mcp").toOption.get
    val req = Request.post(url, Body.fromString(body))
      .addHeader(Header.ContentType(MediaType.application.json))
      .addHeader("accept", "application/json, text/event-stream")
      .addHeader(Negotiation.ProtocolVersionHeader, Modern)
      .addHeader(Negotiation.MethodHeader, method)
    ZClient.batched(req).flatMap: resp =>
      resp.body.asString.flatMap(s => ZIO.fromEither(s.fromJson[Json.Obj]).mapError(e => RuntimeException(s"$e: $s")))

  override def spec =
    suite("Tachyon interop (third-party MCP server)")(

      test("modern-default client negotiates the modern era against tachyon via server/discover"):
        ZIO.scoped:
          for
            server <- tachyonServer
            port    = server.port()
            client <- McpClient.connect(s"http://localhost:$port/mcp")
            tools  <- client.listTools
          yield assertTrue(
            // tachyon implements server/discover, so the client stays modern.
            client.protocolVersion == ProtocolVersion.V2026_07_28.wire,
            client.serverInfo.name == "tachyon-server",
            tools.map(_.name.value).contains("greet"),
          )
      ,

      test("client calls a tachyon-hosted tool"):
        ZIO.scoped:
          for
            server <- tachyonServer
            port    = server.port()
            client <- McpClient.connect(s"http://localhost:$port/mcp")
            result <- client.callTool("greet")
          yield
            val text = result.content.collectFirst { case ToolContent.Text(t, _) => t }
            assertTrue(text.exists(_.contains("hello from tachyon")))
      ,

      test("explicitly legacy-pinned client also interoperates with tachyon"):
        ZIO.scoped:
          for
            server <- tachyonServer
            port    = server.port()
            client <- McpClient.connect(McpClientConfig(
                        s"http://localhost:$port/mcp",
                        preferredVersion = ProtocolVersion.V2025_11_25,
                      ))
            tools  <- client.listTools
          yield assertTrue(
            client.protocolVersion == ProtocolVersion.V2025_11_25.wire,
            tools.map(_.name.value).contains("greet"),
          )
      ,

      // Wire-level cross-checks: a real third-party modern server (tachyon) and
      // our implementation must agree on the 2026-07-28 envelope field names.
      test("tachyon's server/discover returns the modern envelope we expect"):
        ZIO.scoped:
          for
            server <- tachyonServer
            port    = server.port()
            b      <- modernPost(port, 1, "server/discover")
          yield
            val r = b.get("result").flatMap(_.asObject)
            val supported = r.flatMap(_.get("supportedVersions")).flatMap(_.asArray)
              .map(_.flatMap(_.asString).toList).getOrElse(Nil)
            assertTrue(
              supported.contains(ProtocolVersion.V2026_07_28.wire),
              r.flatMap(_.get("resultType")).flatMap(_.asString).contains("complete"),
              r.flatMap(_.get("_meta")).flatMap(_.asObject).flatMap(_.get(McpMeta.ServerInfo)).isDefined,
            )
      ,

      test("tachyon answers a modern tools/list with resultType complete"):
        ZIO.scoped:
          for
            server <- tachyonServer
            port    = server.port()
            b      <- modernPost(port, 2, "tools/list")
          yield
            val r = b.get("result").flatMap(_.asObject)
            assertTrue(
              r.flatMap(_.get("resultType")).flatMap(_.asString).contains("complete"),
              r.flatMap(_.get("tools")).flatMap(_.asArray).exists(_.nonEmpty),
            )
      ,

      // Tasks extension (SEP-2663): tachyon creates the tasks, our client follows them.
      test("callTool follows a tachyon task to its result"):
        ZIO.scoped:
          for
            server <- tachyonTaskServer
            client <- McpClient.connect(s"http://localhost:${server.port()}/mcp")
            result <- client.callTool("research")
          yield
            val text = result.content.collectFirst { case ToolContent.Text(t, _) => t }
            assertTrue(text.contains("research done"))
      ,

      test("startTool returns tachyon's CreateTaskResult and getTask reads it"):
        ZIO.scoped:
          for
            server  <- tachyonTaskServer
            client  <- McpClient.connect(s"http://localhost:${server.port()}/mcp")
            started <- client.startTool("research", Json.Obj())
            task    <- started match
                         case ToolCallOutcome.Started(t)   => ZIO.succeed(t)
                         case ToolCallOutcome.Completed(_) => ZIO.dieMessage("expected a task")
            polled  <- client.getTask(task.taskId)
          yield assertTrue(
            task.status == TaskStatus.Working,
            task.pollIntervalMs.contains(100L),
            task.ttlMs.contains(300000L),
            polled.taskId == task.taskId,
          )
      ,

      test("the client answers a tachyon task's inputRequests with tasks/update"):
        ZIO.scoped:
          for
            server <- tachyonTaskServer
            asked  <- Ref.make(Chunk.empty[String])
            client <- McpClient.connect(McpClientConfig(
                        s"http://localhost:${server.port()}/mcp",
                        onInputRequest = Some(req =>
                          asked.update(_ :+ s"${req.id}:${req.method}").as(Json.Obj(
                            "action"  -> Json.Str("accept"),
                            "content" -> Json.Obj("confirm" -> Json.Bool(true)),
                          ))),
                      ))
            result <- client.callTool("confirm")
            seen   <- asked.get
          yield
            val text = result.content.collectFirst { case ToolContent.Text(t, _) => t }
            assertTrue(
              seen == Chunk("confirm:elicitation/create"),
              text.exists(_.startsWith("confirmed:")),
            )
      ,

      test("cancelTask against tachyon ends the task cancelled"):
        ZIO.scoped:
          for
            server <- tachyonTaskServer
            client <- McpClient.connect(s"http://localhost:${server.port()}/mcp")
            task   <- client.startTool("confirm", Json.Obj()).flatMap:
                        case ToolCallOutcome.Started(t)   => ZIO.succeed(t)
                        case ToolCallOutcome.Completed(_) => ZIO.dieMessage("expected a task")
            _      <- client.cancelTask(task.taskId)
            out    <- client.awaitTask(task.taskId).either
          yield assertTrue(out match
            case Left(McpClientError.TaskCancelled(id, _)) => id == task.taskId.value
            case _                                         => false
          )
      ,

    ).provide(Client.default) @@
      withLiveClock @@ timeout(2.minutes) @@ sequential @@ TestAspect.withLiveEnvironment
