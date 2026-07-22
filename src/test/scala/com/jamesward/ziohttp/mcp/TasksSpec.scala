package com.jamesward.ziohttp.mcp

import zio.*
import zio.http.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*
import zio.test.TestAspect.*

given canEqualStatusTasks: CanEqual[Status, Status] = CanEqual.derived

/**
 * Exercises the 2026-07-28 Tasks extension (`io.modelcontextprotocol/tasks`):
 * capability advertisement, task-augmented tool calls (`resultType: "task"`),
 * `tasks/get` polling to a terminal `completed`, and `tasks/cancel`.
 */
object TasksSpec extends ZIOSpecDefault:

  private val Modern = ProtocolVersion.V2026_07_28.wire

  val quickTool: McpToolHandler = McpTool("quick")
    .description("Completes immediately")
    .handle:
      ZIO.succeed("quick done")

  val slowTool: McpToolHandler = McpTool("slow")
    .description("Takes a while")
    .handleWithContext[Any, ToolError, Chunk[ToolContent]]: _ =>
      ZIO.sleep(30.seconds).as(Chunk(ToolContent.text("slow done")))

  val server = McpServer("tasks-server", "1.0.0").tool(quickTool).tool(slowTool)

  private def meta(withTask: Boolean): Json.Obj =
    val base = Chunk[(String, Json)](McpMeta.ProtocolVersion -> Json.Str(Modern))
    Json.Obj(if withTask then base :+ (McpMeta.Tasks -> (Json.Obj(): Json)) else base)

  private def callBody(id: Int, name: String, asTask: Boolean): String =
    val params = Json.Obj("name" -> Json.Str(name), "arguments" -> Json.Obj(), "_meta" -> (meta(asTask): Json))
    s"""{"jsonrpc":"2.0","id":$id,"method":"tools/call","params":${(params: Json).toJson}}"""

  private def taskMethodBody(id: Int, method: String, taskId: String): String =
    val params = Json.Obj("taskId" -> Json.Str(taskId), "_meta" -> (meta(false): Json))
    s"""{"jsonrpc":"2.0","id":$id,"method":"$method","params":${(params: Json).toJson}}"""

  private def post(port: Int, body: String, method: String, name: Option[String] = None): ZIO[Client & Scope, Throwable, Response] =
    val url = URL.decode(s"http://localhost:$port/mcp").toOption.get
    var req = Request.post(url, Body.fromString(body))
      .addHeader(Header.ContentType(MediaType.application.json))
      .addHeader("accept", "application/json, text/event-stream")
      .addHeader(Negotiation.ProtocolVersionHeader, Modern)
      .addHeader(Negotiation.MethodHeader, method)
    name.foreach(n => req = req.addHeader(Negotiation.NameHeader, n))
    ZClient.batched(req)

  private def bodyJson(r: Response): ZIO[Any, Throwable, Json.Obj] =
    r.body.asString.flatMap(s => ZIO.fromEither(s.fromJson[Json.Obj]).mapError(e => RuntimeException(s"$e: $s")))

  private def result(b: Json.Obj): Option[Json.Obj] = b.get("result").flatMap(_.asObject)
  private def taskStatus(b: Json.Obj): Option[String] =
    result(b).flatMap(_.get("task")).flatMap(_.asObject).flatMap(_.get("status")).flatMap(_.asString)
  private def taskId(b: Json.Obj): Option[String] =
    result(b).flatMap(_.get("task")).flatMap(_.asObject).flatMap(_.get("taskId")).flatMap(_.asString)

  override def spec =
    suite("TasksSpec")(

      test("server/discover advertises the tasks extension capability"):
        for
          port <- Server.install(server.routes)
          resp <- post(port, s"""{"jsonrpc":"2.0","id":1,"method":"server/discover","params":{"_meta":${(meta(false): Json).toJson}}}""", "server/discover")
          b    <- bodyJson(resp)
        yield
          val ext = result(b).flatMap(_.get("capabilities")).flatMap(_.asObject)
            .flatMap(_.get("extensions")).flatMap(_.asObject)
          assertTrue(ext.flatMap(_.get(McpMeta.Tasks)).isDefined)
      ,

      test("task-augmented tool call returns resultType task and completes via tasks/get"):
        for
          port    <- Server.install(server.routes)
          create  <- post(port, callBody(1, "quick", asTask = true), "tools/call", Some("quick"))
          cb      <- bodyJson(create)
          tid      = taskId(cb).getOrElse("")
          // Poll tasks/get until terminal.
          finalB  <- (for
                        g <- post(port, taskMethodBody(2, "tasks/get", tid), "tasks/get")
                        b <- bodyJson(g)
                      yield b).repeatUntil(b => taskStatus(b).exists(s => s == "completed" || s == "failed"))
        yield
          val text = result(finalB).flatMap(_.get("result")).flatMap(_.asObject)
            .flatMap(_.get("content")).flatMap(_.asArray).flatMap(_.headOption)
            .flatMap(_.asObject).flatMap(_.get("text")).flatMap(_.asString)
          assertTrue(
            create.status == Status.Ok,
            result(cb).flatMap(_.get("resultType")).flatMap(_.asString).contains("task"),
            tid.nonEmpty,
            taskStatus(cb).contains("working"),
            taskStatus(finalB).contains("completed"),
            text.contains("quick done"),
          )
      ,

      test("tasks/cancel cancels a running task"):
        for
          port   <- Server.install(server.routes)
          create <- post(port, callBody(1, "slow", asTask = true), "tools/call", Some("slow"))
          cb     <- bodyJson(create)
          tid     = taskId(cb).getOrElse("")
          cancel <- post(port, taskMethodBody(2, "tasks/cancel", tid), "tasks/cancel")
          get    <- post(port, taskMethodBody(3, "tasks/get", tid), "tasks/get")
          gb     <- bodyJson(get)
        yield assertTrue(
          cancel.status == Status.Ok,
          taskStatus(gb).contains("cancelled"),
        )
      ,

      test("tasks/get for an unknown task is an error"):
        for
          port <- Server.install(server.routes)
          resp <- post(port, taskMethodBody(1, "tasks/get", "nope"), "tasks/get")
          b    <- bodyJson(resp)
        yield assertTrue(
          b.get("error").flatMap(_.asObject).flatMap(_.get("code")).flatMap(_.asNumber).map(_.value.intValue)
            .contains(ErrorCode.InvalidParams.code),
        )
      ,

    ).provide(Server.defaultWith(_.onAnyOpenPort), Client.default, Scope.default, McpServer.State.default) @@
      withLiveClock @@ timeout(1.minute) @@ sequential
