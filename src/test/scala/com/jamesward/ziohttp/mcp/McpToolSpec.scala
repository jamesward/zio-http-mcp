package com.jamesward.ziohttp.mcp

import zio.*
import zio.json.ast.Json
import zio.schema.*
import zio.test.*
import zio.test.TestAspect.*

object McpToolSpec extends ZIOSpecDefault:

  // --- Services (README: Tools with ZIO Layers) ---

  trait Greeter:
    def greet(name: String): UIO[String]

  object Greeter:
    val live: ULayer[Greeter] = ZLayer.succeed:
      new Greeter:
        def greet(name: String) = ZIO.succeed(s"Hello, $name!")

  trait UserRepo:
    def findUser(id: String): UIO[String]

  object UserRepo:
    val live: ULayer[UserRepo] = ZLayer.succeed:
      new UserRepo:
        def findUser(id: String) = ZIO.succeed(s"user-$id")

  trait Notifier:
    def notify(user: String, message: String): UIO[String]

  object Notifier:
    val live: ULayer[Notifier] = ZLayer.succeed:
      new Notifier:
        def notify(user: String, message: String) = ZIO.succeed(s"Notified $user: $message")

  trait Database:
    def query(sql: String): IO[ToolError, String]

  object Database:
    val live: ULayer[Database] = ZLayer.succeed:
      new Database:
        def query(sql: String) = ZIO.succeed(s"result of: $sql")

  // --- Input/output types (README examples) ---

  case class NameInput(name: String) derives Schema
  case class NotifyInput(userId: String, message: String) derives Schema
  case class AddInput(a: Int, b: Int) derives Schema
  case class AddOutput(result: Int) derives Schema
  case class QueryInput(sql: String) derives Schema
  case class LookupInput(id: String) derives Schema
  case class DeleteInput(userId: String) derives Schema
  case class Result(value: Int) derives Schema

  // --- Error types (README: Error Handling) ---

  enum AppError:
    case NotFound(id: String)
    case Forbidden(reason: String)

  given McpError[AppError] with
    def message(e: AppError): String = e match
      case AppError.NotFound(id) => s"Not found: $id"
      case AppError.Forbidden(reason) => s"Forbidden: $reason"

  // --- Helper ---

  private def resultText(result: CallToolResult): String =
    result.content.headOption match
      case Some(ToolContent.Text(text, _)) => text
      case _ => ""

  override def spec = suite("McpTool")(

    // README: handle — Typed Input/Output
    suite("handle variations")(
      test("no input, no error — String"):
        val tool = McpTool("hello")
          .description("Says hello")
          .handle:
            ZIO.succeed("Hello!")
        for result <- tool.call(None)
        yield assertTrue(
          resultText(result) == "Hello!",
        )
      ,
      test("no input, no error — ToolContent"):
        val tool = McpTool("hello2")
          .description("Says hello")
          .handle:
            ZIO.succeed(ToolContent.text("Hello!"))
        for result <- tool.call(None)
        yield assertTrue(
          resultText(result) == "Hello!",
        )
      ,
      test("no input, no error — Chunk[ToolContent]"):
        val tool = McpTool("hello3")
          .description("Says hello")
          .handle:
            ZIO.succeed(Chunk(ToolContent.text("Hello!")))
        for result <- tool.call(None)
        yield assertTrue(
          resultText(result) == "Hello!",
        )
      ,
      test("with input, no error — String output"):
        val tool = McpTool("greet")
          .description("Greets someone by name")
          .handle: (input: NameInput) =>
            ZIO.succeed(s"Hello, ${input.name}!")
        for result <- tool.call(Some(Json.Obj(Chunk("name" -> Json.Str("World")))))
        yield assertTrue(
          resultText(result) == "Hello, World!",
        )
      ,
      test("with input, no error — structured output"):
        val tool = McpTool("add")
          .description("Adds two numbers")
          .handle: (input: AddInput) =>
            ZIO.succeed(AddOutput(input.a + input.b))
        for result <- tool.call(Some(Json.Obj(Chunk("a" -> Json.Num(2), "b" -> Json.Num(3)))))
        yield assertTrue(
          result.structuredContent.isDefined,
          resultText(result).contains("5"),
        )
      ,
      test("no input, no error — Clock.instant"):
        val tool = McpTool("time")
          .description("Returns the current time")
          .handle:
            Clock.instant
        for result <- tool.call(None)
        yield assertTrue(
          resultText(result).nonEmpty,
        )
      ,
      test("with input, with error — success"):
        val tool = McpTool("divide")
          .description("Divides two numbers")
          .handle[Any, ToolError, AddInput, Double]: input =>
            if input.b == 0 then ZIO.fail(ToolError("Division by zero"))
            else ZIO.succeed(input.a.toDouble / input.b)
        for result <- tool.call(Some(Json.Obj(Chunk("a" -> Json.Num(10), "b" -> Json.Num(2)))))
        yield assertTrue(
          resultText(result).contains("5.0"),
        )
      ,
      test("with input, with error — failure"):
        val tool = McpTool("divide")
          .description("Divides two numbers")
          .handle[Any, ToolError, AddInput, Double]: input =>
            if input.b == 0 then ZIO.fail(ToolError("Division by zero"))
            else ZIO.succeed(input.a.toDouble / input.b)
        for result <- tool.call(Some(Json.Obj(Chunk("a" -> Json.Num(1), "b" -> Json.Num(0)))))
        yield assertTrue(
          result.isError.contains(true),
          resultText(result) == "Division by zero",
        )
      ,
      test("no input, with error"):
        val tool = McpTool("fail")
          .description("Always fails")
          .handle[Any, ToolError, String]:
            ZIO.fail(ToolError("boom"))
        for result <- tool.call(None)
        yield assertTrue(
          result.isError.contains(true),
          resultText(result) == "boom",
        )
      ,
      test("returns multiple ToolContent items"):
        val tool = McpTool("multi")
          .description("Returns multiple items")
          .handle:
            ZIO.succeed(Chunk(ToolContent.text("hello"), ToolContent.text("world")))
        for result <- tool.call(None)
        yield assertTrue(
          result.content.size == 2,
        )
      ,
      test("returns structured output with schema"):
        val tool = McpTool("result")
          .description("Returns structured result")
          .handle:
            ZIO.succeed(Result(42))
        for result <- tool.call(None)
        yield assertTrue(
          result.structuredContent.isDefined,
          tool.definition.outputSchema.isDefined,
        )
    ),

    // README: handleWithContext — With Tool Context
    suite("handleWithContext variations")(
      test("no input, no error"):
        val tool = McpTool("ctx_tool")
          .description("Uses context")
          .handleWithContext: ctx =>
            ZIO.succeed("with context")
        for result <- tool.call(None)
        yield assertTrue(
          resultText(result) == "with context",
        )
      ,
      test("with input, no error"):
        val tool = McpTool("ctx_greet")
          .description("Greets with context")
          .handleWithContext: (input: NameInput, _: McpToolContext) =>
            ZIO.succeed(s"Hi, ${input.name}!")
        for result <- tool.call(Some(Json.Obj(Chunk("name" -> Json.Str("World")))))
        yield assertTrue(
          resultText(result) == "Hi, World!",
        )
      ,
      test("no input, with error"):
        val tool = McpTool("ctx_fail")
          .description("Fails with context")
          .handleWithContext[Any, ToolError, String]: ctx =>
            ZIO.fail(ToolError("ctx boom"))
        for result <- tool.call(None)
        yield assertTrue(
          result.isError.contains(true),
          resultText(result) == "ctx boom",
        )
    ),

    // README: Tools with ZIO Layers
    suite("layers")(
      test("tool with single layer"):
        val tool = McpTool("greet")
          .description("Greets by name")
          .handle: (input: NameInput) =>
            ZIO.serviceWithZIO[Greeter](_.greet(input.name))
        for result <- tool.call(Some(Json.Obj(Chunk("name" -> Json.Str("World")))))
        yield assertTrue(
          resultText(result) == "Hello, World!",
        )
      .provide(Greeter.live),

      test("tool with two layers"):
        val tool = McpTool("notify_user")
          .description("Finds user and notifies them")
          .handle: (input: NotifyInput) =>
            for
              userName <- ZIO.serviceWithZIO[UserRepo](_.findUser(input.userId))
              msg      <- ZIO.serviceWithZIO[Notifier](_.notify(userName, input.message))
            yield msg
        for
          result <- tool.call(Some(Json.Obj(Chunk(
            "userId" -> Json.Str("42"),
            "message" -> Json.Str("hi"),
          ))))
        yield assertTrue(
          resultText(result).contains("Notified user-42: hi"),
        )
      .provide(UserRepo.live, Notifier.live),

      test("database layer with error"):
        val tool = McpTool("query")
          .description("Runs a database query")
          .handle[Database, ToolError, QueryInput, String]: input =>
            ZIO.serviceWithZIO[Database](_.query(input.sql))
        for result <- tool.call(Some(Json.Obj(Chunk("sql" -> Json.Str("SELECT 1")))))
        yield assertTrue(
          resultText(result) == "result of: SELECT 1",
        )
      .provide(Database.live),
    ),

    // README: Error Handling
    suite("errors")(
      test("custom error type — McpError[AppError]"):
        val tool = McpTool("lookup")
          .handle[Any, AppError, LookupInput, String]: input =>
            if input.id == "missing" then ZIO.fail(AppError.NotFound(input.id))
            else if input.id == "secret" then ZIO.fail(AppError.Forbidden("classified"))
            else ZIO.succeed(s"Found: ${input.id}")
        for
          ok        <- tool.call(Some(Json.Obj(Chunk("id" -> Json.Str("alice")))))
          notFound  <- tool.call(Some(Json.Obj(Chunk("id" -> Json.Str("missing")))))
          forbidden <- tool.call(Some(Json.Obj(Chunk("id" -> Json.Str("secret")))))
        yield assertTrue(
          resultText(ok) == "Found: alice",
          notFound.isError.contains(true),
          resultText(notFound) == "Not found: missing",
          forbidden.isError.contains(true),
          resultText(forbidden) == "Forbidden: classified",
        )
      ,
      test("handler defect returns error result"):
        val tool = McpTool("boom")
          .description("Dies unexpectedly")
          .handle[Any, String]:
            ZIO.die(RuntimeException("something broke"))
        for result <- tool.call(None)
        yield assertTrue(
          result.isError.contains(true),
          resultText(result) == "something broke",
        )
    ),

    // README: Tool Annotations
    suite("annotations")(
      test("annotations appear in tool definition"):
        import OptBool.*
        val tool = McpTool("delete_user")
          .description("Deletes a user account")
          .annotations(destructive = True, idempotent = True)
          .handle: (input: DeleteInput) =>
            ZIO.succeed(s"Deleted: ${input.userId}")
        val ann = tool.definition.annotations.get
        assertTrue(
          ann.destructiveHint.contains(true),
          ann.idempotentHint.contains(true),
          ann.readOnlyHint.isEmpty,
          ann.openWorldHint.isEmpty,
        )
    ),

    // README: Custom JSON Schema
    suite("custom json schema")(
      test("McpInput.raw provides custom schema and passes args through"):
        given McpInput[Option[Json.Obj]] = McpInput.raw(Json.Obj(Chunk(
          "type" -> Json.Str("object"),
          "properties" -> Json.Obj(Chunk(
            "value" -> Json.Obj(Chunk("type" -> Json.Str("string"))),
          )),
        )))
        val tool = McpTool("validate")
          .description("Validate data")
          .handle: (args: Option[Json.Obj]) =>
            val value = args.flatMap(_.get("value")).flatMap(_.asString).getOrElse("")
            ZIO.succeed(s"Received: $value")
        for result <- tool.call(Some(Json.Obj(Chunk("value" -> Json.Str("test")))))
        yield assertTrue(
          resultText(result) == "Received: test",
          tool.definition.inputSchema.get("properties").isDefined,
        )
    ),
  ) @@ withLiveClock
