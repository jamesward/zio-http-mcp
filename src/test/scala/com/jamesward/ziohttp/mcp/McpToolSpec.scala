package com.jamesward.ziohttp.mcp

import zio.*
import zio.json.ast.Json
import zio.schema.*
import zio.test.*

object McpToolSpec extends ZIOSpecDefault:

  // --- Services ---

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

  // --- Input types ---

  case class EmptyInput() derives Schema
  case class NameInput(name: String) derives Schema
  case class NotifyInput(userId: String, message: String) derives Schema

  // --- Error types ---

  enum AppError:
    case NotFound(id: String)
    case Unauthorized(user: String)

  // Custom McpError instance for AppError
  given McpError[AppError] with
    def message(e: AppError): String = e match
      case AppError.NotFound(id) => s"Not found: $id"
      case AppError.Unauthorized(u) => s"Unauthorized: $u"

  // --- Helper ---

  private def resultText(result: CallToolResult): String =
    result.content.headOption match
      case Some(ToolContent.Text(text, _)) => text
      case _ => ""

  override def spec = suite("McpTool")(
    suite("layers")(
      test("tool with single layer"):
        val tool = McpTool("greet")
          .description("Greets by name")
          .handle[Greeter, Nothing, NameInput, String]:
            input => ZIO.serviceWithZIO[Greeter](_.greet(input.name))
        for
          result <- tool.call(Some(Json.Obj(Chunk("name" -> Json.Str("World")))))
        yield assertTrue(
          !result.isError.getOrElse(false),
          resultText(result).contains("Hello, World!"),
        )
      .provide(Greeter.live),
      test("tool with two layers"):
        val tool = McpTool("notify_user")
          .description("Finds user and notifies them")
          .handle[UserRepo & Notifier, Nothing, NotifyInput, String]:
            input =>
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
          !result.isError.getOrElse(false),
          resultText(result).contains("Notified user-42: hi"),
        )
      .provide(UserRepo.live, Notifier.live),
    ),
    suite("errors")(
      test("tool that returns a ToolError"):
        val tool = McpTool("always_fails")
          .description("Always fails")
          .handle[Any, ToolError, EmptyInput, String]: _ =>
            ZIO.fail(ToolError("Something went wrong"))
        for
          result <- tool.call(Some(Json.Obj()))
        yield assertTrue(
          result.isError.contains(true),
          resultText(result) == "Something went wrong",
        )
      ,
      test("tool with custom error type and McpError instance"):
        val tool = McpTool("lookup")
          .description("Looks up with typed errors")
          .handle[Any, AppError, NameInput, String]: input =>
            if input.name == "unknown" then ZIO.fail(AppError.NotFound(input.name))
            else if input.name == "admin" then ZIO.fail(AppError.Unauthorized(input.name))
            else ZIO.succeed(s"Found: ${input.name}")
        for
          ok       <- tool.call(Some(Json.Obj(Chunk("name" -> Json.Str("alice")))))
          notFound <- tool.call(Some(Json.Obj(Chunk("name" -> Json.Str("unknown")))))
          unauth   <- tool.call(Some(Json.Obj(Chunk("name" -> Json.Str("admin")))))
        yield assertTrue(
          !ok.isError.getOrElse(false),
          resultText(ok).contains("Found: alice"),
          notFound.isError.contains(true),
          resultText(notFound) == "Not found: unknown",
          unauth.isError.contains(true),
          resultText(unauth) == "Unauthorized: admin",
        )
    ),
  )
