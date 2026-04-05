zio-http-mcp
------------

An MCP (Model Context Protocol) server library for Scala 3, ZIO, and ZIO HTTP.

Implements the [MCP 2025-11-25 specification](https://modelcontextprotocol.io) with Streamable HTTP transport, SSE streaming, tools, resources, prompts, sampling, elicitation, and progress notifications.

## Getting Started

Add the dependency to your `build.sbt`:

```scala
libraryDependencies += "com.jamesward" %% "zio-http-mcp" % "<version>"
```

### Minimal Server

```scala
import com.jamesward.ziohttp.mcp.*
import zio.*
import zio.http.*
import zio.schema.*

case class NameInput(name: String) derives Schema

val server = McpServer("my-server", "1.0.0")
  .tool(
    McpTool("greet")
      .description("Greets someone by name")
      .handle: (input: NameInput) =>
        ZIO.succeed(s"Hello, ${input.name}!")
  )

object Main extends ZIOAppDefault:
  def run =
    Server.serve(server.routes).provide(Server.default)
```

## Tools

Tools are the primary way to expose functionality to MCP clients. Define input types as case classes with `derives Schema`, and the library generates JSON Schema automatically.

### handle — Typed Input/Output

The `handle` method has overloads for common cases. Type parameters are inferred where possible.

```scala
case class AddInput(a: Int, b: Int) derives Schema
case class AddOutput(result: Int) derives Schema

// With input, no error — types inferred
val addTool = McpTool("add")
  .description("Adds two numbers")
  .handle: (input: AddInput) =>
    ZIO.succeed(AddOutput(input.a + input.b))

// No input, no error
val timeTool = McpTool("time")
  .description("Returns the current time")
  .handle:
    Clock.instant

// With input and error — error type must be explicit
val divTool = McpTool("divide")
  .description("Divides two numbers")
  .handle[Any, ToolError, AddInput, Double]: input =>
    if input.b == 0 then ZIO.fail(ToolError("Division by zero"))
    else ZIO.succeed(input.a.toDouble / input.b)
```

### Output Types

The output type determines how the result is serialized. The `McpOutput` type class handles this:

| Output type | Behavior |
|---|---|
| `String` | Plain text content, no output schema |
| `ToolContent` | Single content item (text, image, audio, embedded resource) |
| `Chunk[ToolContent]` | Multiple content items |
| Any type with `Schema` | JSON-serialized with `structuredContent` and `outputSchema` |

```scala
// Returns plain text
.handle: ZIO.succeed("Hello!")

// Returns a single image
.handle: ZIO.succeed(ToolContent.image(base64Data, "image/png"))

// Returns multiple content items
.handle: ZIO.succeed(Chunk(
  ToolContent.text("Here is an image:"),
  ToolContent.image(base64Data, "image/png"),
))

// Returns structured output with schema
case class Result(value: Int) derives Schema
.handle: ZIO.succeed(Result(42))
```

### handleWithContext — With Tool Context

Use `handleWithContext` when your tool needs logging, progress, sampling, or elicitation:

```scala
case class ProcessInput(data: String) derives Schema

val processTool = McpTool("process")
  .description("Processes data with progress")
  .handleWithContext: (input: ProcessInput, ctx: McpToolContext) =>
    for
      _ <- ctx.log(LogLevel.Info, "Starting")
      _ <- ctx.progress(0, 100)
      result <- doWork(input)
      _ <- ctx.progress(100, 100)
    yield s"Done: $result"

// No input — just takes the context
val statusTool = McpTool("status")
  .description("Reports status")
  .handleWithContext: ctx =>
    for _ <- ctx.log(LogLevel.Info, "Status check")
    yield "All systems operational"
```

`McpToolContext` provides:

| Method | Description |
|--------|-------------|
| `ctx.log(level, message)` | Send log notification to client |
| `ctx.progress(current, total)` | Send progress notification (requires `progressToken` in request) |
| `ctx.sample(prompt, maxTokens)` | Request LLM completion from client |
| `ctx.elicit(message, schema)` | Request user input from client with a JSON Schema form |

### Tools with ZIO Layers

Tools can declare ZIO environment requirements. These propagate through the server to the routes:

```scala
trait Database:
  def query(sql: String): IO[ToolError, String]

case class QueryInput(sql: String) derives Schema

val queryTool = McpTool("query")
  .description("Runs a database query")
  .handle[Database, ToolError, QueryInput, String]: input =>
    ZIO.serviceWithZIO[Database](_.query(input.sql))

val server = McpServer("my-server", "1.0.0")
  .tool(queryTool)   // needs Database
  .tool(cacheTool)   // needs Cache

// server.routes: Routes[Database & Cache, Response]
Server.serve(server.routes).provide(
  Server.default,
  Database.live,
  Cache.live,
)
```

### Error Handling

Tool handler errors are converted to MCP error responses (`isError: true`) using the `McpError[E]` type class. Built-in instances exist for `ToolError`, `String`, `Throwable`, and `Nothing`.

```scala
enum AppError:
  case NotFound(id: String)
  case Forbidden(reason: String)

given McpError[AppError] with
  def message(e: AppError): String = e match
    case AppError.NotFound(id)      => s"Not found: $id"
    case AppError.Forbidden(reason) => s"Forbidden: $reason"

val tool = McpTool("lookup")
  .handle[Any, AppError, LookupInput, String]: input =>
    if input.id == "missing" then ZIO.fail(AppError.NotFound(input.id))
    else ZIO.succeed(s"Found: ${input.id}")
```

### Tool Annotations

```scala
import OptBool.*

val tool = McpTool("delete_user")
  .description("Deletes a user account")
  .annotations(destructive = True, idempotent = True)
  .handle[Any, ToolError, DeleteInput, String](...)
```

Annotation values use `OptBool` (a tri-state enum: `True`, `False`, `Unset`) to distinguish "not set" from `false`. Available annotations: `readOnly`, `destructive`, `idempotent`, `openWorld`, plus `title: Option[String]`.

### Custom JSON Schema

For tools that need a hand-crafted JSON Schema (e.g., JSON Schema 2020-12 features not covered by ZIO Schema), provide a custom `McpInput` instance:

```scala
import zio.json.ast.Json

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
```

## Resources

Expose data to MCP clients as resources:

```scala
val configResource = McpResource("app://config", "App Config")
  .description("Application configuration")
  .mimeType("application/json")
  .read: uri =>
    ZIO.succeed(Chunk(ResourceContents(
      uri = uri,
      mimeType = Some("application/json"),
      text = Some("""{"debug": false}"""),
    )))
```

### Resource Templates

For parameterized resources using URI templates:

```scala
val userResource = McpResourceTemplate("app://users/{id}", "User")
  .description("User by ID")
  .mimeType("application/json")
  .read: uri =>
    val id = uri.stripPrefix("app://users/")
    ZIO.succeed(Chunk(ResourceContents(
      uri = uri,
      mimeType = Some("application/json"),
      text = Some(s"""{"id": "$id"}"""),
    )))
```

## Prompts

Expose reusable prompt templates:

```scala
val codeReviewPrompt = McpPrompt("code_review")
  .description("Review code for issues")
  .argument("language", "Programming language")
  .argument("code", "Code to review")
  .get: args =>
    val lang = args.getOrElse("language", "unknown")
    val code = args.getOrElse("code", "")
    ZIO.succeed(PromptGetResult(
      messages = Chunk(PromptMessage(
        role = "user",
        content = ToolContent.text(s"Review this $lang code:\n$code"),
      )),
    ))
```

## Server Assembly

Combine tools, resources, and prompts into a server:

```scala
val server = McpServer("my-server", "1.0.0")
  .tool(greetTool)
  .tool(queryTool)
  .resource(configResource)
  .resourceTemplate(userResource)
  .prompt(codeReviewPrompt)
```

The server auto-declares capabilities based on what's registered.

### HTTP Endpoints

`server.routes` provides three endpoints:

| Method | Path | Purpose |
|--------|------|---------|
| POST | `/mcp` | All JSON-RPC requests and notifications |
| GET | `/mcp` | SSE stream for server-initiated messages |
| DELETE | `/mcp` | Session cleanup |

### Running

```scala
object Main extends ZIOAppDefault:
  def run =
    Server.serve(server.routes).provide(
      Server.default,
      // ... your layers
    )
```

Or with a custom port:

```scala
Server.serve(server.routes).provide(
  Server.defaultWith(_.binding("0.0.0.0", 8080)),
)
```

## Dev Info

Release:
```
git tag v0.0.0 -m 0.0.0
git push --atomic origin main v0.0.0
```
