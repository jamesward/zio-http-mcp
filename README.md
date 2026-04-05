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
      .handle[Any, Nothing, NameInput, String]: input =>
        ZIO.succeed(s"Hello, ${input.name}!")
  )

object Main extends ZIOAppDefault:
  def run =
    Server.serve(server.routes).provide(Server.default)
```

## Tools

Tools are the primary way to expose functionality to MCP clients. Define input/output types as case classes with `derives Schema`, and the library generates JSON Schema automatically.

### Basic Tool

```scala
case class AddInput(a: Int, b: Int) derives Schema

val addTool = McpTool("add")
  .description("Adds two numbers")
  .handle[Any, Nothing, AddInput, Int]: input =>
    ZIO.succeed(input.a + input.b)
```

The type parameters on `handle` are `[R, E, In, Out]`:

| Param | Meaning |
|-------|---------|
| `R` | ZIO environment requirements (use `Any` for none) |
| `E` | Error type (use `Nothing` for infallible tools) |
| `In` | Input type (must have `Schema`) |
| `Out` | Output type (must have `Schema`) |

### Tools with ZIO Layers

Tools can declare ZIO environment requirements. These propagate through the server to the routes, just like ZIO HTTP:

```scala
trait Database:
  def query(sql: String): IO[ToolError, String]

object Database:
  val live: ULayer[Database] = ZLayer.succeed(???)

case class QueryInput(sql: String) derives Schema

val queryTool = McpTool("query")
  .description("Runs a database query")
  .handle[Database, ToolError, QueryInput, String]: input =>
    ZIO.serviceWithZIO[Database](_.query(input.sql))
```

When tools with different requirements are added to a server, the requirements accumulate:

```scala
val server = McpServer("my-server", "1.0.0")
  .tool(queryTool)   // needs Database
  .tool(cacheTool)   // needs Cache

// server.routes: Routes[Database & Cache, Response]
// Provide layers when serving:
Server.serve(server.routes).provide(
  Server.default,
  Database.live,
  Cache.live,
)
```

Tools with no environment requirements (`R = Any`) don't add to the server's requirements.

### Error Handling

Tool handler errors are converted to MCP tool error responses (`isError: true`) using the `McpError[E]` typeclass. You must provide an instance for any custom error type:

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

Built-in `McpError` instances are provided for `ToolError`, `String`, `Throwable`, and `Nothing`.

No errors propagate out of the MCP server. Protocol-level errors (invalid JSON, unknown method, etc.) are handled internally as JSON-RPC error responses. Tool execution errors become `CallToolResult` with `isError: true`.

### Tool Annotations

```scala
val tool = McpTool("delete_user")
  .description("Deletes a user account")
  .annotations(
    destructive = Some(true),
    idempotent = Some(true),
  )
  .handle[Any, Nothing, DeleteInput, String](...)
```

### Direct Content Tools

For fine-grained control over response content (images, audio, embedded resources), use `handleDirect`:

```scala
val imageTool = McpTool("screenshot")
  .description("Takes a screenshot")
  .handleDirect[EmptyInput]: _ =>
    for
      data <- takeScreenshot()
    yield Chunk(
      ToolContent.text("Screenshot captured:"),
      ToolContent.image(base64Data, "image/png"),
    )
```

Content types: `ToolContent.text`, `ToolContent.image`, `ToolContent.audio`, `ToolContent.embeddedResource`.

### Tools with Context (Logging, Progress, Sampling, Elicitation)

Use `handleDirectWithContext` to access `McpToolContext` during execution:

```scala
val processTool = McpTool("process")
  .description("Processes data with progress")
  .handleDirectWithContext[ProcessInput]: (input, ctx) =>
    for
      _ <- ctx.log(LogLevel.Info, "Starting processing")
      _ <- ctx.progress(0, 100)
      result <- doWork(input)
      _ <- ctx.progress(100, 100)
      _ <- ctx.log(LogLevel.Info, "Done")
    yield Chunk(ToolContent.text(result))
```

`McpToolContext` provides:

| Method | Description |
|--------|-------------|
| `ctx.log(level, message)` | Send log notification to client |
| `ctx.progress(current, total)` | Send progress notification (requires `progressToken` in request) |
| `ctx.sample(prompt, maxTokens)` | Request LLM completion from client |
| `ctx.elicit(message, schema)` | Request user input from client with a JSON Schema form |

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

The server auto-declares capabilities based on what's registered:

- `tools` capability if any tools are added
- `resources` capability (with subscribe support) if any resources or templates are added
- `prompts` capability if any prompts are added
- `logging` and `completions` are always enabled

### HTTP Endpoints

`server.routes` provides three endpoints:

| Method | Path | Purpose |
|--------|------|---------|
| POST | `/mcp` | All JSON-RPC requests and notifications |
| GET | `/mcp` | SSE keepalive stream |
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
  // ... your layers
)
```

## Dev Info

Release:
```
git tag v0.0.0 -m 0.0.0
git push --atomic origin main v0.0.0
```
