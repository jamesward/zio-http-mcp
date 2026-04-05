# ZIO HTTP MCP — User Guide

A Scala 3 / ZIO library for building [Model Context Protocol](https://modelcontextprotocol.io/) servers using ZIO HTTP. Implements the MCP 2025-11-25 spec with Streamable HTTP transport.

## Dependencies

```scala
libraryDependencies ++= Seq(
  "com.jamesward" %% "zio-http-mcp" % "<version>",
)
```

Transitive dependencies include ZIO, ZIO HTTP, ZIO Schema, and ZIO JSON.

## Quick Start

```scala
import com.jamesward.ziohttp.mcp.*
import zio.*
import zio.http.*
import zio.schema.Schema

case class GreetInput(name: String) derives Schema
case class GreetOutput(greeting: String) derives Schema

val greetTool = McpTool("greet")
  .description("Greet someone by name")
  .handle[GreetInput, GreetOutput]: input =>
    ZIO.succeed(GreetOutput(s"Hello, ${input.name}!"))

val server = McpServer("my-server", "1.0.0")
  .tool(greetTool)

object Main extends ZIOAppDefault:
  def run =
    Server.serve(server.routes).provide(Server.defaultWith(_.port(3000)))
```

The server exposes POST/GET/DELETE on `/mcp` as required by the MCP Streamable HTTP transport.

## Tools

Tools are the primary way to expose functionality to MCP clients. There are three handler styles depending on what you need.

### `handle[In, Out]` — Structured Input/Output

Both input and output types derive `Schema`. The library automatically generates JSON Schema for both and includes `structuredContent` in the response.

```scala
case class AddInput(a: Int, b: Int) derives Schema
case class AddOutput(result: Int) derives Schema

val addTool = McpTool("add")
  .description("Add two numbers")
  .handle[AddInput, AddOutput]: input =>
    ZIO.succeed(AddOutput(input.a + input.b))
```

### `handleDirect[In]` — Structured Input, Raw Content Output

Use when you need to return content types directly (text, images, audio, embedded resources) rather than a structured output type.

```scala
case class EchoInput(message: String) derives Schema

val echoTool = McpTool("echo")
  .description("Echo a message back")
  .handleDirect[EchoInput]: input =>
    ZIO.succeed(Chunk(ToolContent.text(input.message)))
```

### `handleDirectWithContext[In]` — With Tool Context

Use when your tool needs to emit log messages, report progress, request sampling from the client, or trigger elicitation.

```scala
case class SlowInput(steps: Int) derives Schema

val slowTool = McpTool("slow-task")
  .description("A task that reports progress")
  .handleDirectWithContext[SlowInput]: (input, ctx) =>
    for
      _ <- ZIO.foreach(1 to input.steps): step =>
             ctx.progress(step.toDouble, input.steps.toDouble, Some(s"Step $step"))
               *> ctx.log(LogLevel.Info, s"Completed step $step")
               *> ZIO.sleep(100.millis)
    yield Chunk(ToolContent.text(s"Completed ${input.steps} steps"))
```

### Tool Annotations

Add MCP tool annotations to provide hints about tool behavior:

```scala
val readOnlyTool = McpTool("list-files")
  .description("List files in a directory")
  .annotations(
    title = Some("List Files"),
    readOnly = Some(true),
    destructive = Some(false),
    idempotent = Some(true),
    openWorld = Some(false),
  )
  .handle[ListInput, ListOutput](...)
```

### Error Handling

Return `ToolError` from handler functions to signal an error to the client. This sets `isError: true` in the response.

```scala
val divTool = McpTool("divide")
  .description("Divide two numbers")
  .handle[DivInput, DivOutput]: input =>
    if input.b == 0 then ZIO.fail(ToolError("Division by zero"))
    else ZIO.succeed(DivOutput(input.a.toDouble / input.b))
```

### Raw JSON Schema

For tools that need a hand-crafted JSON Schema (e.g., using JSON Schema 2020-12 features not covered by ZIO Schema):

```scala
import zio.json.ast.Json

val rawSchemaTool = McpTool("validate")
  .description("Validate data against a schema")
  .withRawInputSchema(Json.Obj(Chunk(
    "type" -> Json.Str("object"),
    "properties" -> Json.Obj(Chunk(
      "value" -> Json.Obj(Chunk("type" -> Json.Str("string"))),
    )),
  )))
  .handleDirect: args =>
    val value = args.flatMap(_.get("value")).flatMap(_.asString).getOrElse("")
    ZIO.succeed(Chunk(ToolContent.text(s"Received: $value")))
```

## Resources

Resources expose data that clients can read by URI.

### Static Resources

```scala
val configResource = McpResource("config://app", "App Config")
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

Resource templates use URI templates with placeholders:

```scala
val userResource = McpResourceTemplate("user://{userId}/profile", "User Profile")
  .description("User profile data")
  .mimeType("application/json")
  .read: uri =>
    // uri is the resolved URI, e.g. "user://42/profile"
    val userId = uri.stripPrefix("user://").stripSuffix("/profile")
    ZIO.succeed(Chunk(ResourceContents(
      uri = uri,
      mimeType = Some("application/json"),
      text = Some(s"""{"id": "$userId", "name": "User $userId"}"""),
    )))
```

### Binary Resources

Use the `blob` field with Base64-encoded data:

```scala
val imageResource = McpResource("asset://logo", "Logo")
  .mimeType("image/png")
  .read: uri =>
    ZIO.succeed(Chunk(ResourceContents(
      uri = uri,
      mimeType = Some("image/png"),
      blob = Some(base64EncodedData),
    )))
```

## Prompts

Prompts define reusable prompt templates that clients can retrieve.

```scala
val codeReviewPrompt = McpPrompt("code-review")
  .description("Review code for best practices")
  .argument("language", "Programming language", required = true)
  .argument("code", "Code to review", required = true)
  .get: args =>
    val lang = args.getOrElse("language", "unknown")
    val code = args.getOrElse("code", "")
    ZIO.succeed(PromptGetResult(
      description = Some("Code review prompt"),
      messages = Chunk(PromptMessage(
        role = "user",
        content = ToolContent.text(s"Review this $lang code:\n\n$code"),
      )),
    ))
```

## Content Types

`ToolContent` is an ADT representing the different content types in MCP responses:

```scala
// Text content
ToolContent.text("Hello, world!")

// Image content (Base64-encoded)
ToolContent.image(base64Data, "image/png")

// Audio content (Base64-encoded)
ToolContent.audio(base64Data, "audio/wav")

// Embedded resource
ToolContent.embeddedResource(ResourceContents(
  uri = "file://example.txt",
  mimeType = Some("text/plain"),
  text = Some("file contents"),
))
```

Return multiple content items from a `handleDirect` tool:

```scala
val multiTool = McpTool("multi-content")
  .handleDirect[MyInput]: input =>
    ZIO.succeed(Chunk(
      ToolContent.text("Here is an image:"),
      ToolContent.image(imageData, "image/png"),
    ))
```

## Tool Context

`McpToolContext` is available in `handleDirectWithContext` handlers and provides four capabilities:

### Logging

Send log messages to the client as server-sent event notifications:

```scala
ctx.log(LogLevel.Info, "Processing started")
ctx.log(LogLevel.Warning, "Rate limit approaching")
ctx.log(LogLevel.Error, "Failed to connect to database")
```

Available log levels: `Debug`, `Info`, `Warning`, `Error`, `Critical`, `Alert`, `Emergency`.

### Progress

Report progress for long-running operations. The client must send a `progressToken` in the `_meta` field of the `tools/call` request for progress notifications to be delivered.

```scala
ctx.progress(current = 5.0, total = 10.0, message = Some("Halfway done"))
```

### Sampling

Request the client to perform LLM sampling (generate text):

```scala
val result: SamplingResult = ctx.sample("Summarize this text: ...", maxTokens = 200)
// result.role    — "assistant"
// result.content — ToolContent with the generated text
// result.model   — model used by the client
```

### Elicitation

Request structured input from the user via the client:

```scala
val result: ElicitationResult = ctx.elicit(
  "What is your name?",
  Json.Obj(Chunk(
    "type" -> Json.Str("object"),
    "properties" -> Json.Obj(Chunk(
      "name" -> Json.Obj(Chunk(
        "type" -> Json.Str("string"),
        "description" -> Json.Str("Your full name"),
      )),
    )),
    "required" -> Json.Arr(Chunk(Json.Str("name"))),
  )),
)
// result.action  — "accept", "decline", or "cancel"
// result.content — Some(Map("name" -> Json.Str("Alice"))) if accepted
```

## Assembling the Server

Chain `.tool()`, `.resource()`, `.resourceTemplate()`, and `.prompt()` calls:

```scala
val server = McpServer("my-server", "1.0.0")
  .tool(greetTool)
  .tool(addTool)
  .resource(configResource)
  .resourceTemplate(userResource)
  .prompt(codeReviewPrompt)
```

The server automatically advertises the correct capabilities based on what you register (tools, resources, prompts, logging, completions).

## Running with ZIO HTTP

```scala
object Main extends ZIOAppDefault:
  def run =
    Server.serve(server.routes).provide(Server.defaultWith(_.port(3000)))
```

The server handles all MCP Streamable HTTP transport concerns:
- `POST /mcp` — JSON-RPC requests and notifications
- `GET /mcp` — SSE stream for server-initiated messages
- `DELETE /mcp` — Session termination
- Session management via `MCP-Session-Id` header
- DNS rebinding protection via Origin header validation

## Complete Example

```scala
import com.jamesward.ziohttp.mcp.*
import zio.*
import zio.http.*
import zio.schema.Schema

// Domain types
case class CalcInput(operation: String, a: Double, b: Double) derives Schema
case class CalcOutput(result: Double) derives Schema

object CalculatorServer extends ZIOAppDefault:

  val calcTool = McpTool("calculate")
    .description("Perform arithmetic operations")
    .annotations(readOnly = Some(true), idempotent = Some(true))
    .handle[CalcInput, CalcOutput]: input =>
      input.operation match
        case "add"      => ZIO.succeed(CalcOutput(input.a + input.b))
        case "subtract" => ZIO.succeed(CalcOutput(input.a - input.b))
        case "multiply" => ZIO.succeed(CalcOutput(input.a * input.b))
        case "divide"   =>
          if input.b == 0 then ZIO.fail(ToolError("Division by zero"))
          else ZIO.succeed(CalcOutput(input.a / input.b))
        case other => ZIO.fail(ToolError(s"Unknown operation: $other"))

  val helpPrompt = McpPrompt("help")
    .description("Get help with the calculator")
    .get: _ =>
      ZIO.succeed(PromptGetResult(
        messages = Chunk(PromptMessage(
          role = "user",
          content = ToolContent.text("Available operations: add, subtract, multiply, divide"),
        )),
      ))

  val server = McpServer("calculator", "1.0.0")
    .tool(calcTool)
    .prompt(helpPrompt)

  def run =
    Server.serve(server.routes).provide(Server.defaultWith(_.port(3000)))
```
