# Spec

Implementation of the Model Context Protocol 2025-11-25 spec:
https://modelcontextprotocol.io/specification/2025-11-25

Technologies:
- Scala 3
- ZIO HTTP
- ZIO JSON
- ZIO Schema
- ZIO Test
- Testcontainers
- Use other Scala libraries if they fit with the ZIO paradigm

## Goal

This library is something that users of Scala & ZIO can plugin to add MCP server & client capabilities to application

The most important thing is the user DSL.
Defining tools, etc has to Effect Oriented / congruent with the ZIO ecosystem.

## Validation

There is an MCP conformance test suite that will validate the impl is compliant.

There should also be integration tests against various client & server impls:
- https://javadocs.dev/mcp
- A Spring AI MCP Client

## Code Guidance

Follow the Zen of James Skill recommendations

---

## Protocol Foundation

### JSON-RPC 2.0

All MCP communication uses JSON-RPC 2.0 over UTF-8. Three message types:

- **Request**: `{ jsonrpc: "2.0", id: RequestId, method: String, params?: Object }` — expects a response
- **Response**: `{ jsonrpc: "2.0", id: RequestId, result: Object }` or `{ jsonrpc: "2.0", id: RequestId, error: { code: Int, message: String, data?: Any } }`
- **Notification**: `{ jsonrpc: "2.0", method: String, params?: Object }` — fire-and-forget, no `id`, no response

`RequestId` = `String | Int` (must be unique per session per requestor)

### Transports

**Streamable HTTP** (only supported transport):
- Single MCP endpoint (e.g. `/mcp`) supporting POST and GET
- POST: Client sends JSON-RPC messages. Server responds with `application/json` or `text/event-stream` (SSE)
- GET: Client opens SSE stream for server-initiated messages
- Session ID via `MCP-Session-Id` header (assigned by server at init, included by client on all subsequent requests)
- Protocol version header: `MCP-Protocol-Version: 2025-11-25`
- Server MUST validate `Origin` header (DNS rebinding protection)
- Supports resumability via SSE event IDs and `Last-Event-ID`

Note: stdio transport is not supported. This library targets HTTP-based deployments via ZIO HTTP.

### Lifecycle

Three-phase: Initialize → Operate → Shutdown

1. Client sends `initialize` request with `protocolVersion`, `capabilities`, `clientInfo`
2. Server responds with `protocolVersion`, `capabilities`, `serverInfo`, optional `instructions`
3. Client sends `notifications/initialized` notification
4. Operation phase: exchange messages per negotiated capabilities
5. Shutdown: HTTP DELETE with session ID

### Capabilities (negotiated at init)

**Server capabilities:**
| Capability | Sub-fields | Description |
|---|---|---|
| `tools` | `listChanged?: Boolean` | Exposes callable tools |
| `resources` | `subscribe?: Boolean`, `listChanged?: Boolean` | Exposes readable resources |
| `prompts` | `listChanged?: Boolean` | Exposes prompt templates |
| `logging` | — | Emits structured log messages |
| `completions` | — | Supports argument auto-completion |

**Client capabilities:**
| Capability | Sub-fields | Description |
|---|---|---|
| `roots` | `listChanged?: Boolean` | Exposes filesystem roots |
| `sampling` | `tools?: Object` | Supports LLM sampling requests from server |
| `elicitation` | `form?: Object`, `url?: Object` | Supports server-initiated user prompts |

### Cross-Cutting: Pagination

`tools/list`, `resources/list`, `resources/templates/list`, `prompts/list` all support opaque cursor-based pagination:
- Request includes optional `cursor: String`
- Response includes optional `nextCursor: String` (absent = end of results)
- Page size determined by server; clients treat cursors as opaque

### Cross-Cutting: Progress

Any request can include `_meta.progressToken: String | Int`. The receiver sends `notifications/progress`:
```json
{ "progressToken": "abc", "progress": 50, "total": 100, "message": "Processing..." }
```
`progress` MUST increase with each notification. `total` and `message` are optional.

### Cross-Cutting: Cancellation

Either side can send `notifications/cancelled` with `requestId` and optional `reason` to cancel an in-progress request.

### Error Codes

| Code | Name | Meaning |
|---|---|---|
| `-32700` | Parse error | Invalid JSON |
| `-32600` | Invalid request | Not a valid JSON-RPC request |
| `-32601` | Method not found | Method doesn't exist or capability not declared |
| `-32602` | Invalid params | Bad arguments, unknown tool name, invalid cursor |
| `-32603` | Internal error | Server internal error |
| `-32002` | Resource not found | Resource URI not found |
| `-32042` | URL elicitation required | Server needs user info via elicitation |

---

## DSL

### Server Tools

Tools are functions that the server exposes for LLM-driven invocation. Each tool has a name, input schema, optional output schema, and optional annotations.

#### MCP Tool Wire Format

A `Tool` definition (sent in `tools/list` response):
```json
{
  "name": "get_weather",                    // required, 1-128 chars, [A-Za-z0-9_\-.]
  "title": "Weather Lookup",               // optional, human-readable display name
  "description": "Get weather for a city",  // optional, for LLM context
  "inputSchema": {                          // required, JSON Schema 2020-12
    "type": "object",
    "properties": { "city": { "type": "string" } },
    "required": ["city"]
  },
  "outputSchema": {                         // optional, JSON Schema 2020-12
    "type": "object",
    "properties": { "temp": { "type": "number" }, "conditions": { "type": "string" } },
    "required": ["temp", "conditions"]
  },
  "annotations": {                          // optional hints for clients
    "title": "Weather Lookup",
    "readOnlyHint": true,
    "destructiveHint": false,
    "idempotentHint": true,
    "openWorldHint": true
  }
}
```

A `tools/call` request:
```json
{ "method": "tools/call", "params": { "name": "get_weather", "arguments": { "city": "NYC" } } }
```

A `CallToolResult` response:
```json
{
  "content": [{ "type": "text", "text": "{\"temp\": 22, \"conditions\": \"sunny\"}" }],
  "structuredContent": { "temp": 22, "conditions": "sunny" },
  "isError": false
}
```

Content blocks in results can be: `TextContent`, `ImageContent`, `AudioContent`, `ResourceLink`, `EmbeddedResource`. Each supports optional `annotations` with `audience: ["user" | "assistant"]`, `priority: 0.0-1.0`, `lastModified: ISO8601`.

When `outputSchema` is defined, the result MUST include `structuredContent` conforming to that schema. For backward compatibility, SHOULD also include serialized JSON in a `TextContent` block.

Tool execution errors (bad input, business logic failures) use `isError: true` in the result — these are LLM-actionable. Protocol errors (unknown tool, malformed request) use JSON-RPC error responses.

#### Scala DSL — Tool Definition

Use ZIO Schema to derive `inputSchema` and `outputSchema` from Scala case classes. The tool handler is a ZIO effect.

```scala
// Domain types (parse, don't validate — Zen of James)
opaque type City = String
object City:
  def apply(s: String): City = s
  given Schema[City] = Schema[String]
  given CanEqual[City, City] = CanEqual.derived

// Input / Output as case classes with ZIO Schema derivation
case class GetWeatherInput(city: City) derives Schema
case class WeatherOutput(temp: Double, conditions: String) derives Schema

// Tool definition
val getWeather = McpTool("get_weather")
  .description("Get current weather for a city")
  .annotations(readOnly = true, idempotent = true, openWorld = true)
  .handle[GetWeatherInput, WeatherOutput] { input =>
    // ZIO effect — can use any ZIO service in the environment
    WeatherService.lookup(input.city)
  }
```

Key design points:
- `inputSchema` derived from `Schema[GetWeatherInput]` → JSON Schema object
- `outputSchema` derived from `Schema[WeatherOutput]` → JSON Schema object (presence of output type param signals structured output)
- Handler returns `ZIO[R, E, WeatherOutput]` — library serializes to both `content` (TextContent) and `structuredContent`
- For tools with no input: `handle[Unit, Out]` or just `handle[Out]` using `{ "type": "object", "additionalProperties": false }` as inputSchema
- For tools returning unstructured content (text, images, etc.) instead of structured output: `handleRaw[In] { input => ZIO.succeed(ToolContent.text("...")) }` — no `outputSchema` emitted

#### Scala DSL — Tool Annotations

```scala
case class ToolAnnotations(
  title: Option[String] = None,
  readOnlyHint: Option[Boolean] = None,
  destructiveHint: Option[Boolean] = None,
  idempotentHint: Option[Boolean] = None,
  openWorldHint: Option[Boolean] = None,
)
```

Builder methods on `McpTool`:
```scala
val deleteTool = McpTool("delete_record")
  .description("Permanently delete a record")
  .annotations(destructive = true, idempotent = false)
  .handle[DeleteInput, DeleteResult] { input => ... }
```

#### Scala DSL — Tool Content (for unstructured results)

ADT for result content blocks:
```scala
enum ToolContent:
  case Text(text: String, annotations: Option[ContentAnnotations] = None)
  case Image(data: String, mimeType: String, annotations: Option[ContentAnnotations] = None)  // base64
  case Audio(data: String, mimeType: String, annotations: Option[ContentAnnotations] = None)  // base64
  case ResourceLink(uri: String, name: String, title: Option[String] = None, mimeType: Option[String] = None)
  case EmbeddedResource(resource: ResourceContents, annotations: Option[ContentAnnotations] = None)

case class ContentAnnotations(
  audience: Option[NonEmptyChunk[Role]] = None,  // NonEmpty — make illegal states unrepresentable
  priority: Option[Double] = None,               // 0.0 to 1.0
  lastModified: Option[java.time.Instant] = None,
)
```

#### Scala DSL — Server Assembly & ZIO HTTP Integration

Compose tools into an MCP server and mount as ZIO HTTP routes:

```scala
val mcpServer = McpServer("my-server", "1.0.0")
  .tool(getWeather)
  .tool(deleteTool)

// Produces Routes that handle the Streamable HTTP transport
val routes: Routes[WeatherService & DeleteService, Nothing] = mcpServer.routes

// Mount alongside other app routes
val app = routes ++ myOtherRoutes
Server.serve(app)
```

The library handles:
- `POST /` — JSON-RPC message dispatch (initialize, tools/list, tools/call, ping, etc.)
- `GET /` — SSE stream for server-initiated messages (notifications)
- `DELETE /` — session termination
- Session management (`MCP-Session-Id` header)
- Protocol version header validation (`MCP-Protocol-Version`)
- Origin header validation
- Capability negotiation (declares `tools: {}` — tool list is static, `listChanged` is not supported)
- Pagination of `tools/list` responses
- Progress notification forwarding
- Cancellation handling (interrupts the ZIO fiber for the tool call)

Note: Dynamic tool registration (add/remove at runtime) is not supported. The tool list is fixed at server construction time.

#### Scala DSL — Error Handling

Tool handlers signal errors via the ZIO error channel:

```scala
.handle[Input, Output] { input =>
  ZIO.fail(ToolError("Invalid date: must be in the future"))
  // → CallToolResult with isError: true, content: [TextContent with error message]
}
```

Protocol-level errors (unknown tool, malformed JSON) are handled by the library automatically as JSON-RPC error responses.

### Client Tools

The MCP client connects to one or more MCP servers, discovers their tools, and invokes them.

#### MCP Client Wire Format

Client sends `initialize` with capabilities, then `tools/list` to discover tools, then `tools/call` to invoke them.

#### Scala DSL — Client Configuration

```scala
val client = McpClient(
  clientInfo = ClientInfo("my-app", "1.0.0"),
  url = "http://localhost:8080/mcp",
)
```

#### Scala DSL — Tool Discovery

```scala
for
  session  <- client.connect                        // initialize handshake
  tools    <- session.listTools                     // tools/list (handles pagination automatically)
  // tools: Chunk[ToolDefinition] — each has name, description, inputSchema, etc.
yield tools
```

`ToolDefinition` is a data class representing the wire `Tool` object:
```scala
case class ToolDefinition(
  name: String,
  title: Option[String],
  description: Option[String],
  inputSchema: JsonSchema,
  outputSchema: Option[JsonSchema],
  annotations: Option[ToolAnnotations],
)
```

#### Scala DSL — Tool Invocation

Untyped (pass raw JSON arguments):
```scala
for
  result <- session.callTool("get_weather", Json.Obj("city" -> Json.Str("NYC")))
  // result: CallToolResult — content blocks + optional structuredContent + isError flag
yield result
```

Typed (use ZIO Schema for serialization/deserialization):
```scala
for
  result <- session.callTool[GetWeatherInput, WeatherOutput](
    "get_weather",
    GetWeatherInput(City("NYC"))
  )
  // result: WeatherOutput (deserialized from structuredContent)
yield result
```

#### Scala DSL — Progress & Cancellation

```scala
for
  result <- session.callTool("long_task", args)
    .withProgress { progress =>
      // progress: ProgressNotification(progress: Double, total: Option[Double], message: Option[String])
      Console.printLine(s"${progress.progress}/${progress.total.getOrElse("?")} - ${progress.message.getOrElse("")}")
    }
    .timeout(30.seconds)  // sends notifications/cancelled on timeout
yield result
```

#### Scala DSL — Listening for Tool List Changes

```scala
for
  _ <- session.onToolsChanged {
    // re-discover tools, update local state
    session.listTools.flatMap(tools => updateToolCache(tools))
  }
yield ()
```

#### Scala DSL — Client Capabilities (Roots, Sampling)

Clients can expose capabilities that servers may request:

```scala
val client = McpClient(
  clientInfo = ClientInfo("my-app", "1.0.0"),
  url = "http://localhost:8080/mcp",
)
  .withRoots(
    Root("file:///home/user/project", "My Project"),
  )
  .withSampling { request =>
    // Handle sampling/createMessage — forward to your LLM provider
    myLlm.complete(request.messages, request.maxTokens)
  }
```

### Other MCP Features (later phases)

#### Resources (Server Feature)

Servers expose data (files, DB records, API responses) as URI-addressable resources.

Methods: `resources/list`, `resources/read`, `resources/templates/list`, `resources/subscribe`
Notifications: `notifications/resources/list_changed`, `notifications/resources/updated`

DSL sketch:
```scala
val mcpServer = McpServer("my-server", "1.0.0")
  .resource(
    McpResource("file:///config.json")
      .name("config.json")
      .mimeType("application/json")
      .read { uri => ZIO.succeed(ResourceContents.text(configJson)) }
  )
  .resourceTemplate(
    McpResourceTemplate("file:///{path}")
      .name("Project Files")
      .read { params => readFile(params("path")) }
  )
```

#### Prompts (Server Feature)

Servers expose structured prompt templates for user-driven selection (e.g. slash commands).

Methods: `prompts/list`, `prompts/get`
Notification: `notifications/prompts/list_changed`

DSL sketch:
```scala
val mcpServer = McpServer("my-server", "1.0.0")
  .prompt(
    McpPrompt("code_review")
      .description("Review code for quality")
      .argument("code", required = true)
      .get { args =>
        ZIO.succeed(Chunk(PromptMessage.user(ToolContent.Text(s"Review this code:\n${args("code")}"))))
      }
  )
```

#### Logging (Server Feature)

Server sends structured log messages to client. Levels follow RFC 5424 syslog:
`debug`, `info`, `notice`, `warning`, `error`, `critical`, `alert`, `emergency`

Methods: `logging/setLevel`
Notification: `notifications/message`

#### Completions (Server Feature)

Auto-completion for prompt arguments and resource template URI parameters.

Method: `completion/complete`

#### Elicitation (Client Feature)

Server requests additional info from user via client. Two modes: `form` (structured data) and `url` (redirect to external page for sensitive input like OAuth).

Method: `elicitation/create`
Notification: `notifications/elicitation/complete`

#### Sampling (Client Feature)

Server requests LLM completion from client. Enables agentic server behavior without server-side API keys.

Method: `sampling/createMessage`
Supports tool use within sampling (multi-turn tool loops).
