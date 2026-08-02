zio-http-mcp
------------

An MCP (Model Context Protocol) server and client library for Scala 3, ZIO, and ZIO HTTP.

Implements the [MCP 2025-11-25 specification](https://modelcontextprotocol.io) with Streamable HTTP transport, SSE streaming, tools, resources, prompts, sampling, elicitation, and progress notifications. The client supports the Streamable HTTP transport and OAuth 2.1 authorization — both `client_credentials` and the MCP-spec `authorization_code` + PKCE flow with Client ID Metadata Documents (CIMD).

The library is **dual-era**: it also implements the stateless [MCP 2026-07-28 specification](https://modelcontextprotocol.io/specification/2026-07-28) (final) and negotiates the protocol version per connection. See [Protocol version negotiation](#protocol-version-negotiation).

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
    Server.serve(server.routes).provide(Server.default, McpServer.State.default)
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

// server.routes: Routes[Database & Cache & McpServer.State, Response]
Server.serve(server.routes).provide(
  Server.default,
  McpServer.State.default,
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

### Instructions

The MCP `initialize` result carries an optional `instructions` string — a hint describing how to use the server and its capabilities, analogous to a system prompt, which clients may surface to the LLM. Set it with `.instructions(...)`:

```scala
val server = McpServer("my-server", "1.0.0")
  .instructions("Use the add tool to sum two integers.")
  .tool(addTool)
```

It's unset by default. On the client side, `client.instructions` returns the `Option[String]` the server sent (`None` when the server set none).

For instructions that vary per caller or per mount, pass an `InstructionsSource` to the same `.instructions(...)` — the dynamic analogue of the `String` form. It's consulted on every `initialize` with the request's `McpToolContext` (the authenticated `ctx.principal` and any `ctx.pathParams`), and returns `Option[String]`:

```scala
val server = McpServer("multi", "1.0.0")
  .instructions(InstructionsSource: ctx =>
    ZIO.succeed(ctx.pathParams.get("slug").map(slug => s"You are talking to $slug."))
  )
  .tool(addTool)
  .mountedAtParam("slug")
```

Because `instructions` is a single value rather than a combinable collection like tools/resources, the `String` and `InstructionsSource` forms are **mutually exclusive** — calling either clears the other, last one wins. (This differs from `.tool` / `.toolSource`, which are additive.) `InstructionsSource.const("…")` wraps a fixed string as a source.


### HTTP Endpoints

`server.routes` provides stateful Streamable HTTP with session tracking and SSE:

| Method | Path | Purpose |
|--------|------|---------|
| POST | `/mcp` | All JSON-RPC requests and notifications |
| GET | `/mcp` | SSE stream for server-initiated messages |
| DELETE | `/mcp` | Session cleanup |

### Stateless Mode

`server.statelessRoutes` provides a stateless transport where each request is independent — no session tracking, no SSE, and tool calls return plain JSON:

| Method | Path | Purpose |
|--------|------|---------|
| POST | `/mcp` | All JSON-RPC requests and notifications |
| GET | `/mcp` | 405 Method Not Allowed |
| DELETE | `/mcp` | 405 Method Not Allowed |

In stateless mode:
- `initialize` does not return an `Mcp-Session-Id` header
- No session validation on subsequent requests
- Tool calls return `application/json` instead of SSE
- Sampling and elicitation are not available (no persistent connection for server-to-client requests)

```scala
object Main extends ZIOAppDefault:
  def run =
    Server.serve(server.statelessRoutes).provide(Server.default)
```

## Protocol version negotiation

The library supports two protocol revisions and negotiates between them per connection:

- **`2025-11-25` (legacy)** — the `initialize` / `notifications/initialized` handshake, `Mcp-Session-Id` sessions, and server-initiated sampling/elicitation over an SSE back-channel.
- **`2026-07-28` (modern)** — stateless: no handshake and no session. Every request carries its protocol version and client capabilities in `_meta` (`io.modelcontextprotocol/protocolVersion`, `io.modelcontextprotocol/clientInfo`, `io.modelcontextprotocol/clientCapabilities`), `server/discover` advertises capabilities on demand, and server-to-client interactions use the Multi Round-Trip Request (MRTR) pattern instead of an SSE back-channel.

### Server

`server.routes` is **dual-era** — it serves both revisions on the same endpoint, choosing per request:

- A request carrying modern `_meta` (or naming a modern-only method such as `server/discover`) is validated (the `Mcp-Method`, `Mcp-Name`, and `MCP-Protocol-Version` headers must match the body) and served statelessly. Results are wrapped in the modern envelope (`resultType`, `_meta.io.modelcontextprotocol/serverInfo`, and `ttlMs`/`cacheScope` on cacheable results). An unsupported version returns `400` with `UnsupportedProtocolVersionError` (`-32022`) listing the supported versions; a header mismatch returns `400`/`-32020`; an unknown modern method returns `404`/`-32601`. A modern `tools/call` that opts into request-scoped notifications — a `_meta.progressToken` and/or `_meta.io.modelcontextprotocol/logLevel` — is answered as an SSE stream carrying `notifications/progress` / `notifications/message` (filtered to the requested level) followed by the final result; without the opt-in it stays a single JSON result.
- An `initialize` request selects the legacy handshake + session path, unchanged.

The server also implements the modern **Tasks extension** (`io.modelcontextprotocol/tasks`): a `tools/call` whose `_meta` carries the tasks marker runs on a background fiber and returns a task handle (`resultType: "task"`) immediately, which the client polls with `tasks/get` and cancels with `tasks/cancel`.

### Client

`McpClient` prefers the newest revision by default and negotiates automatically — it probes `server/discover`, stays modern (stateless) against a modern server, and falls back to the `initialize` handshake against a legacy one:

```scala
// Newest by default: negotiates modern, or falls back to legacy transparently.
ZIO.scoped:
  McpClient.connect("https://example.com/mcp").flatMap: client =>
    client.listTools
```

Pin the era with `preferredVersion`, and supply an `onInputRequest` handler to satisfy a modern server's MRTR sampling/elicitation requests:

```scala
val config = McpClientConfig(
  serverUrl = "https://example.com/mcp",
  preferredVersion = ProtocolVersion.V2025_11_25,      // force the legacy handshake
  onInputRequest = Some(req => ZIO.succeed(/* answer the sampling/elicitation request */ ???)),
)
```

`client.protocolVersion` reports the negotiated revision.

## Authorization

Authorization is opt-in. A server with no `.auth(...)` call behaves exactly as the examples above — no new headers, no new endpoints, no `R` requirement changes. Add `.auth(...)` to enable OAuth 2.1 bearer-token validation conforming to the [MCP authorization spec](https://modelcontextprotocol.io/specification/2026-07-28/basic/authorization) (compatible with `2025-06-18`, `2025-11-25`, and `2026-07-28` — the resource-server requirements are the same across all three).

The library acts as an **OAuth 2.1 Resource Server**. It does not host an authorization server — point at one (Keycloak, Authentik, Auth0, Spring Authorization Server, etc.). DCR (Dynamic Client Registration), CIMD, and the user-consent flow are AS-side concerns.

### Quick Start

```scala
import com.jamesward.ziohttp.mcp.*
import com.jamesward.ziohttp.mcp.auth.*
import zio.*
import zio.http.*

object Main extends ZIOAppDefault:
  def run =
    val program =
      for
        verifier <- TokenVerifier.discoverJwks(issuer = "https://login.jamesward.dev")
        server = McpServer("example", "1.0.0")
          .tool(greetTool)
          .auth(McpAuth(
            authorizationServers = NonEmptyChunk(AuthorizationServer("https://login.jamesward.dev")),
            verifier = verifier,
            requiredScopes = Set(OauthScope("mcp:tools")),
          ))
        _ <- Server.serve(server.statelessRoutes)
      yield ()
    program.provide(Server.default, Client.default)
```

`McpAuth.resourceUri` is optional. When unset, the library derives the resource URI per request from headers in this order: RFC 7239 `Forwarded` → `X-Forwarded-Proto` + `X-Forwarded-Host` → `Host` (HTTP scheme assumed). This works for localhost dev and for platform deployments (Heroku, Cloudflare, AWS ALB, ngrok) that set `X-Forwarded-*` correctly. For production where you don't fully trust the layer in front to set those headers correctly, **pin** to an explicit value:

```scala
.auth(McpAuth(
  resourceUri = Some(ResourceUri.parse("https://mcp.example.com/mcp").toOption.get),
  authorizationServers = NonEmptyChunk(AuthorizationServer("https://login.jamesward.dev")),
  verifier = verifier,
  requiredScopes = Set(OauthScope("mcp:tools")),
))
```

When auth is enabled the library:
- Serves the [RFC 9728 Protected Resource Metadata](https://datatracker.ietf.org/doc/html/rfc9728) document at both `/.well-known/oauth-protected-resource` and `/.well-known/oauth-protected-resource/<path>`.
- Requires every `/mcp` request to carry an `Authorization: Bearer <token>` header.
- Returns `401 Unauthorized` with a `WWW-Authenticate: Bearer realm=…, resource_metadata=…, scope=…` challenge on missing or invalid tokens, and `403 Forbidden` with `error="insufficient_scope"` when scopes are missing.
- Validates token audience binding ([RFC 8707](https://www.rfc-editor.org/rfc/rfc8707)) — only tokens whose `aud` matches the resolved `resourceUri` are accepted.

### Token Verifiers

`TokenVerifier` is the pluggable seam for token validation. It's responsible for signature, `iss`, `exp`, and `nbf` validation only — audience binding and scope enforcement happen in the auth middleware so that `resourceUri` can be derived per request.

Built-in implementations:

| Verifier | Use when | Requires |
|----------|----------|----------|
| `TokenVerifier.discoverJwks(issuer)` | AS issues JWTs and publishes a JWKS (most common) | `Client` |
| `TokenVerifier.jwks(jwksUri, expectedIssuer)` | JWT validation with a hard-coded JWKS URL (no metadata discovery) | `Client` |
| `TokenVerifier.introspection(endpoint, clientId, clientSecret, expectedIssuer)` | AS issues opaque tokens, or you need real-time revocation | `Client` |
| `TokenVerifier.fromFunction(f)` | Tests, custom flows | `R` you choose |

`discoverJwks` performs RFC 8414 metadata discovery (with an OIDC `/.well-known/openid-configuration` fallback) and caches the JWKS document by `kid`. RSA signatures (RS256/RS384/RS512) are supported in v1.

### Reading the Principal

When auth is configured, `ctx.principal` holds the verified caller identity:

```scala
val whoami = McpTool("whoami")
  .description("Returns the authenticated subject")
  .handleWithContext: ctx =>
    ZIO.succeed(ctx.principal.flatMap(_.subject).getOrElse("anonymous"))
```

`Principal` exposes:

| Field | Meaning |
|-------|---------|
| `subject` | `sub` claim |
| `clientId` | `client_id` / `azp` |
| `scopes` | parsed `scope` claim |
| `audience` | `aud` claim entries |
| `issuer` | `iss` claim |
| `expiresAt` | `exp` claim |
| `claims` | full claim set as `Json.Obj` |

`ctx.principal` returns `None` when auth is not enabled.

### Per-Tool Scopes

Tools can declare additional scope requirements on top of the server-wide `requiredScopes`:

```scala
val deleteUser = McpTool("delete_user")
  .description("Deletes a user account")
  .requireScopes(OauthScope("admin"))
  .handle[Any, ToolError, DeleteInput, String]: input =>
    ZIO.succeed(s"deleted ${input.userId}")
```

Calls without the `admin` scope yield `403 Forbidden` with a step-up challenge:

```
WWW-Authenticate: Bearer realm="mcp",
  resource_metadata="https://mcp.example.com/.well-known/oauth-protected-resource",
  error="insufficient_scope",
  scope="mcp:tools admin"
```

When auth is **not** configured, `.requireScopes(...)` is silently ignored — auth authoring stays fully opt-in.

### Restricting to User-Authenticated Calls

By default, *any* token the AS issues with the right `aud`, `iss`, and `scope` is accepted — including `client_credentials` tokens (machine-to-machine, no human present). For most production MCP servers you'll want to require that a token came from a user-facing flow (`authorization_code` + PKCE, the flow VS Code or Claude Desktop use).

The library doesn't enforce a flow distinction itself, because what counts as "user-authenticated" depends on your AS. The verifier hands you the full JWT claim set in `ctx.principal`; gate inside your tool handlers using whatever signal your AS provides.

Common patterns:

**1. Require specific scopes that your AS only attaches to user flows.**

Most authorization servers can be configured so that scopes like `openid` / `profile` / `offline_access` (or domain-specific ones) are *only* granted to authorization-code flows. Require those scopes server-wide:

```scala
McpAuth(
  resourceUri = …,
  authorizationServers = …,
  verifier = verifier,
  requiredScopes = Set(OauthScope("mcp:tools"), OauthScope("openid")),
)
```

A `client_credentials` token won't have `openid` and will be rejected with a 403 step-up challenge.

**2. Reject tokens where `sub == client_id`.**

In `client_credentials`, the `sub` claim equals the OAuth `client_id`. In an `authorization_code` flow, `sub` is the user's identifier and is distinct from the client ID. This is a heuristic — verify against your specific AS — but generally:

```scala
val tool = McpTool("user_only")
  .handleWithContext[Any, ToolError, Input, String]: (in, ctx) =>
    ctx.principal match
      case Some(p) if p.subject != p.clientId =>
        // user-flow token: sub is the user
        ZIO.succeed(s"hello ${p.subject.getOrElse("anon")}")
      case _ =>
        ZIO.fail(ToolError("This tool requires user authentication"))
```

**3. Check `acr` / `amr` claims.**

If your AS includes [Authentication Context Class Reference](https://www.rfc-editor.org/rfc/rfc8176) claims, you can require specific authentication strengths:

```scala
.handleWithContext: ctx =>
  val isUser = ctx.principal.exists(_.claims.get("amr").exists(_ != zio.json.ast.Json.Null))
  if isUser then ZIO.succeed("ok") else ZIO.fail(ToolError("user auth required"))
```

**4. Pin to an allowlist of client IDs.**

If your deployment knows exactly which OAuth clients are permitted (e.g. only the Claude Desktop client and your own SPA), gate on `clientId`:

```scala
val allowedClients = Set("claude-desktop", "my-internal-spa")

.handleWithContext: ctx =>
  if ctx.principal.flatMap(_.clientId).exists(allowedClients.contains) then …
  else ZIO.fail(ToolError("client not authorized"))
```

For a deployment that wants this enforced uniformly across every tool, the cleanest approach is a small middleware-style helper that wraps each handler — or a custom `TokenVerifier` that fails with `AuthError.Invalid("user authentication required")` for tokens that don't meet your criteria, so the rejection happens at the bearer layer (401) rather than per-tool.

### Authorization Server Requirements

The library works against any AS that:
- Publishes [RFC 8414 Authorization Server Metadata](https://datatracker.ietf.org/doc/html/rfc8414) (or [OIDC Discovery 1.0](https://openid.net/specs/openid-connect-discovery-1_0.html)),
- Supports the [RFC 8707 `resource` parameter](https://www.rfc-editor.org/rfc/rfc8707) (for audience binding),
- Either signs JWTs with a JWKS-published key (preferred) or exposes [RFC 7662 token introspection](https://www.rfc-editor.org/rfc/rfc7662).

For clients with no pre-existing relationship, the AS should support [Client ID Metadata Documents](https://datatracker.ietf.org/doc/html/draft-ietf-oauth-client-id-metadata-document-00) (advertised via `client_id_metadata_document_supported`, the `2026-07-28` spec's preferred mechanism) and/or [RFC 7591 Dynamic Client Registration](https://datatracker.ietf.org/doc/html/rfc7591) (deprecated in `2026-07-28`, retained for backwards compatibility). Both are verified to work against `https://login.jamesward.dev`, which supports open DCR and CIMD.

#### Hosting a Client ID Metadata Document

When using CIMD, the client hosts a JSON document at the HTTPS URL it uses as its `client_id`. Two requirements bite in practice:

- The document's `client_id` **must equal its own URL exactly**. Authorization servers reject a mismatch.
- Serve it as **`Content-Type: application/json`**. A document served as `text/plain` is rejected — which rules out `raw.githubusercontent.com` as a host; a CDN that sets the JSON content type (jsDelivr, for example) or your own server works.

### Running

```scala
object Main extends ZIOAppDefault:
  def run =
    Server.serve(server.routes).provide(
      Server.default,
      McpServer.State.default,
      // ... your layers
    )
```

Or with a custom port:

```scala
Server.serve(server.routes).provide(
  Server.defaultWith(_.binding("0.0.0.0", 8080)),
  McpServer.State.default,
)
```

## Client

The library also ships an MCP **client** for the Streamable HTTP transport. Connect to a server, and the client performs the `initialize` handshake, sends `notifications/initialized`, tracks the `Mcp-Session-Id`, and sends the negotiated `MCP-Protocol-Version` on every subsequent request. Responses are read whether the server replies with `application/json` or streams a `text/event-stream` (SSE).

`McpClient.connect` requires a `Client` and a `Scope`. The connection — including any OAuth token and the server session — lives for the duration of the scope; on scope close the client issues a best-effort `DELETE` to release the session.

```scala
import com.jamesward.ziohttp.mcp.client.*
import zio.*
import zio.http.*
import zio.json.ast.Json

object Main extends ZIOAppDefault:
  def run =
    ZIO.scoped:
      for
        client <- McpClient.connect("https://www.javadocs.dev/mcp")
        _      <- Console.printLine(s"Connected to ${client.serverInfo.name}")
        tools  <- client.listTools
        _      <- Console.printLine(s"Tools: ${tools.map(_.name.value).mkString(", ")}")
        result <- client.callTool("search_artifacts", Json.Obj(Chunk("query" -> Json.Str("zio-http"))))
        _      <- Console.printLine(result.content.mkString)
      yield ()
    .provide(Client.default)
```

The client exposes the core MCP operations:

| Method | MCP request |
|--------|-------------|
| `client.ping` | `ping` |
| `client.listTools` | `tools/list` |
| `client.callTool(name, args)` / `client.callTool(name)` | `tools/call` |
| `client.callTool(name, a)` (typed `a: A` via `Schema`) | `tools/call` |
| `client.callToolAs[B](name, args)` / `callToolAs[A, B](name, a)` | `tools/call` (result decoded into `B`) |
| `client.listResources` | `resources/list` |
| `client.listResourceTemplates` | `resources/templates/list` |
| `client.readResource(uri)` | `resources/read` |
| `client.complete(ref, argument)` | `completion/complete` |
| `client.listPrompts` | `prompts/list` |
| `client.getPrompt(name, args)` / `client.getPrompt(name)` | `prompts/get` |

Errors surface as a typed `McpClientError`:

| Case | Meaning |
|------|---------|
| `Transport` | the HTTP request itself failed (connection, timeout, TLS, IO) |
| `Protocol` | a Streamable HTTP / JSON-RPC framing violation (bad status, garbled body) |
| `JsonRpc(code, message, data)` | the server returned a JSON-RPC error object |
| `Decode` | the `result` payload didn't match the expected type |
| `Auth` | the OAuth flow failed, or a 401 persisted after refreshing the token |
| `ToolFailed` | a typed `callToolAs` call ran but the tool reported `isError: true` |

### Typed tool calls

`callTool`/`callToolAs` have `Schema`-based overloads that reuse the same `zio-schema` codecs the server uses to derive a tool's `inputSchema` and `outputSchema`. When you share the input/output types with the server (or model them yourself), a successful encode/decode round-trip *is* the schema-conformance check — there's no separate JSON Schema validation step, because the codec already enforces required fields, types, and structure.

```scala
import zio.schema.*

case class AddInput(a: Int, b: Int) derives Schema
case class AddOutput(result: Int) derives Schema

// typed input — encoded with the tool's input Schema
result <- client.callTool("add", AddInput(5, 3))

// typed output — structuredContent decoded into AddOutput (decode = validation)
out <- client.callToolAs[AddOutput]("add", Json.Obj(Chunk("a" -> Json.Num(5), "b" -> Json.Num(3))))

// both ends typed
out <- client.callToolAs[AddInput, AddOutput]("add", AddInput(4, 4))
```

`callToolAs` decodes the tool's `structuredContent` (falling back to its text content parsed as JSON). It fails with `McpClientError.ToolFailed` if the tool reported `isError`, or `McpClientError.Decode` if the payload doesn't conform to the expected type. The raw `callTool(name, json)` overload stays as the escape hatch for calling tools whose types you don't have locally — there the server remains the authority on argument validation.

Note that decoding validates *shape, types, and required fields*, not JSON-Schema value constraints (`minimum`, `pattern`, an un-modeled `enum`, …); those are enforced server-side.

### OAuth 2.1 (client_credentials)

For servers that require authorization, supply `OAuthClientCredentials`. The client runs the machine-to-machine `client_credentials` flow with automatic discovery per the [MCP authorization spec](https://modelcontextprotocol.io/specification/2025-11-25/basic/authorization):

1. Probe the MCP endpoint; on `401` read the `resource_metadata` URL from the `WWW-Authenticate` challenge (falling back to the well-known path under the server origin).
2. Fetch the [RFC 9728](https://datatracker.ietf.org/doc/html/rfc9728) Protected Resource Metadata to learn the canonical `resource` (audience) and the `authorization_servers`.
3. Fetch the [RFC 8414](https://datatracker.ietf.org/doc/html/rfc8414) Authorization Server Metadata (OIDC discovery fallback) to learn the `token_endpoint`.
4. Request a token with `grant_type=client_credentials` (HTTP Basic client auth), binding it to the resource via the [RFC 8707](https://www.rfc-editor.org/rfc/rfc8707) `resource` parameter.

The token is cached and reused until shortly before it expires; a `401` despite a valid-looking token triggers a single refresh-and-retry.

```scala
import com.jamesward.ziohttp.mcp.*
import com.jamesward.ziohttp.mcp.client.*
import zio.*
import zio.http.*

ZIO.scoped:
  for
    client <- McpClient.connect(McpClientConfig(
                serverUrl = "https://mcp.example.com/mcp",
                oauth = Some(OAuthClientCredentials(
                  clientId = "my-client-id",
                  clientSecret = Config.Secret("my-client-secret"),
                  scopes = Set("mcp:tools"),
                )),
              ))
    tools  <- client.listTools
  yield tools
.provide(Client.default)
```

Supply `tokenEndpoint` and/or `resource` on `OAuthClientCredentials` to pin those values and skip the corresponding discovery step.

### OAuth 2.1 (authorization_code + PKCE, CIMD)

For the [MCP `2026-07-28` hardened authorization flow](https://modelcontextprotocol.io/specification/2026-07-28/basic/authorization), supply `OAuthAuthorizationCode`. The client runs the full flow:

1. Probe the MCP endpoint; on `401` read the `resource_metadata` URL and `scope` hint from the `WWW-Authenticate` challenge (falling back to the [RFC 9728](https://datatracker.ietf.org/doc/html/rfc9728) well-known forms — path-inserted first, then root).
2. Fetch the Protected Resource Metadata; **verify its `resource` matches the server URL** (a PRM naming a different resource aborts the flow); pick the advertised authorization server.
3. Discover AS metadata, trying the [RFC 8414](https://datatracker.ietf.org/doc/html/rfc8414) and [OIDC Discovery](https://openid.net/specs/openid-connect-discovery-1_0.html) well-known forms in the spec's priority order (path-aware for issuers with a path component), and verify the metadata `issuer` matches.
4. Obtain a client id by the spec's registration priority: pre-registered `clientId` → **Client ID Metadata Documents** (`clientMetadataUrl` used as the `client_id` when the AS advertises `client_id_metadata_document_supported`) → Dynamic Client Registration (deprecated fallback).
5. Send the authorization request with **PKCE S256**, the [RFC 8707](https://www.rfc-editor.org/rfc/rfc8707) `resource` parameter, and `state`; scopes follow the spec's selection strategy (explicit config → `WWW-Authenticate` `scope` hint → PRM `scopes_supported`).
6. Validate the authorization response: `state` round-trip and the [RFC 9207](https://datatracker.ietf.org/doc/html/rfc9207) `iss` parameter (exact string comparison, no normalization; required when the AS advertises `authorization_response_iss_parameter_supported`).
7. Exchange the code (+ `code_verifier` + `resource`) for tokens; refresh via `grant_type=refresh_token` when the token expires.

```scala
ZIO.scoped:
  for
    client <- McpClient.connect(McpClientConfig(
                serverUrl = "https://mcp.example.com/mcp",
                oauth = Some(OAuthAuthorizationCode(
                  // the client's hosted CIMD document; used as the client_id
                  clientMetadataUrl = Some("https://app.example.com/oauth/client-metadata.json"),
                  redirectUri = "http://127.0.0.1:3000/callback",
                )),
              ))
    tools  <- client.listTools
  yield tools
.provide(Client.default)
```

How the authorization URL reaches the resource owner is pluggable via `OAuthAuthorizationCode.authorization`. The default, `AuthorizationHandler.autoRedirect`, performs the request non-interactively and reads the redirect `Location` — suitable for auto-approving authorization servers and tests. An interactive app supplies its own `AuthorizationHandler` that opens the system browser and captures the redirect on a loopback listener at `redirectUri`.

Pre-registered credentials take priority when set (`clientId` / optional `clientSecret`); with neither `clientId` nor a CIMD-supporting AS, the client falls back to Dynamic Client Registration when the AS has a `registration_endpoint` (registering with `application_type: "native"` for loopback redirect URIs per SEP-837).

The client-side flow is validated two ways: end-to-end in-process tests against a CIMD-capable test IDP (`CimdAuthSpec`), and the official [MCP conformance kit](https://github.com/modelcontextprotocol/conformance)'s client auth scenarios (`ClientConformanceSpec`) — `auth/basic-cimd`, the `auth/iss-*` issuer-validation family, `auth/metadata-*` discovery forms, `auth/resource-mismatch`, `auth/scope-*` selection, `auth/pre-registration`, and `auth/client-credentials-basic`.

### Static auth headers

For an upstream that authenticates with a pre-shared credential — a fixed bearer token or a custom header — rather than the OAuth flow, connect with `McpClient.streamableHttp` (or set `McpClientConfig.headers`). The supplied headers ride every request:

```scala
import zio.http.*

ZIO.scoped:
  for
    url    <- ZIO.fromEither(URL.decode("https://upstream.example.com/mcp")).orDie
    client <- McpClient.streamableHttp(url, Headers(Header.Authorization.Bearer("upstream-token")))
    tools  <- client.listTools
  yield tools
.provide(Client.default)
```

## Dynamic tool & resource sources

Static `.tool(...)` / `.resource(...)` capture a fixed set at construction. A **source** is instead consulted on every request, with the request's `McpToolContext` (the authenticated `ctx.principal` and any mount `ctx.pathParams`) in hand — so it can serve a different, access-scoped set per caller and per mount. This is what lets one server front tools/resources that live elsewhere and change over time (e.g. proxying upstream MCP servers).

```scala
val toolSource = new McpToolSource[Any]:
  def listTools(ctx: McpToolContext): ZIO[Any, Nothing, Chunk[ToolDefinition]] =
    ZIO.succeed(Chunk(ToolDefinition(
      name = ToolName("echo"),
      inputSchema = Json.Obj(Chunk("type" -> Json.Str("object"))),
      // Arbitrary `_meta` is carried verbatim (e.g. MCP Apps `_meta.ui.*`).
      meta = Some(Json.Obj(Chunk("ui" -> Json.Obj(Chunk("resourceUri" -> Json.Str("ui://echo")))))),
    )))
  def callTool(name: ToolName, args: Option[Json.Obj], ctx: McpToolContext): ZIO[Any, Nothing, CallToolResult] =
    ZIO.succeed(CallToolResult(content = Chunk(ToolContent.text(s"called ${name.value}"))))

val server = McpServer("proxy", "1.0.0").toolSource(toolSource)
```

Dispatch semantics with a source registered:

- `tools/list` returns the static tools (scope-filtered as usual) **++** `toolSource.listTools(ctx)`; the source's result is assumed already access-scoped.
- `tools/call` tries the static `.tool(...)` first, then falls through to `toolSource.callTool(...)` (an unknown/forbidden name returns an `isError` result rather than failing the channel).
- `resources/list`, `resources/templates/list`, and `resources/read` combine static and `resourceSource` (read tries static/template matches first, then the source).
- `completion/complete` delegates to `resourceSource.complete(...)`.

`McpToolSource` / `McpResourceSource` are contravariant in their environment, like `.tool(...)`; registering one widens the server's `R`. `McpToolSource.empty` / `McpResourceSource.empty` are no-op defaults.

### Parameterised mounts

`mountedAtParam(name)` serves `/<value>` for any single path segment, exposing the captured value to sources as `ctx.pathParams(name)`. One server can then back many logical mounts (e.g. one per tenant/slug), switching behavior on the captured segment:

```scala
val server = McpServer("multi", "1.0.0")
  .toolSource(toolSource)        // reads ctx.pathParams("slug")
  .resourceSource(resourceSource)
  .mountedAtParam("slug")        // serves /<slug> for any slug
```

When auth is configured, a parameterised mount derives its resource URI / audience from the host root (not the per-`<value>` path), keeping the audience host-wide. `mountedAt` and `mountedAtParam` are mutually exclusive — the last one called wins.
