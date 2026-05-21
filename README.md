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

## Authorization

Authorization is opt-in. A server with no `.auth(...)` call behaves exactly as the examples above — no new headers, no new endpoints, no `R` requirement changes. Add `.auth(...)` to enable OAuth 2.1 bearer-token validation conforming to the [MCP authorization spec](https://modelcontextprotocol.io/specification/2025-11-25/basic/authorization) (compatible with both `2025-06-18` and `2025-11-25`).

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

For DCR-capable clients, the AS must also support [RFC 7591 Dynamic Client Registration](https://datatracker.ietf.org/doc/html/rfc7591). Verified to work against `https://login.jamesward.dev` (Spring Authorization Server with open DCR).

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

