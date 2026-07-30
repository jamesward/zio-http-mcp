# Auth Spec

Design for adding **opt-in** OAuth 2.1 authorization to `zio-http-mcp`, conforming to the MCP authorization specification (resource-server requirements common to both **2025-06-18** and **2025-11-25**), and supporting Dynamic Client Registration (RFC 7591) on the authorization server side.

References:
- [MCP authorization spec, 2025-06-18](https://modelcontextprotocol.io/specification/2025-06-18/basic/authorization)
- [MCP authorization spec, 2025-11-25](https://modelcontextprotocol.io/specification/2025-11-25/basic/authorization)
- [MCP security tutorial — Understanding Authorization in MCP](https://modelcontextprotocol.io/docs/tutorials/security/authorization)
- [OAuth 2.1 (draft)](https://datatracker.ietf.org/doc/html/draft-ietf-oauth-v2-1-13)
- [RFC 9728 — OAuth 2.0 Protected Resource Metadata](https://datatracker.ietf.org/doc/html/rfc9728)
- [RFC 8414 — Authorization Server Metadata](https://datatracker.ietf.org/doc/html/rfc8414)
- [RFC 7591 — Dynamic Client Registration](https://datatracker.ietf.org/doc/html/rfc7591)
- [draft-ietf-oauth-client-id-metadata-document-00 — Client ID Metadata Documents (CIMD)](https://datatracker.ietf.org/doc/html/draft-ietf-oauth-client-id-metadata-document-00)
- [RFC 8707 — Resource Indicators](https://www.rfc-editor.org/rfc/rfc8707)
- [RFC 7662 — Token Introspection](https://www.rfc-editor.org/rfc/rfc7662)
- [RFC 9068 — JWT Profile for OAuth 2.0 Access Tokens](https://www.rfc-editor.org/rfc/rfc9068)

---

## Goals

1. **Opt-in.** A server with no auth config behaves exactly as it does today (current `routes` / `statelessRoutes` semantics). No new `R` requirements, no new headers, no new endpoints.
2. **Conform to the MCP 2025-06-18 spec** for HTTP transports:
   - Act as an OAuth 2.1 **Resource Server**.
   - Publish [RFC 9728 Protected Resource Metadata](https://datatracker.ietf.org/doc/html/rfc9728) at `/.well-known/oauth-protected-resource`.
   - Return `401 Unauthorized` with a `WWW-Authenticate: Bearer realm=…, resource_metadata=…` header on missing/invalid tokens.
   - Validate access tokens, including audience binding per [RFC 8707](https://www.rfc-editor.org/rfc/rfc8707).
   - Reject tokens not issued for this server (no token passthrough).
3. **Support DCR-based clients.** Clients connecting to a `zio-http-mcp` server can dynamically register with the configured authorization server (RFC 7591). The library does not host the AS; it points at one that supports DCR (e.g. Keycloak).
4. **Pluggable token verification.** The library ships built-in verifiers for the two common modes (introspection, JWT/JWKS) and lets users supply their own.
5. **Per-tool scope enforcement.** Tools can declare required scopes; the library returns `403 Forbidden` for tokens that lack them.
6. **Identity in handlers.** Tool handlers can read the authenticated principal (subject, scopes, claims) from the ZIO environment.
7. **Stay congruent with the rest of the library:** opaque types where they help, ZIO-typed errors, builder DSL on `McpServer`, no surprise `R` requirements unless `.auth(...)` is called.

## Non-Goals

- **Hosting the authorization server.** Per the MCP spec, AS implementation details are out of scope. We point at one. This means **DCR is implemented by the AS, not by `zio-http-mcp`**. Our job is to advertise the AS via PRM so MCP clients can discover the `registration_endpoint` and use it.
- **STDIO transport auth.** Per spec, STDIO transports do not follow this flow.
- **Client-side OAuth flows.** This spec covers the server. An `McpClient` auth story is a separate effort.
- **A built-in AS proxy with DCR.** Mentioned as future work; not in v1.

---

## Library Role

```
┌──────────────┐        ┌────────────────────┐        ┌──────────────────────┐
│ MCP Client   │───────▶│ MCP Resource Server│───────▶│ Authorization Server │
│              │  401   │ (zio-http-mcp)     │        │ (Keycloak, Auth0,…)  │
│              │◀───────│ + WWW-Authenticate │        │                      │
│              │        │ + PRM doc          │        │ - /authorize         │
│              │  PRM   │ + token validation │        │ - /token             │
│              │───────▶│                    │        │ - /register (DCR)    │
│              │        │                    │        │ - JWKS / introspect  │
└──────┬───────┘        └────────────────────┘        └──────────┬───────────┘
       │                                                          │
       │ AS metadata discovery, DCR, /authorize, /token (PKCE)    │
       └──────────────────────────────────────────────────────────┘
       │ Bearer token on every /mcp request                       │
       ▼
   POST /mcp  Authorization: Bearer <jwt>
```

The library is the box in the middle.

---

## DSL

Auth is wired in via a single new builder method on `McpServer`:

```scala
def auth[R1](config: McpAuth[R1]): McpServer[R & R1]
```

When `.auth(...)` is set:
- `routes` and `statelessRoutes` mount an extra unauthenticated route for the PRM document.
- All `/mcp` endpoints require a valid bearer token.
- Tool handlers gain `Principal` in their environment.

When `.auth(...)` is **not** set:
- Everything works exactly as it does today.

### `McpAuth`

```scala
package com.jamesward.ziohttp.mcp.auth

import zio.*
import zio.http.URL

final case class McpAuth[-R](
  authorizationServers: NonEmptyChunk[AuthorizationServer],
  verifier: TokenVerifier[R],
  resourceUri: Option[ResourceUri] = None,             // None = derive per-request from Forwarded/Host headers
  scopesSupported: Chunk[OauthScope] = Chunk.empty,
  resourceName: Option[String] = None,                 // for the PRM `resource_name` field
  resourceDocumentation: Option[String] = None,
  requiredScopes: Set[OauthScope] = Set.empty,         // server-wide minimum scopes
  resourcePath: String = "/mcp",                       // path appended when deriving from headers
  realm: String = "mcp",
):
  def withResourceUri(uri: ResourceUri): McpAuth[R] = copy(resourceUri = Some(uri))
  def withRequiredScopes(scopes: OauthScope*): McpAuth[R] =
    copy(requiredScopes = scopes.toSet)
  def withRealm(r: String): McpAuth[R] = copy(realm = r)

object McpAuth:
  /** Convenience constructor for the common case of a single authorization server. */
  def apply[R](
    authorizationServer: AuthorizationServer,
    verifier: TokenVerifier[R],
  ): McpAuth[R] =
    McpAuth(NonEmptyChunk(authorizationServer), verifier)
```

### Opaque types

Following the existing style in `domain.scala`:

```scala
opaque type OauthScope = String
object OauthScope:
  def apply(s: String): OauthScope = s
  extension (s: OauthScope) def value: String = s
  given CanEqual[OauthScope, OauthScope] = CanEqual.derived
  given JsonEncoder[OauthScope] = JsonEncoder.string
  given JsonDecoder[OauthScope] = JsonDecoder.string

/** Canonical resource URI per RFC 8707 §2: lowercase scheme/host, no fragment, no trailing slash. */
opaque type ResourceUri = String
object ResourceUri:
  /** Parse-don't-validate: returns Left if the URI has a fragment or is otherwise non-canonical. */
  def parse(s: String): Either[String, ResourceUri] = …
  def unsafe(s: String): ResourceUri = s   // for test/internal use
  extension (r: ResourceUri) def value: String = r
  given CanEqual[ResourceUri, ResourceUri] = CanEqual.derived
  given JsonEncoder[ResourceUri] = JsonEncoder.string

opaque type AuthorizationServer = String  // issuer URL
object AuthorizationServer:
  def apply(issuer: String): AuthorizationServer = issuer
  extension (a: AuthorizationServer) def issuer: String = a
  given CanEqual[AuthorizationServer, AuthorizationServer] = CanEqual.derived
  given JsonEncoder[AuthorizationServer] = JsonEncoder.string
```

### `Principal`

The result of successful token verification, available to handlers as a ZIO service.

```scala
final case class Principal(
  subject: Option[String],          // sub claim
  clientId: Option[String],         // client_id / azp
  scopes: Set[OauthScope],
  audience: Set[String],            // aud claim(s)
  issuer: Option[String],           // iss claim
  expiresAt: Option[java.time.Instant],
  raw: String,                      // the original bearer token
  claims: zio.json.ast.Json.Obj,    // full claim set (or a normalized projection)
)
```

Handlers access it via the environment:

```scala
val whoami = McpTool("whoami")
  .handle[Principal, Nothing, Unit, String]: _ =>
    ZIO.serviceWith[Principal](_.subject.getOrElse("anonymous"))
```

The library injects `Principal` into the request scope for every authenticated request. With `.auth(...)`, `server.routes` becomes `Routes[R & McpServer.State & Principal, Response]` — wait, that's wrong: `Principal` is per-request, not a singleton service. So we use `ZIO.serviceWithZIO` against a *request-scoped* layer. Concretely the auth middleware does:

```scala
ZIO.scoped {
  ZIO.serviceWithZIO[TokenVerifier[R]] { v =>
    v.verify(rawToken).flatMap { principal =>
      handlerEffect.provideSomeEnvironment[R](_.add(principal))
    }
  }
}
```

So the `Principal` requirement does not appear in the `routes` type signature — it's added inside the route handler before calling user code, the same way `McpToolContext.make(...)` is used today inside `handleToolsCall`. This keeps the public type signature stable:

```scala
def routes: Routes[R & McpServer.State, Response]   // unchanged with or without auth
```

The user *opts in* to seeing the principal by writing handlers that ask for it.

### `TokenVerifier`

```scala
package com.jamesward.ziohttp.mcp.auth

trait TokenVerifier[-R]:
  def verify(rawToken: String): ZIO[R, AuthError, Principal]

enum AuthError:
  case Missing
  case Invalid(reason: String)
  case Expired
  case AudienceMismatch(expected: ResourceUri, actual: Set[String])
  case IssuerMismatch(expected: String, actual: Option[String])
  case InsufficientScope(required: Set[OauthScope], actual: Set[OauthScope])
  case UpstreamFailure(reason: String)
```

`AuthError` maps to HTTP responses inside the middleware:

| `AuthError`                  | HTTP status | `WWW-Authenticate` `error=` |
|------------------------------|-------------|------------------------------|
| `Missing`                    | 401         | (none — initial challenge)   |
| `Invalid` / `Expired`        | 401         | `invalid_token`              |
| `AudienceMismatch`           | 401         | `invalid_token`              |
| `IssuerMismatch`             | 401         | `invalid_token`              |
| `InsufficientScope`          | 403         | `insufficient_scope`         |
| `UpstreamFailure`            | 503         | (none — server-side issue)   |

All `4xx` responses include `WWW-Authenticate` per RFC 9728 §5.1, and per the 2025-11-25 spec they include a `scope` parameter listing the scopes the client should request:

```
WWW-Authenticate: Bearer realm="mcp",
  resource_metadata="https://mcp.example.com/.well-known/oauth-protected-resource",
  scope="mcp:tools",
  error="invalid_token",
  error_description="Token expired"
```

### Built-in verifiers

```scala
object TokenVerifier:

  /** RFC 7662 introspection. Suitable when the AS supports introspection (e.g. Keycloak).
   *  Validates `active`, `iss`, `exp`. Audience binding is enforced by the auth middleware. */
  def introspection(
    endpoint: URL,
    clientId: String,
    clientSecret: zio.Config.Secret,
    expectedIssuer: String,
  ): TokenVerifier[Client]

  /** RFC 7519 / RFC 9068 JWT validation against a JWKS endpoint.
   *  Validates signature, `iss`, `exp`, `nbf`. Audience is checked by the middleware. */
  def jwks(
    jwksUri: URL,
    expectedIssuer: String,
    clockSkew: Duration = 60.seconds,
  ): TokenVerifier[Client]

  /** Discovers the JWKS URI from RFC 8414 metadata at <issuer>/.well-known/oauth-authorization-server
   *  (with OIDC fallback to /.well-known/openid-configuration). Caches keys with TTL.
   *  Validates signature, `iss`, `exp`, `nbf` only — audience is checked by the middleware. */
  def discoverJwks(
    issuer: String,
    clockSkew: Duration = 60.seconds,
  ): TokenVerifier[Client]

  /** For tests and custom flows. */
  def fromFunction[R](f: String => ZIO[R, AuthError, Principal]): TokenVerifier[R] =
    new TokenVerifier[R] { def verify(t: String) = f(t) }
```

`jwks` and `introspection` both depend on a `zio.http.Client` for HTTP calls. They internally cache JWKS / discovery documents with a configurable TTL.

For the v1 implementation we will delegate JWT parsing/signing to a small, well-tested Java library to avoid reimplementing crypto. Candidates: `com.auth0:java-jwt` + `com.auth0:jwks-rsa-java`, or `org.bitbucket.b_c:jose4j`. The choice is left as an implementation detail; the public API does not expose it.

### Per-tool scopes

Tools can declare additional required scopes. Server-wide `requiredScopes` apply to every tool; per-tool scopes are additive.

```scala
val deleteUser = McpTool("delete_user")
  .description("Delete a user")
  .requireScopes(OauthScope("admin"))
  .handle[Any, ToolError, DeleteInput, String]: input => …
```

When auth is disabled and a tool declares scopes, **the scopes are silently ignored**. This keeps the auth surface fully opt-in and avoids forcing every tool author to know about auth.

### Resources and prompts

Resources and prompts are likewise gated by the server-wide bearer-token middleware. They share the server-wide `requiredScopes`. Per-resource and per-prompt scope declarations may be added in a future iteration; for v1 the simple model is fine.

---

## Routes added when auth is enabled

### Protected Resource Metadata endpoints

Public, unauthenticated `GET`. Returns the PRM document at **both** of these paths (per the 2025-11-25 spec, which allows either form):

- `/.well-known/oauth-protected-resource` — root-relative form, used when the host has a single MCP resource.
- `/.well-known/oauth-protected-resource/mcp` — path-suffixed form, used when the host serves multiple resources at different paths. The suffix mirrors the MCP endpoint's path.

Both return identical JSON:

```json
{
  "resource": "https://mcp.example.com/mcp",
  "authorization_servers": ["https://auth.example.com/realms/master"],
  "scopes_supported": ["mcp:tools"],
  "bearer_methods_supported": ["header"],
  "resource_name": "Example MCP Server",
  "resource_documentation": "https://example.com/docs"
}
```

Response includes `Cache-Control: max-age=3600` so well-behaved clients don't re-fetch on every request. Mounted only when `.auth(...)` is configured.

### Bearer-token middleware on `/mcp`

Wraps the existing POST/GET/DELETE handlers. Pseudocode:

```scala
def authenticate(req: Request): ZIO[R, Response, Principal] =
  for
    raw <- extractBearerToken(req).orElseFail(unauthorized(AuthError.Missing))
    p   <- verifier.verify(raw).mapError(authErrorToResponse)
    _   <- enforceScopes(p, requiredScopesForRoute).mapError(authErrorToResponse)
  yield p
```

`extractBearerToken` parses `Authorization: Bearer <token>`, rejecting any token in the URL query (per spec §5.1).

The middleware sits *outside* `Origin` validation, since `Origin` is for browser-based DNS-rebinding protection and is independent of the auth check.

---

## Wire-level behaviors

### 401 challenge

```
HTTP/1.1 401 Unauthorized
WWW-Authenticate: Bearer realm="mcp",
  resource_metadata="https://mcp.example.com/.well-known/oauth-protected-resource",
  scope="mcp:tools"
Content-Type: application/json

{"jsonrpc":"2.0","error":{"code":-32001,"message":"Unauthorized"},"id":null}
```

The body is a JSON-RPC error so MCP clients that use a JSON-RPC parser do not crash on the response. The `scope` parameter is the 2025-11-25 hint that tells the client which scopes to request — the library populates it from `requiredScopes` (or the union of server-wide + per-tool when the failing route is identifiable).

We will use code `-32001` (a server-defined error in the reserved range) for "Unauthorized" and `-32003` for "Forbidden". These are added to `ErrorCode` in `domain.scala`.

### 403 insufficient scope (step-up challenge)

```
HTTP/1.1 403 Forbidden
WWW-Authenticate: Bearer realm="mcp",
  resource_metadata="https://mcp.example.com/.well-known/oauth-protected-resource",
  error="insufficient_scope",
  scope="mcp:tools admin",
  error_description="Tool 'delete_user' requires scope 'admin'"
```

The `scope` parameter lists exactly the scopes the client should request to satisfy the failed call (server-wide + per-tool). This drives the 2025-11-25 step-up authorization flow on compliant clients.

### Audience validation

Per RFC 8707 + RFC 9068:
- Token MUST contain an `aud` claim.
- At least one entry of `aud` MUST equal `resourceUri.value` (with the canonical-form normalization per the MCP spec — case-insensitive scheme/host, optional trailing slash tolerated).
- If `aud` is missing or no entry matches, return 401 with `error="invalid_token"` and an explanatory `error_description`.

Audience binding is enforced inside the auth middleware (not the verifier) so that [[McpAuth.resourceUri]] can be derived from request headers when not explicitly set. The middleware checks `principal.audience.exists(resolvedResourceUri.matchesAudience)` after the verifier returns; on mismatch it emits `AuthError.AudienceMismatch` and the standard 401 response.

---

## Dynamic Client Registration

Per the MCP spec, **DCR is the AS's responsibility**. The library's role is:

1. **Advertise** the AS via the `authorization_servers` field of the PRM document.
2. **Trust** the AS's RFC 8414 metadata, which contains the `registration_endpoint`. Clients then post their registration to that endpoint themselves.

Therefore "DCR support" in `zio-http-mcp` means:
- The PRM document correctly lists DCR-capable AS issuers.
- The token verifier accepts tokens issued under any client (including ones registered dynamically).
- We provide an example and an integration test showing the full DCR + token + tool-call flow against Keycloak.

### Future: AS proxy with DCR

A future version may ship `com.jamesward.ziohttp.mcp.auth.proxy` with a thin AS that:
- Implements DCR (RFC 7591) and AS metadata (RFC 8414) endpoints.
- Delegates `/authorize` and `/token` to an upstream IdP (e.g. an OIDC provider).
- Issues its own audience-bound JWTs.

This is **out of scope for v1**. The recommended path is to point at a real AS (Keycloak, Auth0, Okta, Authentik, etc.).

---

## Example — minimal server with JWT validation against `login.jamesward.dev`

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
        server = McpServer("example-server", "1.0.0")
          .auth(McpAuth(
            authorizationServers = NonEmptyChunk(AuthorizationServer("https://login.jamesward.dev")),
            scopesSupported      = Chunk(OauthScope("mcp:tools")),
            resourceName         = Some("Example MCP Server"),
            verifier             = verifier,
            requiredScopes       = Set(OauthScope("mcp:tools")),
            // resourceUri = None ⇒ derive from Forwarded / X-Forwarded-* / Host headers
          ))
          .tool(McpTool("greet").handle: (i: NameInput) => ZIO.succeed(s"Hello, ${i.name}"))
        _ <- Server.serve(server.statelessRoutes)
      yield ()
    program.provide(Server.default, Client.default)
```

`discoverJwks` fetches `https://login.jamesward.dev/.well-known/oauth-authorization-server` once on first verification, caches the `jwks_uri` and the JWKS itself, and validates JWTs locally without contacting the AS on every request. Audience binding is enforced by the auth middleware against the resolved-per-request `resourceUri`, so the verifier itself is configuration-light.

For deployments that prefer introspection (RFC 7662) — typically because the AS issues opaque tokens, or because the operator wants real-time revocation — swap in:

```scala
verifier = TokenVerifier.introspection(
  endpoint         = URL.decode("https://login.jamesward.dev/oauth2/introspect").toOption.get,
  clientId         = sys.env("OAUTH_CLIENT_ID"),
  clientSecret     = Config.Secret(sys.env("OAUTH_CLIENT_SECRET")),
  expectedIssuer   = "https://login.jamesward.dev",
)
```

The library is AS-agnostic: any AS that publishes RFC 8414 metadata, supports RFC 7591 DCR (or has clients pre-registered), and either signs JWTs with a published JWKS or exposes RFC 7662 introspection works.

---

## Implementation plan

### New files

- `src/main/scala/com/jamesward/ziohttp/mcp/auth/McpAuth.scala` — `McpAuth`, `OauthScope`, `ResourceUri`, `AuthorizationServer`.
- `src/main/scala/com/jamesward/ziohttp/mcp/auth/Principal.scala` — `Principal`, `AuthError`.
- `src/main/scala/com/jamesward/ziohttp/mcp/auth/TokenVerifier.scala` — trait + introspection + JWKS implementations.
- `src/main/scala/com/jamesward/ziohttp/mcp/auth/ProtectedResourceMetadata.scala` — RFC 9728 doc model + JSON codec.
- `src/main/scala/com/jamesward/ziohttp/mcp/auth/ResourceUriResolver.scala` — derives the resource URI from request headers when `McpAuth.resourceUri` is unset.
- `src/main/scala/com/jamesward/ziohttp/mcp/auth/AuthMiddleware.scala` — `WWW-Authenticate` building, audience binding, route wrapping, scope enforcement.

### Modified files

- `McpServer.scala`:
  - Add `private val auth: Option[McpAuth[R]]` field, `def auth[R1](a: McpAuth[R1]): McpServer[R & R1]` builder.
  - In `routes` / `statelessRoutes`: prepend the PRM route (when `auth.isDefined`, dynamic per-request body), and wrap the `/mcp` handlers in the auth middleware.
- `McpTool.scala`:
  - Add `def requireScopes(scopes: OauthScope*): McpTool[R, E, In, Out]` builder. Stores scopes on the handler so `dispatchMethod` can check them per call.
- `domain.scala`:
  - Extend `ErrorCode` with `Unauthorized = -32001` and `Forbidden = -32003`.

### Backwards compatibility

- No existing API changes. Servers without `.auth(...)` are byte-for-byte compatible with the current behavior.
- New code lives under `com.jamesward.ziohttp.mcp.auth` so the import surface for non-auth users is unaffected.

### New dependencies

- `com.guizmaii %% scala-nimbus-jose-jwt-zio % 4.1.2` — ZIO-native wrapper around `nimbus-jose-jwt`. Provides `ZioJwtValidator` with eager fail-fast JWKS fetch on startup, lock-free `AtomicReference` JWKS cache, background refresh fiber (default every 4 minutes), built-in ZIO Metrics + OpenTelemetry tracing, and graceful degradation with stale cache on refresh failures. The validator's `validate` method is non-blocking after construction (signature verification is pure CPU work). Pulls in `nimbus-jose-jwt` transitively as the cryptographic engine.
- AS metadata discovery (`/.well-known/oauth-authorization-server` with OIDC fallback) is implemented in-house since the lib only handles JWKS fetching given a known `jwks_uri`.

Test-only:
- None. `LiveAuthSpec` talks to a hosted AS over plain HTTP. (`ConformanceSpec` shells out to the conformance kit via `npx`, so it needs Node on the host but no extra JVM dependency; an optional `KeycloakAuthSpec` would need `org.testcontainers:testcontainers` added back.)

---

## Test Plan

The plan below validates each behavior the library is responsible for. Tests live alongside existing specs in `src/test/scala/com/jamesward/ziohttp/mcp/`.

### 1. Unit tests — `AuthSpec`

Use `TokenVerifier.fromFunction` to inject canned `Principal` / `AuthError` results — no live AS, no JWT cryptography. Bind the server with `Server.install(server.routes)` as the existing specs do.

| # | Test                                                                                   | Assertion |
|---|----------------------------------------------------------------------------------------|-----------|
| 1 | Without `.auth(...)`, `/.well-known/oauth-protected-resource` returns 404               | Backwards compatibility |
| 2 | With `.auth(...)`, GET `/.well-known/oauth-protected-resource` returns 200 + PRM JSON   | `resource`, `authorization_servers`, `scopes_supported` all present and correct |
| 2b| With `.auth(...)`, GET `/.well-known/oauth-protected-resource/mcp` returns the same PRM JSON | Path-suffixed form per 2025-11-25 |
| 2c| Both PRM responses include `Cache-Control: max-age=3600`                                  | |
| 3 | POST `/mcp` without `Authorization` header → 401 + correct `WWW-Authenticate`           | Header includes `realm`, `resource_metadata` URL, **`scope`** parameter |
| 4 | POST `/mcp` with malformed `Authorization` → 401, `error="invalid_token"`                | |
| 5 | POST `/mcp` with token verifier returning `Invalid(...)` → 401, `error="invalid_token"`  | Body is valid JSON-RPC error `-32001` |
| 6 | POST `/mcp` with token verifier returning `AudienceMismatch` → 401, `error="invalid_token"` | `error_description` mentions audience |
| 7 | POST `/mcp` with valid token but missing required scope → 403, `error="insufficient_scope"`  | `scope="<required scopes>"` and `resource_metadata="…"` both present (step-up challenge) |
| 8 | POST `/mcp` with valid token + required scopes → tool call succeeds                      | Result body matches expected `CallToolResult` |
| 9 | Tool handler can read `Principal` from the env                                           | `whoami` tool returns the `sub` claim |
|10 | Per-tool scope: tool with extra `requireScopes` is denied with token lacking scope, but other tools succeed | 403 only on the gated tool |
|11 | `GET /mcp` (SSE) with no token → 401 (stateful only)                                     | |
|12 | `DELETE /mcp` with no token → 401                                                        | |
|13 | Stateless `routes` apply the same middleware                                             | All of #3–#9 also pass against `statelessRoutes` |
|14 | Token in URL query (`?access_token=…`) is rejected even if otherwise valid               | 401, per OAuth 2.1 §5 |
|15 | When auth is disabled, tools that called `.requireScopes(...)` still execute             | Scopes silently ignored |

### 2. PRM document compliance — `ProtectedResourceMetadataSpec`

Pure JSON tests on the PRM document model.

| # | Test                                                                            |
|---|---------------------------------------------------------------------------------|
| 1 | PRM JSON has `resource`, `authorization_servers`, `bearer_methods_supported`     |
| 2 | `scopes_supported` is omitted when the user doesn't set scopes                   |
| 3 | `resource_name` and `resource_documentation` round-trip cleanly                  |
| 4 | URL with trailing slash is normalized to canonical form (no trailing slash)       |
| 5 | URL with fragment is rejected by `ResourceUri.parse`                             |

### 3. Verifier unit tests — `TokenVerifierSpec`

Mock the AS HTTP responses with `zio.http.TestClient` (or a stub `Client` layer).

| # | Test                                                                            |
|---|---------------------------------------------------------------------------------|
| 1 | `introspection`: `active=true` with matching `aud` and `iss` → `Principal`       |
| 2 | `introspection`: `active=false` → `AuthError.Invalid`                            |
| 3 | `introspection`: missing `aud` → `AuthError.AudienceMismatch`                    |
| 4 | `introspection`: wrong `iss` → `AuthError.IssuerMismatch`                        |
| 5 | `introspection`: HTTP 5xx from AS → `AuthError.UpstreamFailure`                  |
| 6 | `introspection`: respects timeouts (does not hang the request)                    |
| 7 | `jwks`: signed-with-known-key + correct `aud`/`iss`/`exp` → `Principal`          |
| 8 | `jwks`: signed with unknown key → `AuthError.Invalid`                            |
| 9 | `jwks`: expired token → `AuthError.Expired`                                      |
|10 | `jwks`: wrong audience → `AuthError.AudienceMismatch`                            |
|11 | `jwks`: JWKS document is cached (second verify makes 0 HTTP calls within TTL)    |

### 4. Integration test against `login.jamesward.dev` — `LiveAuthSpec`

This is the **end-to-end DCR validation** against a real, hosted Spring Authorization Server at `https://login.jamesward.dev`. The advantage over a self-hosted testcontainers Keycloak is no Docker dependency, faster setup, and validation against an independently-maintained AS. The tradeoff is the test requires network access and a live external service, so it is gated behind a `LiveAuth` test tag and excluded from default CI runs unless credentials are present.

#### What the AS supports (verified from its metadata)

`GET https://login.jamesward.dev/.well-known/oauth-authorization-server` returns:

| Field                                | Value                                                   |
|--------------------------------------|---------------------------------------------------------|
| `issuer`                             | `https://login.jamesward.dev`                           |
| `authorization_endpoint`             | `https://login.jamesward.dev/oauth2/authorize`          |
| `token_endpoint`                     | `https://login.jamesward.dev/oauth2/token`              |
| `jwks_uri`                           | `https://login.jamesward.dev/oauth2/jwks`               |
| `introspection_endpoint`             | `https://login.jamesward.dev/oauth2/introspect`         |
| `registration_endpoint`              | `https://login.jamesward.dev/oauth2/register`           |
| `grant_types_supported`              | `authorization_code`, `client_credentials`, `refresh_token`, `urn:ietf:params:oauth:grant-type:token-exchange` |
| `code_challenge_methods_supported`   | `S256` (PKCE)                                           |
| `response_types_supported`           | `code`                                                  |

This is everything we need: DCR (RFC 7591), JWKS (for JWT validation), introspection (RFC 7662), PKCE (S256), and `client_credentials` grant — which lets us obtain a token in tests without a browser.

**Open DCR.** The `/oauth2/register` endpoint accepts unauthenticated registration requests. A `POST` with `client_name`, `grant_types: ["client_credentials"]`, `scope: "mcp:tools"`, and `token_endpoint_auth_method: "client_secret_basic"` returns 201 with a freshly minted `client_id` and `client_secret`. No bootstrap credential is required.

**RFC 8707 resource binding.** The token endpoint honors the `resource` parameter and writes its value into the JWT `aud` claim. A successful `client_credentials` exchange against `/oauth2/token` with `resource=<MCP server URL>` and `scope=mcp:tools` yields a JWT with the claims:

```json
{
  "sub": "<dynamically-registered-client-id>",
  "aud": "<MCP server URL>",
  "iss": "https://login.jamesward.dev",
  "scope": ["mcp:tools"],
  "exp": <…>, "iat": <…>, "nbf": <…>, "jti": "<…>"
}
```

This means audience-bound tokens come for free — the AS does the right thing — and the `discoverJwks` verifier can validate everything locally without any AS-side credential on the resource-server side.

#### Setup

```scala
val issuer            = "https://login.jamesward.dev"
val resourceUri       = ResourceUri.parse(s"http://host.testcontainers.internal:$port/mcp").toOption.get
val server = McpServer("test-server", "0.1.0")
  .auth(McpAuth(
    resourceUri          = Some(resourceUri),
    authorizationServers = NonEmptyChunk(AuthorizationServer(issuer)),
    scopesSupported      = Chunk(OauthScope("mcp:tools")),
    verifier             = TokenVerifier.discoverJwks(issuer = issuer),
    requiredScopes       = Set(OauthScope("mcp:tools")),
  ))
  .tool(addTool)
```

We use `discoverJwks` (JWT validation against the published JWKS) rather than introspection, because JWT validation does not require any AS-side credential and is purely public-key based. The introspection path is exercised separately in `TokenVerifierSpec` with a mocked `Client`.

#### Tests

| # | Scenario                                                                                          | Asserts |
|---|---------------------------------------------------------------------------------------------------|---------|
| 1 | Unauthenticated `POST /mcp` → 401 with `WWW-Authenticate` pointing at the PRM URL                  | `resource_metadata` URL resolves and lists `https://login.jamesward.dev` |
| 2 | Fetch RFC 8414 metadata from the issuer in PRM → contains `registration_endpoint`                  | Confirms the AS we point at supports DCR |
| 3 | **DCR**: `POST` to `registration_endpoint` with no auth header and a body containing `client_name`, `grant_types: ["client_credentials"]`, `scope: "mcp:tools"`, `token_endpoint_auth_method: "client_secret_basic"` → 201 with `client_id` + `client_secret` | Dynamic registration succeeds against open DCR |
| 4 | Exchange the new credentials at `token_endpoint` (grant_type=client_credentials, `resource=<MCP server URL>`, `scope=mcp:tools`) → JWT access token | Decoded JWT has `iss=https://login.jamesward.dev`, `aud=<MCP server URL>`, `scope=mcp:tools` |
| 5 | `POST /mcp` `initialize` + `tools/list` with that token → 200 + test tool list                     | Principal is populated; subject reflects the dynamically registered client |
| 6 | `POST /mcp` `tools/call` for `add` with that token → 200 + correct result                          | |
| 7 | Manually mint a token with the wrong `aud` (e.g. obtain one with a different `resource` parameter) → 401 `invalid_token` | Confirms audience binding |
| 8 | Token without `mcp:tools` scope (request without `scope`) → 403 `insufficient_scope`               | |
| 9 | Tampered JWT (flip a byte in the signature) → 401 `invalid_token`                                  | JWKS-based verifier rejects |
|10 | Verifier caches JWKS — second authenticated call within TTL makes no `jwks_uri` HTTP call          | Use a stub `Client` wrapping the real one for call counting |

Test gating:

```scala
override def spec =
  suite("LiveAuthSpec")(...)
    @@ withLiveClient    // requires network access to login.jamesward.dev
    @@ tag("live-auth")
```

The test runs in CI by default — open DCR removes the credential bootstrapping problem. If the AS is unreachable (offline development, network policy), the suite is excluded via the `live-auth` tag and the offline tests (`AuthSpec`, `TokenVerifierSpec`, `ProtectedResourceMetadataSpec`) still cover all the resource-server logic with stubs.

#### Fallback: testcontainers Keycloak

For full offline reproducibility, an alternative `KeycloakAuthSpec` running Keycloak via testcontainers is a *future* addition. The library implementation is AS-agnostic (any RFC 8414 + RFC 9728 + RFC 7591 + RFC 7662 / JWKS-issuing AS works), so the same test code generalizes. If `LiveAuthSpec` proves flaky in CI we add the Keycloak variant; otherwise the live test is sufficient.

#### Why this validates "auth with DCR"

The MCP spec assigns DCR to the AS, but what users actually want validated end-to-end is:
1. A client that does not know the MCP server in advance can discover everything via the PRM document. ✓ (test #1, #2)
2. That client can register itself on the AS without the server operator pre-creating it. ✓ (test #3)
3. The token it obtains is accepted by the MCP server, with proper audience and scope checks. ✓ (test #4–#6)
4. Tokens that do not meet the spec's requirements are rejected. ✓ (test #7–#9)
5. The library's caching behavior holds under realistic conditions. ✓ (test #10)

The full PKCE + browser-based authorization-code flow is not exercised by automated tests (no browser). The `client_credentials` grant covers the same token-issuance and resource-server code paths. The auth-code + PKCE flow is exercised by manual testing with VS Code or MCP Inspector, documented in the README.

That covers every spec requirement that touches the resource server.

### 5. Java MCP SDK interop — `JavaSdkAuthSpec`

Validates that the official Java MCP SDK (`io.modelcontextprotocol.sdk:mcp-core`) can talk to our auth-protected server when given a valid bearer token. Re-uses the DCR + `client_credentials` token helpers from [[AuthTestHelpers]] against `https://login.jamesward.dev`.

The Java SDK's `HttpClientStreamableHttpTransport.Builder.httpRequestCustomizer(...)` hook is used to inject `Authorization: Bearer <token>` on every request. Tests:

| # | Scenario                                                                 |
|---|--------------------------------------------------------------------------|
| 1 | SDK with valid token: `initialize` + `tools/list` succeeds, `add` is in the list |
| 2 | SDK with valid token: `tools/call` for `add` returns the correct result   |
| 3 | SDK without token: `initialize` throws (server returned 401)              |
| 4 | SDK with token bound to a different audience: `initialize` throws (server returned 401) |

Tagged `live-auth` and runs alongside `LiveAuthSpec`.

#### Future: full DCR-aware client via Spring AI / `mcp-security`

The Java MCP SDK alone does not implement the spec's client-side DCR/CIMD flow (it just sends the bearer header you give it). The full client behavior — receive 401, parse `WWW-Authenticate` for the PRM URL, fetch PRM, discover the AS, do DCR, get a token, retry — is implemented in [`org.springaicommunity:mcp-client-security`](https://github.com/spring-ai-community/mcp-security).

Adding a `SpringAiMcpAuthSpec` that exercises this is **deferred from v1** for these reasons:

- mcp-security 0.1.x targets Spring AI 2.0.x and pulls in Spring Boot + Spring Security + Spring AI as test dependencies — a heavy classpath cost for one additional spec.
- The full client flow is a *client-side* responsibility. Our resource-server impl correctness is already validated end-to-end against `login.jamesward.dev` (`LiveAuthSpec`) and against the Java SDK as a token-bearing client (`JavaSdkAuthSpec`). Both those flows exercise every code path on our side that a DCR-aware client would.
- mcp-security itself uses Spring's `OAuth2HttpClientTransportCustomizer` — internally the same `McpSyncHttpClientRequestCustomizer` hook we exercise in `JavaSdkAuthSpec`. So we already cover the integration surface; what we'd be additionally testing is mcp-security's own implementation, not ours.

If we later want this coverage, the test would live in a separate sbt sub-module to isolate the Spring dep tree and run as an opt-in CI job.

### 6. README-driven tests (per `AGENTS.md`)

Every code example added to the README's auth section gets a matching test in `McpToolSpec` (or a new `AuthExamplesSpec`):
- Minimal `.auth(...)` example compiles and serves.
- `whoami` tool example works with a stubbed verifier.
- Per-tool `.requireScopes(...)` example enforces scopes.

---

## Resolved decisions

1. **JWT library: [`com.guizmaii %% scala-nimbus-jose-jwt-zio`](https://github.com/guizmaii-opensource/scala-nimbus-jose-jwt) `4.1.2`.** Scala 3 + ZIO-native wrapper around the [`com.nimbusds:nimbus-jose-jwt`](https://connect2id.com/products/nimbus-jose-jwt) cryptographic engine. Provides:
   - Eager fail-fast JWKS fetch on startup with a configurable retry schedule
   - Lock-free `AtomicReference` JWKS cache (instant non-blocking reads at validation time)
   - Background refresh fiber that keeps the cache warm
   - Health monitoring (`JwksHealth.Healthy` / `Degraded` with last-error)
   - ZIO Metrics + OpenTelemetry tracing built in
   - Graceful degradation: stale cache served if refresh fails
   ```scala
   libraryDependencies += "com.guizmaii" %% "scala-nimbus-jose-jwt-zio" % "4.1.2"
   ```
   Earlier picks: we tried `com.github.jwt-scala:jwt-zio-json` (zio-json conflict, infrequent updates) and then `com.nimbusds:nimbus-jose-jwt` directly (forced us to maintain ~150 lines of JWKS fetch+cache code in-house). The guizmaii lib gives us the cryptographic correctness of nimbus plus a ZIO-idiomatic non-blocking interface and production-grade JWKS lifecycle management. Audience binding is configured to be a no-op at the validator (we pass `null` for `acceptedAudience` to `DefaultJWTClaimsVerifier`) so the auth middleware can do per-request audience validation against the resolved `resourceUri`. The validator's lifetime is tied to a [[zio.Scope]] — `TokenVerifier.discoverJwks` requires `Client & Scope`, with `Scope` typically supplied by `ZIOAppDefault.run`.

2. **PRM path: serve both `/.well-known/oauth-protected-resource` and `/.well-known/oauth-protected-resource/<server-path>`.** The 2025-11-25 spec explicitly endorses the path-suffixed form (e.g. `https://example.com/public/mcp` → `https://example.com/.well-known/oauth-protected-resource/public/mcp`) for hosts with multiple resources. Both URLs serve identical content. Clients are required to support both discovery mechanisms and we want either to work.

3. **PRM caching: `Cache-Control: max-age=3600`.** RFC 9728 metadata changes rarely; an hour is conservative enough to allow operator-driven AS migrations within a reasonable window without hammering the endpoint.

4. **Per-resource and per-prompt scopes: deferred.** v1 ships only server-wide and per-tool scopes. See *Future work* below.

5. **MCP spec version compatibility:** see next section.

## MCP spec version compatibility

The library targets the MCP **2025-06-18** authorization spec for v1, with all the resource-server-side additions from **2025-11-25** adopted from the start. This is achievable because the differences between the two versions are concentrated on the *client* side of OAuth — specifically how the client identifies itself to the AS — not on the resource-server side.

### What changed between 2025-06-18 and 2025-11-25

| Area                         | 2025-06-18                                                | 2025-11-25                                                                       | Affects RS? |
|------------------------------|-----------------------------------------------------------|----------------------------------------------------------------------------------|-------------|
| Client registration approach | DCR is a `SHOULD`                                          | Three approaches: pre-registration, **CIMD** (`SHOULD`), DCR (`MAY`, "for backwards compat") | No — these are all *client→AS* mechanisms. The RS receives a bearer token regardless. |
| AS metadata discovery        | RFC 8414 only                                              | RFC 8414 *or* OIDC Discovery 1.0; multiple well-known URI patterns                | No — the RS doesn't fetch AS metadata; the client does. |
| `WWW-Authenticate` `scope=`  | Not specified                                              | `SHOULD` include in the 401 challenge                                            | **Yes** — adopt from start. |
| Step-up `insufficient_scope` | Generic 403                                                | 403 with `WWW-Authenticate: Bearer error="insufficient_scope", scope="…", resource_metadata="…"` | **Yes** — already in the design. |
| PRM well-known URI form      | One canonical path                                         | Two acceptable forms (root-relative and path-suffixed)                            | **Yes** — adopt from start (decision #2 above). |
| PKCE / code_challenge        | `MUST` use S256                                            | `MUST` use S256, plus AS metadata `code_challenge_methods_supported` discovery     | No — that's an AS metadata field; the RS doesn't serve AS metadata. |

### Defining "what version we support"

The PRM document does not include a "supported MCP spec version" field. Conformance is determined by behavior, not declaration. Concretely:

- The library is **fully compliant with the 2025-06-18 resource-server requirements** (PRM, WWW-Authenticate, audience-bound token validation, 401/403/400 status handling, RFC 8707 binding).
- The library is **also compliant with the 2025-11-25 resource-server requirements** because every additional 2025-11-25 RS requirement is adopted from v1 (scope hint in WWW-Authenticate, dual well-known URI forms, step-up 403 challenge).
- **Client-registration mechanisms are out of scope** for this library because we don't implement an MCP client. CIMD and DCR are both client-side; we just need to point at an AS that supports whichever mechanism the deployed clients use. `login.jamesward.dev` supports DCR today, which exercises the most stringent end-to-end path.

This means the library **does not need a "compat with 2025-06-18 only" caveat** for the resource-server piece. The CIMD/DCR distinction simply doesn't surface in our code path. If we later add an `McpClient` library, that's where CIMD support would live — and that's a separate effort tracked under *Future work*.

The README will state: *"Compatible with MCP authorization specs 2025-06-18 and 2025-11-25 (resource server only)."*

### 2026-07-28 update

The **2026-07-28** authorization spec hardened the *client* side; the resource-server requirements are unchanged, so the server implementation above remains fully conformant. The client-side flow is now implemented in `McpClient` (superseding the "Client-side OAuth flows" non-goal and *Future work* item 2):

- `OAuthAuthorizationCode` runs the full hardened flow: PRM discovery (path-inserted well-known form first) with **PRM `resource` validation against the server URL**, path-aware RFC 8414/OIDC AS-metadata discovery with issuer validation, **PKCE S256**, RFC 8707 `resource` on both authorization and token requests, **RFC 9207 `iss` validation** (exact string compare, required when `authorization_response_iss_parameter_supported` is advertised), the spec's scope-selection strategy, and refresh-token renewal.
- Client identification follows the spec's priority: pre-registration → **CIMD** (`client_id_metadata_document_supported`) → DCR (deprecated fallback, registering with `application_type` per SEP-837).
- Validation: `CimdAuthSpec` (in-process against `TestIdp`), `ClientConformanceSpec` (the official conformance kit's `auth/*` client scenarios), and `LiveCimdAuthSpec` (CIMD interop against the real `login.jamesward.dev`).

Two findings from the live CIMD interop worth recording, since neither is obvious from the specs:

- Authorization servers require the metadata document to be served as `Content-Type: application/json`; `raw.githubusercontent.com`'s `text/plain` is rejected, so the fixtures are read over jsDelivr instead.
- `login.jamesward.dev` rejects an unresolvable or mismatched metadata document with a bare `401` carrying its protected-resource `WWW-Authenticate` challenge, rather than the OAuth2 `invalid_client` JSON error an opaque unknown `client_id` produces. Client code should therefore not rely on a structured `invalid_client` body to detect a rejected CIMD client.

## Future work

Items intentionally deferred from v1, in rough priority order:

1. **Per-resource and per-prompt scope declarations.** Same model as `McpTool.requireScopes(...)`, just on `McpResource` and `McpPrompt`. Wait for a concrete use case before designing the API; v1 server-wide scopes cover the basics.
2. **Client-side OAuth + CIMD support in `McpClient`.** When `McpClient` grows from a sketch to a full implementation, add:
   - PRM document fetching and parsing
   - AS metadata discovery (RFC 8414 + OIDC fallback)
   - Authorization-code + PKCE flow
   - Pre-registration, CIMD (the new 2025-11-25 SHOULD), and DCR (fallback)
   - Token storage with secure refresh
3. **Built-in AS proxy with DCR.** A `com.jamesward.ziohttp.mcp.auth.proxy` package implementing RFC 7591 + RFC 8414 with delegation to an upstream IdP, for users who want a single deployable that includes auth without standing up Keycloak. Significant engineering effort; only justified if there's demand.
4. **`KeycloakAuthSpec` via testcontainers.** Offline-reproducible integration test as an alternative to the network-dependent `LiveAuthSpec`. Add only if the live test proves flaky in CI.
5. **Token revocation cache invalidation.** When introspection is configured, optionally subscribe to AS revocation events (RFC 7009) so locally cached `Principal`s are invalidated promptly. Currently we rely on the verifier being called on every request; the JWKS-based path has no per-token state to invalidate anyway.
6. **DPoP / mTLS bound tokens.** `login.jamesward.dev` advertises `dpop_signing_alg_values_supported` and `tls_client_certificate_bound_access_tokens`. Out of scope for v1.
