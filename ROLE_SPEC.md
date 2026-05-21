# Role Spec

Design for role-based access control over MCP tools. Once a request is authenticated, the visible tool list and the set of tools the caller can invoke depend on the caller's identity.

References:
- [AUTH_SPEC.md](./AUTH_SPEC.md) — establishes `Principal`, `OauthScope`, the auth middleware, per-tool `.requireScopes(...)`, and the tools-list / tools-call dispatch path.
- [MCP authorization spec, 2025-11-25](https://modelcontextprotocol.io/specification/2025-11-25/basic/authorization) — sets the spec floor for token validation, scope challenges, and step-up authorization.

---

## Goals

1. **Filter `tools/list` per caller.** Each authenticated client sees only the tools they're allowed to invoke. No information leak about hidden tools' existence.
2. **Enforce on `tools/call` consistently.** A tool that doesn't appear in the caller's `tools/list` cannot be invoked, even if the client guesses the name. Treat hidden tools the same way unknown tools are treated (not as "forbidden").
3. **Layer cleanly on top of existing primitives.** Build on `Principal` and the existing per-tool `.requireScopes(...)` slot. Don't replace OAuth scopes — roles are an *additional*, *application-level* gating dimension.
4. **Pluggable role source.** Roles come from wherever the operator wants: a JWT claim (`roles` / `groups` / custom), a database lookup keyed by `sub`, an in-memory allowlist, etc. The library doesn't prescribe a specific role model.
5. **Maximally simple for the common case.** `tool.requireRoles("admin")` with a server-wide role extractor should just work. Custom predicates are available for the long tail.
6. **No-auth servers behave unchanged.** When `.auth(...)` is not configured, role declarations are silently ignored, just like `.requireScopes(...)`. Authoring stays fully opt-in.

## Non-Goals

- **Centralized role registry / hierarchy** (e.g., role inheritance, "admin > editor > viewer"). The library treats roles as opaque identifiers; if you want hierarchy, encode it in your extractor.
- **Resource and prompt gating.** Initial scope is tools. Per-resource and per-prompt gating tracks the same v1 / v2 split as `.requireScopes(...)` — see *Future work*.
- **Run-time mutation of role assignments.** Role assignments are derived from `Principal` per-request; we don't ship a CRUD UI or persistent role store.
- **Defining "what's a role" semantically.** Roles are application strings. Whether `admin` means "can do anything" or "can read user records" is up to the deployment.

---

## Conceptual model

```
                   ┌──────────────────────────┐
   bearer token ──▶│   AuthMiddleware         │── Principal ──┐
                   │   (signature, iss, aud,  │               │
                   │    scope, audience)      │               ▼
                   └──────────────────────────┘    ┌──────────────────────┐
                                                   │  RoleExtractor       │
                                                   │  Principal => Roles  │
                                                   └──────────┬───────────┘
                                                              │
                                                          Set[Role]
                                                              │
                          ┌───────────────────────────────────┴────────────────────────┐
                          ▼                                                            ▼
                ┌──────────────────────┐                                ┌────────────────────────┐
                │  tools/list filter   │                                │  tools/call check      │
                │  (visibility)        │                                │  (defense in depth)    │
                └──────────────────────┘                                └────────────────────────┘
```

A tool's per-request visibility is determined by a **predicate** of type `Principal => Boolean`. The library provides:

1. A direct primitive: `tool.allowFor(principal => …)` — full flexibility.
2. Sugar over the primitive: `tool.requireRoles("admin")` — uses the server-wide `RoleExtractor` to compute roles for the principal, then checks subset containment.

The same predicate is used for both `tools/list` filtering and `tools/call` resolution — there's no second policy to keep in sync.

### Why a separate "Role" concept (not just OauthScope)?

OAuth scopes describe *what permissions the OAuth client requested and was granted at the AS*. Roles describe *what the principal is, in your application's domain model*. They're related but not the same:

- A user can have role `editor` regardless of what scopes their token carries (the token might just have `openid profile`).
- Multiple OAuth flows might issue tokens for the same user with different scopes; their role assignment is the same.
- Roles often come from sources other than the token — a user database, an LDAP group, an entitlements service.

Conflating them would force users to encode roles as scopes, which is fine when it's true and confusing when it isn't. We keep them distinct types and let the operator bridge them with a `RoleExtractor`.

That said: if your AS already attaches roles as scopes (e.g. `mcp:role:admin`), the trivial extractor `principal => principal.scopes.map(s => Role(s.value))` makes the bridge.

---

## DSL

### Types

```scala
package com.jamesward.ziohttp.mcp.auth

opaque type Role = String

object Role:
  def apply(s: String): Role = s
  extension (r: Role) def value: String = r
  given CanEqual[Role, Role] = CanEqual.derived
  given JsonEncoder[Role] = JsonEncoder.string
  given JsonDecoder[Role] = JsonDecoder.string

/**
 * Computes the set of roles for a given authenticated [[Principal]]. The function may be
 * pure (e.g. read a JWT claim) or effectful (e.g. database lookup); the latter is supported
 * via [[RoleExtractor.fromZIO]].
 */
trait RoleExtractor[-R]:
  def extract(principal: Principal): ZIO[R, Nothing, Set[Role]]

object RoleExtractor:
  /** Constant — every authenticated principal gets the same roles. Useful for tests. */
  def constant(roles: Set[Role]): RoleExtractor[Any] =
    new RoleExtractor[Any] { def extract(p: Principal) = ZIO.succeed(roles) }

  /** Pure function over [[Principal]] — most common case. */
  def fromFunction(f: Principal => Set[Role]): RoleExtractor[Any] =
    new RoleExtractor[Any] { def extract(p: Principal) = ZIO.succeed(f(p)) }

  /** Effectful (e.g. database lookup). */
  def fromZIO[R](f: Principal => ZIO[R, Nothing, Set[Role]]): RoleExtractor[R] =
    new RoleExtractor[R] { def extract(p: Principal) = f(p) }

  /**
   * Convenience: read a top-level array claim like `roles` / `groups` and parse string
   * entries into `Role` values. Missing or wrong-typed claims yield the empty set.
   */
  def fromClaim(name: String): RoleExtractor[Any] =
    fromFunction { p =>
      p.claims.get(name) match
        case Some(zio.json.ast.Json.Arr(xs)) =>
          xs.collect { case zio.json.ast.Json.Str(s) => Role(s) }.toSet
        case Some(zio.json.ast.Json.Str(s))  => s.split("[,\\s]+").filter(_.nonEmpty).map(Role(_)).toSet
        case _                                => Set.empty
    }

  /** Bridge OAuth scopes to roles 1:1. Useful when the AS encodes roles as scope strings. */
  val fromScopes: RoleExtractor[Any] =
    fromFunction(p => p.scopes.map(s => Role(s.value)))

  /** No roles ever — useful as a placeholder; combined with `.requireRoles(...)` makes every
   *  gated tool inaccessible (escape hatch for testing). */
  val none: RoleExtractor[Any] =
    constant(Set.empty)
```

### Server-wide configuration

The role extractor lives on `McpAuth`, alongside the verifier:

```scala
final case class McpAuth[-R](
  authorizationServers: NonEmptyChunk[AuthorizationServer],
  verifier: TokenVerifier[R],
  // existing fields …
  resourceUri: Option[ResourceUri] = None,
  scopesSupported: Chunk[OauthScope] = Chunk.empty,
  // …
  // new:
  roleExtractor: RoleExtractor[R] = RoleExtractor.none,
):
  def withRoles(extractor: RoleExtractor[R]): McpAuth[R] = copy(roleExtractor = extractor)
```

When `roleExtractor` is the default `none`, any tool with a non-empty `requiredRoles` is effectively invisible. That's the safe default: opt-in roles, no accidental leak.

### Per-tool gating

`McpTool` gains two new builders:

```scala
final class McpTool private (…):

  /**
   * Restrict visibility/invocation to principals who hold ALL of the supplied roles.
   * Combines (AND) with [[requireScopes]] and any [[allowFor]] predicates.
   *
   * Server-wide [[McpAuth.roleExtractor]] computes the principal's role set; this method
   * is sugar for `allowFor(p => extractor(p).contains(allOf))`.
   */
  def requireRoles(roles: Role*): McpTool

  /**
   * Restrict visibility/invocation to principals who hold ANY of the supplied roles.
   * Useful for "admin or staff can do this" semantics.
   */
  def requireAnyRole(roles: Role*): McpTool

  /**
   * Arbitrary predicate over the [[Principal]]. May be effectful (e.g. database lookup).
   * Composes with [[requireRoles]] / [[requireScopes]] via AND.
   *
   * When auth is not configured, the predicate is silently ignored.
   */
  def allowFor[R1](predicate: Principal => Boolean): McpTool[R1]
  def allowForZIO[R1](predicate: Principal => ZIO[R1, Nothing, Boolean]): McpTool[R1]
```

A tool with multiple gating clauses passes only when all clauses pass:

```scala
val deleteUser = McpTool("delete_user")
  .description("Permanently deletes a user account")
  .requireScopes(OauthScope("admin"))           // OAuth: token must have admin scope
  .requireRoles(Role("user_admin"))              // App role: user_admin
  .allowForZIO(p => featureFlag(p.subject))      // Plus an effectful gate
  .handle[Any, ToolError, DeleteInput, String]: input => …
```

### Examples

**Common case — single role from a JWT claim:**

```scala
val auth = McpAuth(
  authorizationServers = NonEmptyChunk(AuthorizationServer("https://login.jamesward.dev")),
  verifier             = verifier,
  roleExtractor        = RoleExtractor.fromClaim("roles"),
)

val server = McpServer("my-server", "1.0.0")
  .auth(auth)
  .tool(McpTool("list_users").handle: ZIO.succeed("…"))                      // public to anyone authenticated
  .tool(McpTool("delete_user").requireRoles(Role("admin")).handle: …)        // only role=admin
  .tool(McpTool("audit").requireRoles(Role("admin"), Role("compliance")).handle: …)  // BOTH roles
  .tool(McpTool("triage").requireAnyRole(Role("admin"), Role("oncall")).handle: …)   // EITHER role
```

A token whose `roles` claim is `["admin"]` sees `list_users`, `delete_user`, and `triage` in `tools/list`, but not `audit`. A token whose `roles` claim is `[]` or absent sees only `list_users`.

**Database-backed roles:**

```scala
trait UserRepo:
  def rolesFor(subject: String): UIO[Set[Role]]

val auth = McpAuth(
  authorizationServers = NonEmptyChunk(AuthorizationServer(...)),
  verifier             = verifier,
  roleExtractor        = RoleExtractor.fromZIO[UserRepo]: principal =>
    ZIO.serviceWithZIO[UserRepo](_.rolesFor(principal.subject.getOrElse(""))),
)
```

The `R` in `McpAuth[R]` accumulates `UserRepo`, so the `routes` type signature reflects it and the user provides the layer at server start.

**Fully arbitrary policy:**

```scala
val tenantTool = McpTool("read_tenant_data")
  .allowForZIO[TenantService]: principal =>
    val tenantId = principal.claims.get("tid").flatMap(_.asString).getOrElse("")
    ZIO.serviceWithZIO[TenantService](_.userBelongsTo(principal.subject.getOrElse(""), tenantId))
  .handle(...)
```

---

## Wire behavior

### `tools/list`

After the request is authenticated, before formatting the response:

```
visibleTools = registeredTools.filter { tool =>
  predicateFor(tool, principal)  // server-wide RoleExtractor + per-tool requireRoles + allowFor
}
```

The response shape is unchanged — we just emit fewer entries. Pagination cursors stay opaque to the client; the server takes the visible-set into account when computing them.

For unauthenticated mode (no `.auth(...)`), no filtering — all tools are visible.

For authenticated mode where the principal has no special grants, tools that declared `requireRoles` / `requireAnyRole` / non-trivial `allowFor` are excluded from the list.

### `tools/call`

Lookup is restricted to the visible set. The visibility predicate runs *before* the existing `enforceToolScopes` check:

```
1. Authenticate (extract Bearer, verify, audience-bind)
2. resolveToolCall(name, params):
     - if name not in visibleTools(principal) → InvalidParams "Unknown tool: <name>"
3. enforceToolScopes (existing — checks per-tool requireScopes against principal.scopes)
4. Run the tool handler
```

A tool that's hidden from the principal returns the same error a client gets for a tool name that doesn't exist at all. This avoids leaking the existence of restricted tools through differential error messages.

### Why "Unknown tool" and not "Forbidden"?

Two consistent design alternatives:

1. **Hidden = Unknown.** `tools/list` filters; `tools/call` returns `Unknown tool` for hidden + non-existent. ← chosen
2. **Hidden = Forbidden.** `tools/list` filters; `tools/call` returns `403 Forbidden` for tools the principal isn't allowed to call.

Choice #1 has these advantages:
- A client that successfully called `tools/list` and got `[a, b]` and tries to call `c` learns *nothing new* — `c` is unknown to them, full stop. There's no information channel about the existence of hidden tools.
- Aligns with REST conventions where unauthorized access to non-listed resources commonly returns 404.
- Consistent error type with truly-unknown tools — clients don't need branching error handling.

The cost: a client *can* learn about hidden tools by other means (documentation, prior knowledge) and not get a clear "you don't have permission" signal. We accept this — if you want explicit "you can't do this" feedback, use `requireScopes` instead, which yields a 403 step-up challenge. Roles are for hiding; scopes are for challenging.

This keeps the two mechanisms cleanly distinct:

| Mechanism                       | Visibility          | Failure mode                                    |
|---------------------------------|---------------------|-------------------------------------------------|
| `requireScopes` (OAuth)         | Visible to all      | 403 + `WWW-Authenticate: insufficient_scope`    |
| `requireRoles` / `allowFor`     | Hidden from non-eligible callers | `InvalidParams: Unknown tool: <name>` |

A tool may use both — visible only to those with the right roles, AND requiring the right scopes to actually invoke. The middleware applies them in order: roles first (visibility), scopes second (capability challenge).

### Notifications: `notifications/tools/list_changed`

Out of scope for v1 (and for the existing scope/visibility model). The library doesn't issue this notification when role assignments change, because role changes happen out-of-band (token re-issuance, database update). Clients that care should re-call `tools/list` on logical refresh events.

---

## Behavior matrix

| Auth     | `requireRoles` set | Principal's roles ⊇ required | `tools/list` shows it? | `tools/call` succeeds? |
|----------|--------------------|------------------------------|------------------------|------------------------|
| disabled | yes                | (n/a)                        | yes                    | yes                    |
| disabled | no                 | (n/a)                        | yes                    | yes                    |
| enabled  | yes                | yes                          | yes                    | yes (subject to scopes)|
| enabled  | yes                | no                           | no                     | "Unknown tool"         |
| enabled  | no                 | (any)                        | yes                    | yes (subject to scopes)|
| enabled  | (uses `allowFor` returning false) | (any)         | no                     | "Unknown tool"         |

When the server-wide `roleExtractor` is `RoleExtractor.none` and a tool declares `requireRoles`, the tool is invisible — operator must either remove `.requireRoles(...)` or supply a non-trivial extractor.

---

## Implementation plan

### New files

- `src/main/scala/com/jamesward/ziohttp/mcp/auth/Role.scala` — `Role` opaque type and `RoleExtractor` trait + factories.

### Modified files

- `auth/McpAuth.scala` — add `roleExtractor: RoleExtractor[R]` field with default `RoleExtractor.none`; carry the `R` from the extractor into the `McpAuth` `R` type parameter.
- `McpTool.scala` — add three builder methods:
  - `requireRoles(roles: Role*)` — stored as `Set[Role]` on the handler
  - `requireAnyRole(roles: Role*)` — stored as another `Set[Role]` on the handler
  - `allowFor[R1](Principal => Boolean)` and `allowForZIO[R1](Principal => ZIO[R1, Nothing, Boolean])` — stored as `Principal => ZIO[R1, Nothing, Boolean]`. Returns `McpTool` with R widened.
- `McpToolHandlerR` trait — add the three new fields with sensible defaults (empty sets, `_ => true` predicate).
- `McpServer.scala`:
  - Compute `visibleTools(principal): UIO[Chunk[McpToolHandlerR[R]]]` once per request, after auth.
  - `handleToolsList` filters by `visibleTools`.
  - `resolveToolCall` looks up against `visibleTools` (so a hidden tool returns `InvalidParams: Unknown tool` rather than `enforceToolScopes` running).
  - `R & R_extractor` propagates so the `routes` type signature carries the extractor's environment.

### Type-level concern

`McpAuth.roleExtractor: RoleExtractor[R]` and per-tool `allowForZIO[R1]` both add new `R` requirements. They merge into the server's `R` like any other tool layer, so:

```scala
McpServer("…", "1.0.0")
  .auth(McpAuth(…, roleExtractor = RoleExtractor.fromZIO[UserRepo](…)))
  .tool(toolA.allowForZIO[FeatureFlags](…))

// server.routes: Routes[UserRepo & FeatureFlags & McpServer.State, Response]
```

The user supplies the layers via `.provide(...)` exactly as they do today for tool environments.

---

## Test plan

Tests live in a new `RoleSpec` (mirroring `AuthSpec`'s structure), with one live-AS-shaped test added to `LiveAuthSpec` and one Java-SDK-shaped test added to `JavaSdkAuthSpec` to confirm interop.

### Unit tests — `RoleSpec`

Use `TokenVerifier.fromFunction` + `RoleExtractor.constant` so we don't need a live AS or JWT crypto.

| # | Scenario                                                                                                                  | Assertion                                                                          |
|---|---------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------|
| 1 | Auth disabled, tool with `.requireRoles("admin")` → still listed and invokable                                            | `requireRoles` is silently ignored when `auth = None`                              |
| 2 | Auth on, no extractor configured (default `none`), tool gated by `requireRoles` → tool hidden from `tools/list`            | Visible tools list excludes the gated tool                                         |
| 3 | Auth on, extractor returns `Set("admin")`, tool gated by `requireRoles("admin")` → tool visible                            | Visible tools list includes the gated tool                                         |
| 4 | Auth on, extractor returns `Set("user")`, tool gated by `requireRoles("admin")` → tool hidden, call returns "Unknown tool" | `tools/list` excludes; `tools/call` returns `InvalidParams` with name not found    |
| 5 | `.requireRoles("a", "b")` requires ALL roles                                                                              | Principal with `Set("a")` is denied; with `Set("a","b")` is allowed                |
| 6 | `.requireAnyRole("a", "b")` requires ANY role                                                                             | Principal with `Set("a")` is allowed; with `Set("c")` is denied                    |
| 7 | `.allowFor(p => p.subject.contains("alice"))` predicate                                                                   | Predicate is evaluated, controls visibility and call                               |
| 8 | `.allowForZIO[R1]` predicate plus `.requireRoles(...)` — both must pass                                                    | Tool visible only when both clauses are satisfied                                  |
| 9 | Roles + scopes interaction                                                                                                | Gated tool with `requireRoles("admin")` AND `requireScopes("write")`. With both → ok. With roles only → 403 insufficient_scope. With scopes only → "Unknown tool". |
| 10 | `RoleExtractor.fromClaim("roles")` parses array, string-CSV, and missing claim                                           | Three sub-cases: `["a","b"]`, `"a,b"`, missing → `Set.empty`                       |
| 11 | `RoleExtractor.fromScopes` bridges scopes to roles                                                                        | A token with `scope="admin"` resolves to `Set(Role("admin"))`                      |
| 12 | `RoleExtractor.fromZIO` accepts an effectful extractor with its own `R` requirement                                       | Verifies `R` propagates into the `routes` type signature                           |
| 13 | Pagination consistency                                                                                                    | If `tools/list` is paginated and the principal can see only N visible tools, cursor traversal yields exactly those N (no jumps over hidden tools that change cursor math) |

### `LiveAuthSpec` extension

Add one test that uses a real JWT from `login.jamesward.dev` and a real `roleExtractor` reading the JWT `scope` claim (we don't control the AS's claim layout, so this is the path of least resistance). Verifies end-to-end that an `admin`-scoped token sees an `admin`-gated tool.

### `JavaSdkAuthSpec` extension

Add one test that uses the Java MCP SDK to call `tools/list` with a bearer token, asserts the visible-tools set matches the principal's roles. Confirms the Java SDK doesn't see hidden tools and treats the filtered list as authoritative.

---

## Resolved decisions

1. **Hidden = Unknown** (vs `Forbidden`). Visibility-based access control should not leak information about hidden tools through differential error responses. Scope-based access control already covers the "tell me clearly" case (returns 403 with step-up challenge).
2. **Roles are opaque strings.** No hierarchy, no inheritance — encode those policies in your `RoleExtractor` if you need them.
3. **Roles are separate from OAuth scopes.** Both can gate a tool, AND'd together. `RoleExtractor.fromScopes` is the bridge if you want them coupled.
4. **No-auth servers ignore role declarations** — same model as `.requireScopes(...)`.
5. **Server-wide extractor + per-tool predicate.** Server-wide extractor produces `Set[Role]` once; per-tool `requireRoles` does subset checks. Per-tool `allowFor` for the long tail.

## Future work

1. **Resource and prompt gating.** Apply the same `requireRoles` / `allowFor` to `McpResource` and `McpPrompt`. Fits naturally — same predicate machinery — but defer until tools land cleanly.
2. **Role hierarchies / aliases as a built-in.** Currently encoded in the `RoleExtractor` if needed. Could become a first-class concept later.
3. **`notifications/tools/list_changed` on role change.** Requires an event bus the library doesn't have today. A user can drive this themselves by holding a `Hub[ToolListChanged]` and emitting on their role-change events.
4. **`requireAuthenticated` shorthand.** A common pattern is "any authenticated user, no specific role" — currently expressed by *not* declaring `.requireRoles(...)`. That's already correct behavior, but a `.requireAuthenticated` builder would be a clearer signal in source.
5. **Audit logging.** A structured log line per visibility / authorization decision, to help operators audit access. The existing `Auth ok` / `Auth failed` lines cover the auth event; we could extend them with `tool=… visible=true/false`.
6. **Per-call principal claims override** — e.g., a feature-flag service supplying additional ad-hoc claims at call time. Use `allowForZIO` for now; first-class support if a real use case emerges.
