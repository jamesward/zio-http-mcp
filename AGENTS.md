# Agent Guidelines

## README Examples

Every code example in README.md must have a corresponding test. Tests live in:

- `McpToolSpec` — for tool DSL examples (`.handle`, `.handleWithContext`, error handling, etc.)
- `AuthSpec` — for authorization examples (`.auth(...)`, `ctx.principal`, `.requireScopes(...)`)
- `TokenVerifierSpec` — for `TokenVerifier.discoverJwks` / `.jwks` / `.introspection` behavior
- `LiveAuthSpec` — for end-to-end DCR + token + tool-call validation (raw HTTP) against `login.jamesward.dev`
- `JavaSdkAuthSpec` — for end-to-end Java MCP SDK interop with a bearer token against `login.jamesward.dev`
- `McpClientSpec` — for client examples (`McpClient.connect`, `listTools`, `callTool`, resources, error handling) against our own `McpServer` over loopback HTTP; also covers the legacy-pinned (`preferredVersion = V2025_11_25`) client
- `McpClientModernSpec` — for the modern (2026-07-28) client examples: `server/discover` negotiation, stateless calls, and the `onInputRequest` (MRTR) example
- `NegotiationSpec` — for protocol version negotiation: era detection, header validation, `server/discover`, the modern result envelope, version/header error responses, MRTR round trips, and modern request-scoped notification streaming (`_meta.progressToken` / `_meta.io.modelcontextprotocol/logLevel` → SSE)
- `TasksSpec` — for the Tasks extension example (`io.modelcontextprotocol/tasks`): task-augmented tool calls, `tasks/get`, `tasks/cancel`
- `TachyonInteropSpec` — for cross-implementation interop against the third-party `kpavlov/tachyon` server (modern negotiation + legacy interop)
- `McpClientLiveSpec` — for the no-auth client example against the live Java SDK server `https://www.javadocs.dev/mcp`
- `McpClientAuthSpec` — for the client `OAuthClientCredentials` example, exercised against our own auth-protected `McpServer` with tokens minted via DCR at `login.jamesward.dev`
- `CimdAuthSpec` — for the client `OAuthAuthorizationCode` example (authorization-code + PKCE, CIMD, DCR fallback, pre-registration, RFC 9207 `iss` validation, PRM resource validation), exercised in-process against `TestIdp` + our own auth-protected `McpServer` (loopback, no network)
- `LiveCimdAuthSpec` — for CIMD interop against the real `login.jamesward.dev` authorization server (metadata advertisement, client identification by metadata document, rejection of a mismatched or unresolvable document)
- `ClientConformanceSpec` — for the client-side auth behaviors graded by the official MCP conformance kit's client scenarios (`auth/basic-cimd`, `auth/iss-*`, `auth/metadata-*`, `auth/resource-mismatch`, `auth/scope-*`, `auth/pre-registration`, `auth/client-credentials-basic`), driving `ConformanceClientMain` as the client-under-test

When adding or modifying a README example, add or update the matching test in the appropriate spec.

## Testing

- Run `./sbt "testOnly *McpToolSpec*"` for tool DSL unit tests
- Run `./sbt "testOnly *AuthSpec* *ProtectedResourceMetadataSpec* *TokenVerifierSpec*"` for auth unit tests
- Run `./sbt "testOnly *LiveAuthSpec* *JavaSdkAuthSpec*"` for end-to-end auth tests against `login.jamesward.dev` (requires network access; tagged `live-auth` for filtering)
- Run `./sbt "testOnly *McpClientSpec*"` for client unit tests against our own server (loopback HTTP, no network)
- Run `./sbt "testOnly *McpClientLiveSpec*"` for the no-auth client test against `www.javadocs.dev` (requires network; tagged `live`)
- Run `./sbt "testOnly *McpClientAuthSpec*"` for the client OAuth `client_credentials` test against our own auth server + `login.jamesward.dev` (requires network; tagged `live-auth`)
- Run `./sbt "testOnly *CimdAuthSpec*"` for the client authorization-code + PKCE + CIMD flow against the in-process `TestIdp` (loopback, no network)
- Run `./sbt "testOnly *ClientConformanceSpec*"` for the official conformance kit's *client* auth scenarios against our `McpClient` (requires `npx`/Node and npm-registry network access; tagged `conformance-client`)
- Run `./sbt "testOnly *LiveCimdAuthSpec*"` for CIMD interop against `login.jamesward.dev` (requires network; tagged `live-auth`). The metadata documents it uses live in `src/test/resources/cimd` and are read over jsDelivr at the `main` ref, so the document-dependent assertions skip (with a warning) until a fixture change reaches `main`. To exercise them from a branch, publish the branch and set `CIMD_BASE_URL` to the branch's jsDelivr directory — note that sbt's server pins environment variables at startup, so run `./sbt shutdown` first when changing it
- Run `./sbt "testOnly *NegotiationSpec* *TasksSpec*"` for protocol version negotiation and Tasks-extension unit/HTTP tests (no network)
- Run `./sbt "testOnly *McpClientModernSpec*"` for the modern (2026-07-28) client negotiation tests against our own dual-era server (loopback, no network)
- Run `./sbt "testOnly *TachyonInteropSpec*"` for third-party interop against `kpavlov/tachyon` (loopback, no external network; JDK 21+)
- Run `./sbt "testOnly *ConformanceSpec*"` for MCP conformance tests (requires `npx`/Node and npm-registry network access). Runs the `2025-11-25` kit (`0.1.x`, pinned) and the `2026-07-28` kit (`0.2.0` line) — both are hard checks now that the `2026-07-28` spec is final
- The kit runs as a host process against a `localhost` URL, with an empty expected-failures baseline: every scenario is expected to pass. It previously ran in a testcontainer, which forced `host.testcontainers.internal` networking and made `dns-rebinding-protection` an unavoidable baselined failure

## Shared Test Helpers

`AuthTestHelpers` (in the test source tree) provides DCR + token-fetch + auth-server-build helpers used by `LiveAuthSpec`, `JavaSdkAuthSpec`, and `McpClientAuthSpec`. Reuse these rather than duplicating helper code when adding new auth integration tests.

`TestIdp` (in the test source tree) is a minimal in-process OAuth 2.1 authorization server for client-side auth flow tests: RFC 8414 metadata, auto-approving `/authorize` with CIMD dereferencing and configurable RFC 9207 `iss` behavior, PKCE-verifying `/token` minting RS256 JWTs (validated via `discoverJwks`), optional DCR, and recorded events for wire-level assertions. Used by `CimdAuthSpec`; reuse it for new client-side auth tests.

`ConformanceClientMain` (in the test source tree) is the client-under-test entrypoint for the conformance kit's client mode; it reads `MCP_CONFORMANCE_SCENARIO` / `MCP_CONFORMANCE_CONTEXT` / `MCP_CONFORMANCE_PROTOCOL_VERSION` and picks the matching `McpClientOAuth` config.
