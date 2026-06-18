# Agent Guidelines

## README Examples

Every code example in README.md must have a corresponding test. Tests live in:

- `McpToolSpec` — for tool DSL examples (`.handle`, `.handleWithContext`, error handling, etc.)
- `AuthSpec` — for authorization examples (`.auth(...)`, `ctx.principal`, `.requireScopes(...)`)
- `TokenVerifierSpec` — for `TokenVerifier.discoverJwks` / `.jwks` / `.introspection` behavior
- `LiveAuthSpec` — for end-to-end DCR + token + tool-call validation (raw HTTP) against `login.jamesward.dev`
- `JavaSdkAuthSpec` — for end-to-end Java MCP SDK interop with a bearer token against `login.jamesward.dev`
- `McpClientSpec` — for client examples (`McpClient.connect`, `listTools`, `callTool`, resources, error handling) against our own `McpServer` over loopback HTTP
- `McpClientLiveSpec` — for the no-auth client example against the live Java SDK server `https://www.javadocs.dev/mcp`
- `McpClientAuthSpec` — for the client `OAuthClientCredentials` example, exercised against our own auth-protected `McpServer` with tokens minted via DCR at `login.jamesward.dev`

When adding or modifying a README example, add or update the matching test in the appropriate spec.

## Testing

- Run `./sbt "testOnly *McpToolSpec*"` for tool DSL unit tests
- Run `./sbt "testOnly *AuthSpec* *ProtectedResourceMetadataSpec* *TokenVerifierSpec*"` for auth unit tests
- Run `./sbt "testOnly *LiveAuthSpec* *JavaSdkAuthSpec*"` for end-to-end auth tests against `login.jamesward.dev` (requires network access; tagged `live-auth` for filtering)
- Run `./sbt "testOnly *McpClientSpec*"` for client unit tests against our own server (loopback HTTP, no network)
- Run `./sbt "testOnly *McpClientLiveSpec*"` for the no-auth client test against `www.javadocs.dev` (requires network; tagged `live`)
- Run `./sbt "testOnly *McpClientAuthSpec*"` for the client OAuth `client_credentials` test against our own auth server + `login.jamesward.dev` (requires network; tagged `live-auth`)
- Run `./sbt "testOnly *ConformanceSpec*"` for MCP conformance tests (requires Docker)
- Conformance tests use testcontainers with `host.testcontainers.internal` for Docker networking (rootless Docker compatible)

## Shared Test Helpers

`AuthTestHelpers` (in the test source tree) provides DCR + token-fetch + auth-server-build helpers used by `LiveAuthSpec`, `JavaSdkAuthSpec`, and `McpClientAuthSpec`. Reuse these rather than duplicating helper code when adding new auth integration tests.
