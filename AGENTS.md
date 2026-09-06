# Agent Guidelines

## README Examples

Every code example in README.md must have a corresponding test. Tests live in:

- `McpToolSpec` — for tool DSL examples (`.handle`, `.handleWithContext`, error handling, etc.)
- `AuthSpec` — for authorization examples (`.auth(...)`, `ctx.principal`, `.requireScopes(...)`)
- `TokenVerifierSpec` — for `TokenVerifier.discoverJwks` / `.jwks` / `.introspection` behavior
- `LiveAuthSpec` — for end-to-end DCR + token + tool-call validation (raw HTTP) against `login.jamesward.dev`
- `JavaSdkAuthSpec` — for end-to-end Java MCP SDK interop with a bearer token against `login.jamesward.dev`
- `McpClientSpec` — for client examples (`McpClient.connect`, `listTools`, `callTool`, resources, error handling) against our own `McpServer` over loopback HTTP; also covers the legacy-pinned (`preferredVersion = V2025_11_25`) client
- `McpClientModernSpec` — for the modern (2026-07-28) client examples: `server/discover` negotiation, stateless calls, the `onInputRequest` (MRTR) example, and the multi-round exchange where the client echoes the server's `requestState`
- `NegotiationSpec` — for protocol version negotiation: era detection, header validation, `server/discover`, the modern result envelope, version/header error responses, modern request-scoped notification streaming (`_meta.progressToken` / `_meta.io.modelcontextprotocol/logLevel` → SSE), and MRTR (SEP-2322) — keyed `inputRequests`/`inputResponses`, `ctx.inputs` batching, signed `requestState` across rounds and its rejection when tampered, capability-gated input, malformed `inputResponses`, and `prompts/get` asking for input
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
- Run `./sbt "testOnly *LiveCimdAuthSpec*"` for CIMD interop against `login.jamesward.dev` (requires network; tagged `live-auth`). A CIMD document must be served from a public HTTPS URL matching its own `client_id`, so the spec mints one per test from the CIMD test server: `https://www.cimd.now/<port>/<path>` returns a document whose `client_id` is that URL and whose `redirect_uris` is `http://localhost:<port>/<path>`. Set `CIMD_TEST_SERVER` to point at another deployment — note that sbt's server pins environment variables at startup, so run `./sbt shutdown` first when changing it
- Run `./sbt "testOnly *NegotiationSpec* *TasksSpec*"` for protocol version negotiation and Tasks-extension unit/HTTP tests (no network)
- Run `./sbt "testOnly *McpClientModernSpec*"` for the modern (2026-07-28) client negotiation tests against our own dual-era server (loopback, no network)
- Run `./sbt "testOnly *TachyonInteropSpec*"` for third-party interop against `kpavlov/tachyon` (loopback, no external network; JDK 21+)
- Run `./sbt "testOnly *ConformanceSpec*"` for MCP conformance tests (requires Docker; no Node on the host). Runs the `2025-11-25` kit (`0.1.x`, pinned) and the `2026-07-28` kit (`0.2.0` line) — both are hard checks now that the `2026-07-28` spec is final
- The modern run uses the kit's `--requirements 2026-07-28` mode (kit `0.2.0-alpha.11`+), which runs exactly the scenarios that revision requires from its frozen manifest. That is the only mode that reaches the `input-required-result-*` (MRTR) scenarios: they were pending in the kit's own suite when the revision shipped, so the default `active` suite skips them. The legacy run stays on `--spec-version`, which is all the `0.1.x` line understands
- The MRTR scenarios drive named fixtures on the test server (`test_input_required_result_*` and the `test_input_required_result_prompt` prompt). The names, the input-request keys, and the `state-ok` marker are fixed by the suite — read the scenario descriptions (`conformance list`, or the scenario's `description`) before changing one
- `server-stateless` is baselined as an expected failure. `--requirements` reaches scenarios `--spec-version` never ran, and its failing checks are SEP-2575 gaps of their own: no `-32602` + HTTP 400 for a modern request whose `_meta` omits `protocolVersion`/`clientCapabilities`, no `MissingRequiredClientCapabilityError` (-32021) path, and `subscriptions/listen` neither acknowledges a subscription nor tags notifications with a subscription id. The kit's not-scored scenarios (the `tasks-*` extension, and scenarios added after the revision shipped) run and report but never count, so they need no baseline
- Run the kit without Docker — a host with Node but no daemon, or to iterate on one scenario — by serving the same fixtures directly: `./sbt "Test/runMain com.jamesward.ziohttp.mcp.ConformanceServerMain"` (port 3000, override with `PORT`), then `npx @modelcontextprotocol/conformance@<version> server --url http://localhost:3000/mcp --requirements 2026-07-28`. `ConformanceSpec` remains the checked-in run: it pins the versions and the baseline
- The kit runs in a testcontainer built from `node:22-slim` with the kit preinstalled, so the host needs nothing but Docker. On Linux the container joins the host network namespace and reaches the server at a real `localhost` URL, which lets `dns-rebinding-protection` run and keeps the expected-failures baseline empty — every scenario is expected to pass. Elsewhere it falls back to bridged networking via `host.testcontainers.internal`, where that one scenario is baselined. Force a mode with `CONFORMANCE_HOST_NETWORK=true|false`
- If the host sits behind a TLS-intercepting proxy, the image build picks up its CA from the standard environment variables (`NODE_EXTRA_CA_CERTS`, `SSL_CERT_FILE`, `CURL_CA_BUNDLE`, `REQUESTS_CA_BUNDLE`) and trusts it, so `npm install` does not fail the build with `SELF_SIGNED_CERT_IN_CHAIN`. On a machine with none of those set, the build is unchanged

## Shared Test Helpers

`AuthTestHelpers` (in the test source tree) provides DCR + token-fetch + auth-server-build helpers used by `LiveAuthSpec`, `JavaSdkAuthSpec`, and `McpClientAuthSpec`. Reuse these rather than duplicating helper code when adding new auth integration tests.

It also provides `retryTransientUpstream`, a `TestAspect` applied to every spec that talks to a hosted service. It retries a test up to 3 times with exponential backoff, but **only** when the failure text looks like a transient outage (`ServiceUnavailable`/`503`, `502`, `504`, dropped or timed-out connections). Assertion failures and other runtime errors are never retried, so a real regression still fails on the first attempt. Add it to new specs that depend on an external host; don't add it to offline specs, where a flake is a bug worth seeing.

`TestIdp` (in the test source tree) is a minimal in-process OAuth 2.1 authorization server for client-side auth flow tests: RFC 8414 metadata, auto-approving `/authorize` with CIMD dereferencing and configurable RFC 9207 `iss` behavior, PKCE-verifying `/token` minting RS256 JWTs (validated via `discoverJwks`), optional DCR, and recorded events for wire-level assertions. Used by `CimdAuthSpec`; reuse it for new client-side auth tests.

`ConformanceServerMain` (in the test source tree) serves `ConformanceSpec.testServer` on a fixed port so the kit can be pointed at it without Docker.

`ConformanceClientMain` (in the test source tree) is the client-under-test entrypoint for the conformance kit's client mode; it reads `MCP_CONFORMANCE_SCENARIO` / `MCP_CONFORMANCE_CONTEXT` / `MCP_CONFORMANCE_PROTOCOL_VERSION` and picks the matching `McpClientOAuth` config.
