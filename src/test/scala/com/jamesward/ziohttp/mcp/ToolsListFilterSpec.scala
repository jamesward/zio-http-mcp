package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.auth.*
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*
import zio.test.TestAspect.*

import java.time.Instant

/** Tests for `tools/list` filtering by the caller's OAuth scopes.
  *
  * The contract:
  *   - When the server has `.auth(...)` configured, `tools/list`
  *     returns only those tools whose combined required scopes
  *     (`auth.requiredScopes ++ tool.requiredScopes`) are a subset
  *     of the caller's principal scopes.
  *   - Tools with no `.requireScopes(...)` are visible to any
  *     authenticated caller (server-wide auth already passed to
  *     reach the dispatch).
  *   - Without `.auth(...)`, every tool is visible (no principal,
  *     no filter).
  *
  * The corresponding scope-check on `tools/call` is in `AuthSpec`
  * (test "10 — per-tool scope: delete_user denied without admin
  * scope"); this spec asserts the discovery side of the contract.
  */
object ToolsListFilterSpec extends ZIOSpecDefault:

  given canEqualStatus: CanEqual[zio.http.Status, zio.http.Status] = CanEqual.derived

  /* ─────────────── fixtures ─────────────── */

  private val testResourceUri = ResourceUri.parse("http://localhost:0/mcp").toOption.get
  private val testIssuer      = AuthorizationServer("https://auth.example.com")

  private val baseScope    = OauthScope("mcp:tools")     // server-wide scope
  private val authorScope  = OauthScope("role:author")
  private val adminScope   = OauthScope("role:admin")

  private def principalWith(scopes: OauthScope*): Principal =
    Principal(
      subject   = Some("test-user"),
      clientId  = Some("test-client"),
      scopes    = scopes.toSet,
      audience  = Set("http://localhost:0/mcp"),
      issuer    = Some("https://auth.example.com"),
      expiresAt = Some(Instant.now().plusSeconds(3600)),
      raw       = "test-token",
      claims    = Json.Obj(),
    )

  /** Verifier that decodes the bearer token as a comma-separated list
    * of scope strings — e.g. `Bearer mcp:tools,role:author` produces a
    * principal with those two scopes. Lets each test assert on a
    * specific scope set without juggling multiple stub verifiers. */
  private val scopeEncodingVerifier: TokenVerifier[Any] =
    TokenVerifier.fromFunction { token =>
      if token.isEmpty then ZIO.fail(AuthError.Invalid("empty"))
      else
        val scopes = token.split(",").iterator.map(OauthScope(_)).toSet
        ZIO.succeed(principalWith(scopes.toSeq*))
    }

  private def authConfig(required: Set[OauthScope] = Set.empty): McpAuth[Any] =
    McpAuth(
      resourceUri          = Some(testResourceUri),
      authorizationServers = NonEmptyChunk(testIssuer),
      scopesSupported      = Chunk(baseScope, authorScope, adminScope),
      verifier             = scopeEncodingVerifier,
      requiredScopes       = required,
    )

  /* ─────────────── tools ─────────────── */

  private val publicTool: McpToolHandler =
    McpTool("public").description("no role gate").handle:
      ZIO.succeed("public")

  private val authorTool: McpToolHandler =
    McpTool("authorOnly").description("authors and above").requireScopes(authorScope).handle:
      ZIO.succeed("author")

  private val adminTool: McpToolHandler =
    McpTool("adminOnly").description("admins only").requireScopes(adminScope).handle:
      ZIO.succeed("admin")

  /* ─────────────── server constructors ─────────────── */

  private def authedServer(required: Set[OauthScope] = Set.empty): McpServer[Any] =
    McpServer("filter-test", "0.1.0")
      .tool(publicTool)
      .tool(authorTool)
      .tool(adminTool)
      .auth(authConfig(required))

  private val unauthedServer: McpServer[Any] =
    McpServer("filter-test-noauth", "0.1.0")
      .tool(publicTool)
      .tool(authorTool)
      .tool(adminTool)

  /* ─────────────── HTTP helpers ─────────────── */

  import zio.http.*

  private def installStateless(server: McpServer[Any]): ZIO[Server, Throwable, Int] =
    Server.install(server.statelessRoutes)

  private def post(port: Int, body: String, token: Option[String] = None): ZIO[Client & Scope, Throwable, Response] =
    val url = URL.decode(s"http://localhost:$port/mcp").toOption.get
    val base = Request.post(url, Body.fromString(body))
      .addHeader(Header.ContentType(MediaType.application.json))
    val withToken = token.fold(base)(t => base.addHeader("authorization", s"Bearer $t"))
    ZClient.batched(withToken)

  private val toolsListRequest = """{"jsonrpc":"2.0","id":1,"method":"tools/list"}"""

  /** Pull the tool names out of a `tools/list` JSON response. */
  private def toolNames(body: String): Set[String] =
    body.fromJson[Json.Obj].toOption
      .flatMap(_.get("result")).flatMap(_.asObject)
      .flatMap(_.get("tools")).flatMap(_.asArray)
      .toList.flatten
      .flatMap(_.asObject).flatMap(_.get("name")).flatMap(_.asString)
      .toSet

  /* ─────────────── spec ─────────────── */

  override def spec =
    suite("ToolsListFilterSpec")(

      test("caller with admin scope sees all three tools"):
        for
          port <- installStateless(authedServer())
          rsp  <- post(port, toolsListRequest, token = Some("role:admin,role:author"))
          body <- rsp.body.asString
        yield assertTrue(
          rsp.status == Status.Ok,
          toolNames(body) == Set("public", "authorOnly", "adminOnly"),
        )
      ,

      test("caller with author scope sees public + authorOnly, not adminOnly"):
        for
          port <- installStateless(authedServer())
          rsp  <- post(port, toolsListRequest, token = Some("role:author"))
          body <- rsp.body.asString
        yield assertTrue(
          rsp.status == Status.Ok,
          toolNames(body) == Set("public", "authorOnly"),
        )
      ,

      test("caller with no role scope sees only the unrestricted tool"):
        for
          port <- installStateless(authedServer())
          rsp  <- post(port, toolsListRequest, token = Some("mcp:tools"))
          body <- rsp.body.asString
        yield assertTrue(
          rsp.status == Status.Ok,
          toolNames(body) == Set("public"),
        )
      ,

      test("server-wide required scopes are factored into the filter"):
        // With auth.requiredScopes = {mcp:tools}, every tool's
        // combined required scope set includes mcp:tools. A caller
        // without it can't even reach tools/list (the server-wide
        // check rejects them earlier). A caller WITH mcp:tools
        // sees the unrestricted tool through the filter.
        for
          port <- installStateless(authedServer(required = Set(baseScope)))
          rsp  <- post(port, toolsListRequest, token = Some("mcp:tools"))
          body <- rsp.body.asString
        yield assertTrue(
          rsp.status == Status.Ok,
          toolNames(body) == Set("public"),
        )
      ,

      test("admin scope passes server-wide + per-tool scope check"):
        for
          port <- installStateless(authedServer(required = Set(baseScope)))
          rsp  <- post(port, toolsListRequest, token = Some("mcp:tools,role:author,role:admin"))
          body <- rsp.body.asString
        yield assertTrue(
          rsp.status == Status.Ok,
          toolNames(body) == Set("public", "authorOnly", "adminOnly"),
        )
      ,

      test("without .auth(...), tools/list returns every tool"):
        // No auth means no principal, no filter. Server-wide auth is
        // disabled; per-tool .requireScopes(...) is silently ignored.
        for
          port <- installStateless(unauthedServer)
          rsp  <- post(port, toolsListRequest)  // no token
          body <- rsp.body.asString
        yield assertTrue(
          rsp.status == Status.Ok,
          toolNames(body) == Set("public", "authorOnly", "adminOnly"),
        )
      ,

    ).provide(Server.defaultWithPort(0), Client.default, Scope.default) @@ sequential @@ withLiveClock @@ timeout(30.seconds)
