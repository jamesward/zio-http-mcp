package com.jamesward.ziohttp.mcp

import zio.*
import zio.http.*
import zio.json.*
import zio.json.ast.Json
import zio.schema.*
import zio.test.*
import zio.test.TestAspect.*

given canEqualStatusNeg: CanEqual[Status, Status] = CanEqual.derived

/**
 * Exercises MCP protocol version negotiation and the modern (2026-07-28)
 * request/response envelope against the dual-era `routes`. Split into pure unit
 * tests over [[Negotiation]] and HTTP-level tests over a running server.
 */
object NegotiationSpec extends ZIOSpecDefault:

  private val Modern = ProtocolVersion.V2026_07_28.wire
  private val Legacy = ProtocolVersion.V2025_11_25.wire

  case class AddInput(a: Int, b: Int) derives Schema

  val addTool: McpToolHandler = McpTool("add")
    .description("Add two numbers")
    .handle[Any, Nothing, AddInput, String]: input =>
      ZIO.succeed(s"${input.a + input.b}")

  val noisyTool: McpToolHandler = McpTool("noisy")
    .description("Emits progress and log notifications while running")
    .handleWithContext[Any, ToolError, Chunk[ToolContent]]: ctx =>
      for
        _ <- ctx.progress(0, 100)
        _ <- ctx.log(com.jamesward.ziohttp.mcp.LogLevel.Debug, "starting")
        _ <- ctx.progress(50, 100)
        _ <- ctx.log(com.jamesward.ziohttp.mcp.LogLevel.Warning, "halfway")
        _ <- ctx.progress(100, 100)
      yield Chunk(ToolContent.text("noisy done"))

  val testResource: McpResourceHandler = McpResource("test://data", "Test Data")
    .description("A test resource")
    .mimeType("text/plain")
    .read: uri =>
      ZIO.succeed(Chunk(ResourceContents(uri = uri, mimeType = Some("text/plain"), text = Some("resource content"))))

  val testPrompt: McpPromptHandler = McpPrompt("greet")
    .description("A prompt")
    .get: _ =>
      ZIO.succeed(PromptGetResult(messages = Chunk(PromptMessage(Role.User, ToolContent.text("hi")))))

  val testServer = McpServer("neg-server", "9.9.9")
    .instructions("be helpful")
    .tool(addTool)
    .tool(noisyTool)
    .resource(testResource)
    .prompt(testPrompt)

  // --- modern request builders ---

  private def meta(version: String): Json.Obj =
    Json.Obj(McpMeta.ProtocolVersion -> Json.Str(version))

  private def modernBody(
    id: Int,
    method: String,
    extra: Chunk[(String, Json)] = Chunk.empty,
    version: String = Modern,
    metaExtra: Chunk[(String, Json)] = Chunk.empty,
  ): String =
    val fullMeta = Json.Obj(meta(version).fields ++ metaExtra)
    val params = Json.Obj(extra :+ ("_meta" -> (fullMeta: Json)))
    s"""{"jsonrpc":"2.0","id":$id,"method":"$method","params":${(params: Json).toJson}}"""

  /** POST a modern request with the required routing headers. Overrides let
    * individual header/version fields be corrupted for negative tests. */
  private def postModern(
    port: Int,
    body: String,
    method: String,
    name: Option[String] = None,
    protocolHeader: Option[String] = Some(Modern),
    methodHeader: Option[String] = None,
  ): ZIO[Client & Scope, Throwable, Response] =
    val url = URL.decode(s"http://localhost:$port/mcp").toOption.get
    var req = Request.post(url, Body.fromString(body))
      .addHeader(Header.ContentType(MediaType.application.json))
      .addHeader("accept", "application/json, text/event-stream")
      .addHeader(Negotiation.MethodHeader, methodHeader.getOrElse(method))
    protocolHeader.foreach(v => req = req.addHeader(Negotiation.ProtocolVersionHeader, v))
    name.foreach(n => req = req.addHeader(Negotiation.NameHeader, n))
    ZClient.batched(req)

  private def bodyJson(response: Response): ZIO[Any, Throwable, Json.Obj] =
    response.body.asString.flatMap(s => ZIO.fromEither(s.fromJson[Json.Obj]).mapError(e => RuntimeException(s"$e: $s")))

  private def resultOf(b: Json.Obj): Option[Json.Obj] = b.get("result").flatMap(_.asObject)
  private def errorOf(b: Json.Obj): Option[Json.Obj]  = b.get("error").flatMap(_.asObject)
  private def codeOf(b: Json.Obj): Option[Int] = errorOf(b).flatMap(_.get("code")).flatMap(_.asNumber).map(_.value.intValue)

  /** Parse the JSON payloads out of an SSE body's `data:` lines, in order. */
  private def sseDataJsons(body: String): Chunk[Json.Obj] =
    Chunk.fromIterator(
      body.linesIterator
        .filter(_.startsWith("data: "))
        .map(_.stripPrefix("data: "))
        .flatMap(_.fromJson[Json.Obj].toOption)
    )

  private def notificationsOf(events: Chunk[Json.Obj], method: String): Chunk[Json.Obj] =
    events.filter(_.get("method").flatMap(_.asString).contains(method))
      .flatMap(_.get("params").flatMap(_.asObject))

  private val unitSuite = suite("Negotiation (unit)")(

    test("isModernRequest: body _meta protocolVersion marks a request modern"):
      val params = Some(Json.Obj("_meta" -> meta(Modern)))
      assertTrue(
        Negotiation.isModernRequest("tools/list", params, None),
        !Negotiation.isModernRequest("tools/list", Some(Json.Obj()), None),
        !Negotiation.isModernRequest("tools/list", None, Some(Legacy)),
      )
    ,

    test("isModernRequest: modern-only methods are modern even without _meta"):
      assertTrue(
        Negotiation.isModernRequest("server/discover", None, None),
        Negotiation.isModernRequest("subscriptions/listen", None, None),
        Negotiation.isModernRequest("tasks/get", None, None),
      )
    ,

    test("isModernRequest: modern MCP-Protocol-Version header marks a request modern"):
      assertTrue(
        Negotiation.isModernRequest("tools/list", None, Some(Modern)),
        !Negotiation.isModernRequest("tools/list", None, Some(Legacy)),
      )
    ,

    test("resolveModern: happy path resolves to the requested version"):
      val params = Some(Json.Obj("_meta" -> meta(Modern)))
      val r = Negotiation.resolveModern("tools/list", params, Some(Modern), Some("tools/list"), None)
      assertTrue(r == Right(ProtocolVersion.V2026_07_28))
    ,

    test("resolveModern: missing Mcp-Method is accepted (lenient about absence)"):
      val params = Some(Json.Obj("_meta" -> meta(Modern)))
      val r = Negotiation.resolveModern("tools/list", params, Some(Modern), None, None)
      assertTrue(r == Right(ProtocolVersion.V2026_07_28))
    ,

    test("resolveModern: Mcp-Method disagreeing with body is a header mismatch"):
      val params = Some(Json.Obj("_meta" -> meta(Modern)))
      val r = Negotiation.resolveModern("tools/list", params, Some(Modern), Some("tools/call"), None)
      assertTrue(r.left.exists { case NegotiationError.HeaderMismatch(_) => true; case _ => false })
    ,

    test("resolveModern: header/body protocol version disagreement is a header mismatch"):
      val params = Some(Json.Obj("_meta" -> meta(Modern)))
      val r = Negotiation.resolveModern("tools/list", params, Some(Legacy), Some("tools/list"), None)
      assertTrue(r.left.exists { case NegotiationError.HeaderMismatch(_) => true; case _ => false })
    ,

    test("resolveModern: missing MCP-Protocol-Version header is accepted (body is source of truth)"):
      val params = Some(Json.Obj("_meta" -> meta(Modern)))
      val r = Negotiation.resolveModern("tools/list", params, None, Some("tools/list"), None)
      assertTrue(r == Right(ProtocolVersion.V2026_07_28))
    ,

    test("resolveModern: tools/call validates a present Mcp-Name but accepts a missing one"):
      val params = Some(Json.Obj("name" -> Json.Str("add"), "_meta" -> meta(Modern)))
      val missing = Negotiation.resolveModern("tools/call", params, Some(Modern), Some("tools/call"), None)
      val wrong   = Negotiation.resolveModern("tools/call", params, Some(Modern), Some("tools/call"), Some("subtract"))
      val ok      = Negotiation.resolveModern("tools/call", params, Some(Modern), Some("tools/call"), Some("add"))
      assertTrue(
        // absent → accepted (lenient)
        missing == Right(ProtocolVersion.V2026_07_28),
        // present but wrong → rejected
        wrong.left.exists { case NegotiationError.HeaderMismatch(_) => true; case _ => false },
        ok == Right(ProtocolVersion.V2026_07_28),
      )
    ,

    test("resolveModern: a request with no routing headers at all is accepted"):
      val params = Some(Json.Obj("_meta" -> meta(Modern)))
      val r = Negotiation.resolveModern("server/discover", params, None, None, None)
      assertTrue(r == Right(ProtocolVersion.V2026_07_28))
    ,

    test("resolveModern: resources/read Mcp-Name mirrors params.uri"):
      val params = Some(Json.Obj("uri" -> Json.Str("test://data"), "_meta" -> meta(Modern)))
      val ok = Negotiation.resolveModern("resources/read", params, Some(Modern), Some("resources/read"), Some("test://data"))
      assertTrue(ok == Right(ProtocolVersion.V2026_07_28))
    ,

    test("resolveModern: unsupported version reported with the requested string"):
      val params = Some(Json.Obj("_meta" -> meta("1900-01-01")))
      val r = Negotiation.resolveModern("tools/list", params, Some("1900-01-01"), Some("tools/list"), None)
      assertTrue(r == Left(NegotiationError.UnsupportedVersion("1900-01-01")))
    ,

    test("decodeHeaderValue: Base64 sentinel round-trips, plain passes through"):
      val encoded = "=?base64?" + java.util.Base64.getEncoder.encodeToString("Hello, 世界".getBytes("UTF-8")) + "?="
      assertTrue(
        Negotiation.decodeHeaderValue(encoded) == "Hello, 世界",
        Negotiation.decodeHeaderValue("plain") == "plain",
      )
    ,

    test("ProtocolVersion ordering and statelessness"):
      assertTrue(
        ProtocolVersion.V2026_07_28.isAtLeast(ProtocolVersion.V2025_11_25),
        !ProtocolVersion.V2025_11_25.isAtLeast(ProtocolVersion.V2026_07_28),
        ProtocolVersion.V2026_07_28.isStateless,
        !ProtocolVersion.V2025_11_25.isStateless,
        ProtocolVersion.latest == ProtocolVersion.V2026_07_28,
        ProtocolVersion.default == ProtocolVersion.V2025_11_25,
      )
    ,
  )

  private val httpSuite = suite("Negotiation (HTTP, modern era)")(

    test("server/discover advertises supported versions, capabilities, serverInfo, and cache hints"):
      for
        port <- Server.install(testServer.routes)
        resp <- postModern(port, modernBody(1, "server/discover"), "server/discover")
        b    <- bodyJson(resp)
      yield
        val r = resultOf(b)
        val supported = r.flatMap(_.get("supportedVersions")).flatMap(_.asArray)
          .map(_.flatMap(_.asString).toList).getOrElse(Nil)
        val serverInfoName = r.flatMap(_.get("_meta")).flatMap(_.asObject)
          .flatMap(_.get(McpMeta.ServerInfo)).flatMap(_.asObject)
          .flatMap(_.get("name")).flatMap(_.asString)
        val topLevelServerInfo = r.flatMap(_.get("serverInfo")).flatMap(_.asObject)
          .flatMap(_.get("name")).flatMap(_.asString)
        assertTrue(
          resp.status == Status.Ok,
          supported.contains(Modern) && supported.contains(Legacy),
          r.flatMap(_.get("resultType")).flatMap(_.asString).contains("complete"),
          r.flatMap(_.get("capabilities")).flatMap(_.asObject).isDefined,
          serverInfoName.contains("neg-server"),
          // serverInfo is also emitted at the top level for MCP SDK v2 beta.1–beta.4 clients
          topLevelServerInfo.contains("neg-server"),
          r.flatMap(_.get("instructions")).flatMap(_.asString).contains("be helpful"),
          r.flatMap(_.get("ttlMs")).isDefined,
          r.flatMap(_.get("cacheScope")).flatMap(_.asString).contains("public"),
        )
    ,

    test("modern tools/list carries resultType, serverInfo _meta, and cache hints"):
      for
        port <- Server.install(testServer.routes)
        resp <- postModern(port, modernBody(1, "tools/list"), "tools/list")
        b    <- bodyJson(resp)
      yield
        val r = resultOf(b)
        assertTrue(
          resp.status == Status.Ok,
          r.flatMap(_.get("resultType")).flatMap(_.asString).contains("complete"),
          r.flatMap(_.get("tools")).flatMap(_.asArray).exists(_.size == 2),
          r.flatMap(_.get("_meta")).flatMap(_.asObject).flatMap(_.get(McpMeta.ServerInfo)).isDefined,
          r.flatMap(_.get("ttlMs")).isDefined,
        )
    ,

    test("modern tools/call returns an augmented JSON result"):
      val extra = Chunk[(String, Json)](
        "name" -> Json.Str("add"),
        "arguments" -> Json.Obj("a" -> Json.Num(2), "b" -> Json.Num(5)),
      )
      for
        port <- Server.install(testServer.routes)
        resp <- postModern(port, modernBody(1, "tools/call", extra), "tools/call", name = Some("add"))
        b    <- bodyJson(resp)
      yield
        val r = resultOf(b)
        val text = r.flatMap(_.get("content")).flatMap(_.asArray).flatMap(_.headOption)
          .flatMap(_.asObject).flatMap(_.get("text")).flatMap(_.asString)
        assertTrue(
          resp.status == Status.Ok,
          text.contains("7"),
          r.flatMap(_.get("resultType")).flatMap(_.asString).contains("complete"),
        )
    ,

    test("modern tools/call with a progressToken streams progress notifications then the result over SSE"):
      val extra = Chunk[(String, Json)]("name" -> Json.Str("noisy"), "arguments" -> Json.Obj())
      val body  = modernBody(1, "tools/call", extra, metaExtra = Chunk(McpMeta.ProgressToken -> Json.Str("tok-1")))
      for
        port  <- Server.install(testServer.routes)
        resp  <- postModern(port, body, "tools/call", name = Some("noisy"))
        raw   <- resp.body.asString
      yield
        val events   = sseDataJsons(raw)
        val progress = notificationsOf(events, "notifications/progress")
        val values   = progress.flatMap(_.get("progress")).flatMap(_.asNumber).map(_.value.doubleValue)
        val response = events.findLast(_.get("id").isDefined)
        val result   = response.flatMap(resultOf)
        val text     = result.flatMap(_.get("content")).flatMap(_.asArray).flatMap(_.headOption)
          .flatMap(_.asObject).flatMap(_.get("text")).flatMap(_.asString)
        assertTrue(
          resp.status == Status.Ok,
          resp.rawHeader("content-type").exists(_.contains("text/event-stream")),
          progress.size == 3,
          progress.forall(_.get("progressToken").flatMap(_.asString).contains("tok-1")),
          values == Chunk(0.0, 50.0, 100.0),
          // no logLevel opt-in on the request, so log calls MUST NOT surface
          notificationsOf(events, "notifications/message").isEmpty,
          text.contains("noisy done"),
          result.flatMap(_.get("resultType")).flatMap(_.asString).contains("complete"),
          result.flatMap(_.get("_meta")).flatMap(_.asObject).flatMap(_.get(McpMeta.ServerInfo)).isDefined,
        )
    ,

    test("modern tools/call with a logLevel streams messages at or above that level"):
      val extra = Chunk[(String, Json)]("name" -> Json.Str("noisy"), "arguments" -> Json.Obj())
      val body  = modernBody(1, "tools/call", extra, metaExtra = Chunk(McpMeta.LogLevel -> Json.Str("warning")))
      for
        port  <- Server.install(testServer.routes)
        resp  <- postModern(port, body, "tools/call", name = Some("noisy"))
        raw   <- resp.body.asString
      yield
        val events   = sseDataJsons(raw)
        val messages = notificationsOf(events, "notifications/message")
        val data     = messages.flatMap(_.get("data")).flatMap(_.asString)
        assertTrue(
          resp.status == Status.Ok,
          resp.rawHeader("content-type").exists(_.contains("text/event-stream")),
          // the debug-level "starting" message is below the requested level
          data == Chunk("halfway"),
          // no progressToken on the request, so progress calls MUST NOT surface
          notificationsOf(events, "notifications/progress").isEmpty,
          events.findLast(_.get("id").isDefined).flatMap(resultOf)
            .flatMap(_.get("resultType")).flatMap(_.asString).contains("complete"),
        )
    ,

    test("modern resources/read renumbers resource-not-found to -32602"):
      val extra = Chunk[(String, Json)]("uri" -> Json.Str("test://missing"))
      for
        port <- Server.install(testServer.routes)
        resp <- postModern(port, modernBody(1, "resources/read", extra), "resources/read", name = Some("test://missing"))
        b    <- bodyJson(resp)
      yield assertTrue(codeOf(b).contains(ErrorCode.InvalidParams.code))
    ,

    test("unsupported protocol version -> 400 with -32022 and supported list"):
      for
        port <- Server.install(testServer.routes)
        resp <- postModern(port, modernBody(1, "tools/list", version = "1999-01-01"), "tools/list",
                  protocolHeader = Some("1999-01-01"))
        b    <- bodyJson(resp)
      yield
        val data = errorOf(b).flatMap(_.get("data")).flatMap(_.asObject)
        val supported = data.flatMap(_.get("supported")).flatMap(_.asArray).map(_.flatMap(_.asString).toList).getOrElse(Nil)
        assertTrue(
          resp.status == Status.BadRequest,
          codeOf(b).contains(ErrorCode.UnsupportedProtocolVersion.code),
          supported.contains(Modern),
          data.flatMap(_.get("requested")).flatMap(_.asString).contains("1999-01-01"),
        )
    ,

    test("Mcp-Method header mismatch -> 400 with -32020"):
      for
        port <- Server.install(testServer.routes)
        resp <- postModern(port, modernBody(1, "tools/list"), "tools/list", methodHeader = Some("tools/call"))
        b    <- bodyJson(resp)
      yield assertTrue(
        resp.status == Status.BadRequest,
        codeOf(b).contains(ErrorCode.HeaderMismatch.code),
      )
    ,

    test("missing Mcp-Name on modern tools/call is accepted (lenient about absent headers)"):
      val extra = Chunk[(String, Json)]("name" -> Json.Str("add"), "arguments" -> Json.Obj("a" -> Json.Num(1), "b" -> Json.Num(1)))
      for
        port <- Server.install(testServer.routes)
        resp <- postModern(port, modernBody(1, "tools/call", extra), "tools/call", name = None)
        b    <- bodyJson(resp)
      yield
        val text = resultOf(b).flatMap(_.get("content")).flatMap(_.asArray).flatMap(_.headOption)
          .flatMap(_.asObject).flatMap(_.get("text")).flatMap(_.asString)
        assertTrue(resp.status == Status.Ok, text.contains("2"))
    ,

    test("present-but-wrong Mcp-Name on modern tools/call -> 400 with -32020"):
      val extra = Chunk[(String, Json)]("name" -> Json.Str("add"), "arguments" -> Json.Obj("a" -> Json.Num(1), "b" -> Json.Num(1)))
      for
        port <- Server.install(testServer.routes)
        resp <- postModern(port, modernBody(1, "tools/call", extra), "tools/call", name = Some("wrong"))
        b    <- bodyJson(resp)
      yield assertTrue(
        resp.status == Status.BadRequest,
        codeOf(b).contains(ErrorCode.HeaderMismatch.code),
      )
    ,

    test("unknown modern method -> 404 with -32601"):
      for
        port <- Server.install(testServer.routes)
        resp <- postModern(port, modernBody(1, "does/notexist"), "does/notexist")
        b    <- bodyJson(resp)
      yield assertTrue(
        resp.status == Status.NotFound,
        codeOf(b).contains(ErrorCode.MethodNotFound.code),
      )
    ,

    test("ping is method-not-found under the modern era"):
      for
        port <- Server.install(testServer.routes)
        resp <- postModern(port, modernBody(1, "ping"), "ping")
        b    <- bodyJson(resp)
      yield assertTrue(
        resp.status == Status.NotFound,
        codeOf(b).contains(ErrorCode.MethodNotFound.code),
      )
    ,

    test("legacy initialize result carries NO modern envelope fields"):
      val url = (port: Int) => URL.decode(s"http://localhost:$port/mcp").toOption.get
      val initBody =
        s"""{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"$Legacy","capabilities":{},"clientInfo":{"name":"t","version":"1"}}}"""
      for
        port <- Server.install(testServer.routes)
        resp <- ZClient.batched(Request.post(url(port), Body.fromString(initBody))
                  .addHeader(Header.ContentType(MediaType.application.json)))
        b    <- bodyJson(resp)
      yield
        val r = resultOf(b)
        assertTrue(
          resp.status == Status.Ok,
          resp.rawHeader("mcp-session-id").isDefined,
          r.flatMap(_.get("protocolVersion")).flatMap(_.asString).contains(Legacy),
          r.flatMap(_.get("resultType")).isEmpty,
          r.flatMap(_.get("ttlMs")).isEmpty,
        )
    ,

  ).provide(Server.defaultWith(_.onAnyOpenPort), Client.default, Scope.default, McpServer.State.default) @@
    withLiveClock @@ timeout(1.minute) @@ sequential

  // --- Older (pre-2025-11-25) legacy version negotiation ---

  /** POST a legacy request (no modern `_meta`), optionally with a session id and
    * an `MCP-Protocol-Version` header, the way an older client would. */
  private def postLegacy(
    port: Int,
    body: String,
    sessionId: Option[String] = None,
    protocolHeader: Option[String] = None,
  ): ZIO[Client & Scope, Throwable, Response] =
    val url = URL.decode(s"http://localhost:$port/mcp").toOption.get
    var req = Request.post(url, Body.fromString(body))
      .addHeader(Header.ContentType(MediaType.application.json))
      .addHeader("accept", "application/json, text/event-stream")
    sessionId.foreach(s => req = req.addHeader("mcp-session-id", s))
    protocolHeader.foreach(v => req = req.addHeader("mcp-protocol-version", v))
    ZClient.batched(req)

  private def initBody(id: Int, version: String): String =
    s"""{"jsonrpc":"2.0","id":$id,"method":"initialize","params":{"protocolVersion":"$version","capabilities":{},"clientInfo":{"name":"old-client","version":"1"}}}"""

  private val legacySuite = suite("Legacy version negotiation (older clients)")(

    test("initialize echoes a supported older version (2025-06-18) and no modern envelope"):
      for
        port <- Server.install(testServer.routes)
        resp <- postLegacy(port, initBody(1, "2025-06-18"))
        b    <- bodyJson(resp)
      yield
        val r = resultOf(b)
        assertTrue(
          resp.status == Status.Ok,
          resp.rawHeader("mcp-session-id").isDefined,
          r.flatMap(_.get("protocolVersion")).flatMap(_.asString).contains("2025-06-18"),
          r.flatMap(_.get("resultType")).isEmpty,
        )
    ,

    test("initialize echoes the 2025-03-26 revision"):
      for
        port <- Server.install(testServer.routes)
        resp <- postLegacy(port, initBody(1, "2025-03-26"))
        b    <- bodyJson(resp)
      yield assertTrue(
        resultOf(b).flatMap(_.get("protocolVersion")).flatMap(_.asString).contains("2025-03-26"),
      )
    ,

    test("initialize with an unknown/older version falls back to the newest legacy revision"):
      for
        port <- Server.install(testServer.routes)
        resp <- postLegacy(port, initBody(1, "2024-11-05"))
        b    <- bodyJson(resp)
      yield assertTrue(
        resultOf(b).flatMap(_.get("protocolVersion")).flatMap(_.asString).contains(ProtocolVersion.latestLegacy.wire),
      )
    ,

    test("an older client completes an initialize + session + follow-up round trip"):
      for
        port    <- Server.install(testServer.routes)
        initR   <- postLegacy(port, initBody(1, "2025-06-18"))
        session  = initR.rawHeader("mcp-session-id").getOrElse("")
        // follow-up requests carry the negotiated older version header + session
        listR   <- postLegacy(port, """{"jsonrpc":"2.0","id":2,"method":"tools/list"}""",
                     sessionId = Some(session), protocolHeader = Some("2025-06-18"))
        listB   <- bodyJson(listR)
        pingR   <- postLegacy(port, """{"jsonrpc":"2.0","id":3,"method":"ping"}""",
                     sessionId = Some(session), protocolHeader = Some("2025-06-18"))
        pingB   <- bodyJson(pingR)
      yield assertTrue(
        initR.status == Status.Ok,
        session.nonEmpty,
        listR.status == Status.Ok,
        listB.get("result").flatMap(_.asObject).flatMap(_.get("tools")).flatMap(_.asArray).exists(_.size == 2),
        // legacy follow-ups are NOT wrapped in the modern envelope
        listB.get("result").flatMap(_.asObject).flatMap(_.get("resultType")).isEmpty,
        // `ping` still exists in the legacy era and round-trips over the session
        pingR.status == Status.Ok,
        pingB.get("result").isDefined,
      )
    ,

  ).provide(Server.defaultWith(_.onAnyOpenPort), Client.default, Scope.default, McpServer.State.default) @@
    withLiveClock @@ timeout(1.minute) @@ sequential

  // --- MRTR (Multi Round-Trip Requests) ---

  val sampleTool: McpToolHandler = McpTool("summarize")
    .description("Summarizes via sampling")
    .handleWithContext[Any, ToolError, Chunk[ToolContent]]: ctx =>
      ctx.sample("Summarize this", 100).map: result =>
        val text = result.content match
          case ToolContent.Text(t, _) => t
          case _                       => ""
        Chunk(ToolContent.text(s"summary: $text"))

  val elicitTool: McpToolHandler = McpTool("ask_name")
    .description("Asks the user for a name")
    .handleWithContext[Any, ToolError, Chunk[ToolContent]]: ctx =>
      ctx.elicit("What is your name?", Json.Obj("type" -> Json.Str("object"))).map: result =>
        val name = result.content.flatMap(_.get("name")).flatMap(_.asString).getOrElse("?")
        Chunk(ToolContent.text(s"hello $name (${result.action})"))

  /** Asks for an elicitation, a sampling and the client's roots in one round
    * trip — the README's `onboard` example. */
  val batchTool: McpToolHandler = McpTool("onboard")
    .description("Collects what it needs to onboard someone")
    .handleWithContext[Any, ToolError, Chunk[ToolContent]]: ctx =>
      ctx.inputs(
        InputSpec.elicit("user_name", "What is your name?", Json.Obj("type" -> Json.Str("object"))),
        InputSpec.sample("greeting", "Generate a greeting", 50),
        InputSpec.listRoots("client_roots"),
      ).map: results =>
        val name = results.elicitation("user_name").content.flatMap(_.get("name")).flatMap(_.asString).getOrElse("?")
        val greeting = results.sampling("greeting").content match
          case ToolContent.Text(t, _) => t
          case _                       => ""
        Chunk(ToolContent.text(s"$greeting $name from ${results.roots("client_roots").map(_.uri).mkString(",")}"))

  /** Two rounds of input, resumed from the opaque state rather than by replay. */
  val statefulTool: McpToolHandler = McpTool("two_step")
    .description("Collects two answers over two rounds")
    .handleWithContext[Any, ToolError, Chunk[ToolContent]]: ctx =>
      val askColour = ctx.elicit("step2", "Colour?", Json.Obj("type" -> Json.Str("object")))
      ctx.requestState match
        case Some("round-2") =>
          askColour.map: result =>
            val colour = result.content.flatMap(_.get("color")).flatMap(_.asString).getOrElse("?")
            Chunk(ToolContent.text(s"done: $colour"))
        case Some("round-1") =>
          ctx.setRequestState("round-2") *> askColour.map(_ => Chunk(ToolContent.text("unreachable")))
        case _ =>
          ctx.setRequestState("round-1") *>
            ctx.elicit("step1", "Name?", Json.Obj("type" -> Json.Str("object")))
              .map(_ => Chunk(ToolContent.text("unreachable")))

  /** Only asks for what the client said it can answer. */
  val capabilityTool: McpToolHandler = McpTool("ask_what_you_can")
    .description("Respects the client's declared capabilities")
    .handleWithContext[Any, ToolError, Chunk[ToolContent]]: ctx =>
      val specs = Chunk(
        Option.when(ctx.clientSupports("elicitation"))(InputSpec.elicit("who", "Who?", Json.Obj("type" -> Json.Str("object")))),
        Option.when(ctx.clientSupports("sampling"))(InputSpec.sample("greeting", "Greet", 10)),
      ).flatten
      ctx.inputs(specs*).map(_ => Chunk(ToolContent.text("asked")))

  val mrtrPrompt: McpPromptHandler = McpPrompt("contextual")
    .description("Elicits its context before rendering")
    .getWithContext: (_, ctx) =>
      ctx.elicit("user_context", "What context?", Json.Obj("type" -> Json.Str("object"))).map: result =>
        val context = result.content.flatMap(_.get("context")).flatMap(_.asString).getOrElse("none")
        PromptGetResult(messages = Chunk(PromptMessage(role = Role.User, content = ToolContent.text(s"Context: $context"))))

  val mrtrServer = McpServer("mrtr-server", "1.0.0")
    .tool(sampleTool)
    .tool(elicitTool)
    .tool(batchTool)
    .tool(statefulTool)
    .tool(capabilityTool)
    .prompt(mrtrPrompt)

  /** The `inputRequests` of an `input_required` result: an object keyed by
    * correlation id, whose values are `{method, params}`. */
  private def inputRequestsOf(b: Json.Obj): Chunk[(String, Json)] =
    resultOf(b).flatMap(_.get("inputRequests")).flatMap(_.asObject).fold(Chunk.empty)(_.fields)

  private def methodOfRequest(b: Json.Obj, key: String): Option[String] =
    inputRequestsOf(b).collectFirst { case (k, v) if k == key => v }
      .flatMap(_.asObject).flatMap(_.get("method")).flatMap(_.asString)

  private def stateOf(b: Json.Obj): Option[String] =
    resultOf(b).flatMap(_.get("requestState")).flatMap(_.asString)

  private def textOf(b: Json.Obj): Option[String] =
    resultOf(b).flatMap(_.get("content")).flatMap(_.asArray).flatMap(_.headOption)
      .flatMap(_.asObject).flatMap(_.get("text")).flatMap(_.asString)

  private def isInputRequired(b: Json.Obj): Boolean =
    resultOf(b).flatMap(_.get("resultType")).flatMap(_.asString).contains("input_required")

  private def sampledJson(text: String): Json = Json.Obj(
    "role" -> Json.Str("assistant"),
    "model" -> Json.Str("test-model"),
    "content" -> Json.Obj("type" -> Json.Str("text"), "text" -> Json.Str(text)),
  )

  private def elicitedJson(field: String, value: String): Json = Json.Obj(
    "action" -> Json.Str("accept"),
    "content" -> Json.Obj(field -> Json.Str(value)),
  )

  private val rootsJson: Json = Json.Obj(
    "roots" -> Json.Arr(Chunk(Json.Obj("uri" -> Json.Str("file:///test/root"), "name" -> Json.Str("Test Root")))),
  )

  private def retryBody(
    id: Int,
    tool: String,
    responses: Chunk[(String, Json)],
    state: Option[String] = None,
  ): String =
    modernBody(id, "tools/call", Chunk[(String, Json)](
      "name" -> Json.Str(tool),
      "arguments" -> Json.Obj(),
      "inputResponses" -> Json.Obj(responses),
    ) ++ state.fold(Chunk.empty[(String, Json)])(s => Chunk("requestState" -> Json.Str(s))))

  private val mrtrSuite = suite("MRTR (modern server-to-client input)")(

    test("sampling tool first returns input_required, then completes on retry"):
      val call1 = modernBody(1, "tools/call", Chunk("name" -> Json.Str("summarize"), "arguments" -> Json.Obj()))
      for
        port <- Server.install(mrtrServer.routes)
        r1   <- postModern(port, call1, "tools/call", name = Some("summarize"))
        b1   <- bodyJson(r1)
        // The requests are keyed by correlation id; the retry echoes those keys.
        key   = inputRequestsOf(b1).headOption.map(_._1).getOrElse("")
        call2 = retryBody(2, "summarize", Chunk(key -> sampledJson("it is short")))
        r2   <- postModern(port, call2, "tools/call", name = Some("summarize"))
        b2   <- bodyJson(r2)
      yield assertTrue(
        r1.status == Status.Ok,
        isInputRequired(b1),
        key == "input-0",
        methodOfRequest(b1, key).contains("sampling/createMessage"),
        resultOf(b2).flatMap(_.get("resultType")).flatMap(_.asString).contains("complete"),
        textOf(b2).contains("summary: it is short"),
      )
    ,

    test("elicitation tool round-trips via input_required"):
      val call1 = modernBody(1, "tools/call", Chunk("name" -> Json.Str("ask_name"), "arguments" -> Json.Obj()))
      for
        port <- Server.install(mrtrServer.routes)
        r1   <- postModern(port, call1, "tools/call", name = Some("ask_name"))
        b1   <- bodyJson(r1)
        key   = inputRequestsOf(b1).headOption.map(_._1).getOrElse("")
        call2 = retryBody(2, "ask_name", Chunk(key -> elicitedJson("name", "Ada")))
        r2   <- postModern(port, call2, "tools/call", name = Some("ask_name"))
        b2   <- bodyJson(r2)
      yield assertTrue(
        methodOfRequest(b1, key).contains("elicitation/create"),
        textOf(b2).contains("hello Ada (accept)"),
      )
    ,

    test("several inputs of different kinds travel in one round trip"):
      val call1 = modernBody(1, "tools/call", Chunk("name" -> Json.Str("onboard"), "arguments" -> Json.Obj()))
      for
        port <- Server.install(mrtrServer.routes)
        r1   <- postModern(port, call1, "tools/call", name = Some("onboard"))
        b1   <- bodyJson(r1)
        call2 = retryBody(2, "onboard", Chunk(
                  "user_name" -> elicitedJson("name", "Ada"),
                  "greeting" -> sampledJson("Hello"),
                  "client_roots" -> rootsJson,
                ))
        r2   <- postModern(port, call2, "tools/call", name = Some("onboard"))
        b2   <- bodyJson(r2)
      yield assertTrue(
        isInputRequired(b1),
        inputRequestsOf(b1).map(_._1) == Chunk("user_name", "greeting", "client_roots"),
        methodOfRequest(b1, "user_name").contains("elicitation/create"),
        methodOfRequest(b1, "greeting").contains("sampling/createMessage"),
        methodOfRequest(b1, "client_roots").contains("roots/list"),
        textOf(b2).contains("Hello Ada from file:///test/root"),
      )
    ,

    test("requestState carries the handler across rounds and is signed"):
      val call1 = modernBody(1, "tools/call", Chunk("name" -> Json.Str("two_step"), "arguments" -> Json.Obj()))
      for
        port  <- Server.install(mrtrServer.routes)
        r1    <- postModern(port, call1, "tools/call", name = Some("two_step"))
        b1    <- bodyJson(r1)
        state1 = stateOf(b1).getOrElse("")
        call2  = retryBody(2, "two_step", Chunk("step1" -> elicitedJson("name", "Ada")), Some(state1))
        r2    <- postModern(port, call2, "tools/call", name = Some("two_step"))
        b2    <- bodyJson(r2)
        state2 = stateOf(b2).getOrElse("")
        call3  = retryBody(3, "two_step", Chunk("step2" -> elicitedJson("color", "blue")), Some(state2))
        r3    <- postModern(port, call3, "tools/call", name = Some("two_step"))
        b3    <- bodyJson(r3)
      yield assertTrue(
        isInputRequired(b1),
        // opaque and signed on the wire, never the handler's plain string
        state1.nonEmpty,
        state1 != "round-1",
        methodOfRequest(b1, "step1").contains("elicitation/create"),
        isInputRequired(b2),
        state2.nonEmpty,
        state2 != state1,
        methodOfRequest(b2, "step2").contains("elicitation/create"),
        textOf(b3).contains("done: blue"),
      )
    ,

    test("a tampered requestState is rejected"):
      val call1 = modernBody(1, "tools/call", Chunk("name" -> Json.Str("two_step"), "arguments" -> Json.Obj()))
      for
        port  <- Server.install(mrtrServer.routes)
        r1    <- postModern(port, call1, "tools/call", name = Some("two_step"))
        b1    <- bodyJson(r1)
        state  = stateOf(b1).getOrElse("")
        call2  = retryBody(2, "two_step", Chunk("step1" -> elicitedJson("name", "Ada")), Some(s"$state-TAMPERED"))
        r2    <- postModern(port, call2, "tools/call", name = Some("two_step"))
        b2    <- bodyJson(r2)
      yield assertTrue(
        codeOf(b2).contains(ErrorCode.InvalidParams.code),
        errorOf(b2).flatMap(_.get("message")).flatMap(_.asString).exists(_.contains("integrity")),
      )
    ,

    test("a malformed inputResponses is a protocol error, not another round trip"):
      val call = modernBody(1, "tools/call", Chunk(
        "name" -> Json.Str("ask_name"),
        "arguments" -> Json.Obj(),
        "inputResponses" -> Json.Obj("input-0" -> Json.Num(12345)),
      ))
      for
        port <- Server.install(mrtrServer.routes)
        r    <- postModern(port, call, "tools/call", name = Some("ask_name"))
        b    <- bodyJson(r)
      yield assertTrue(codeOf(b).contains(ErrorCode.InvalidParams.code))
    ,

    test("an unanswered id is re-requested rather than errored"):
      // The client answered a key the server never asked for.
      val call = retryBody(1, "ask_name", Chunk("wrong_key" -> elicitedJson("name", "Ada")))
      for
        port <- Server.install(mrtrServer.routes)
        r    <- postModern(port, call, "tools/call", name = Some("ask_name"))
        b    <- bodyJson(r)
      yield assertTrue(isInputRequired(b), methodOfRequest(b, "input-0").contains("elicitation/create"))
    ,

    test("extra unrecognized inputResponses keys are ignored"):
      val call = retryBody(1, "ask_name", Chunk(
        "input-0" -> elicitedJson("name", "Ada"),
        "unknown_extra_key" -> elicitedJson("foo", "bar"),
      ))
      for
        port <- Server.install(mrtrServer.routes)
        r    <- postModern(port, call, "tools/call", name = Some("ask_name"))
        b    <- bodyJson(r)
      yield assertTrue(textOf(b).contains("hello Ada (accept)"))
    ,

    test("only capabilities the client declared are asked for"):
      val call = modernBody(1, "tools/call",
        Chunk("name" -> Json.Str("ask_what_you_can"), "arguments" -> Json.Obj()),
        metaExtra = Chunk(McpMeta.ClientCapabilities -> Json.Obj("sampling" -> Json.Obj())),
      )
      for
        port <- Server.install(mrtrServer.routes)
        r    <- postModern(port, call, "tools/call", name = Some("ask_what_you_can"))
        b    <- bodyJson(r)
      yield assertTrue(
        isInputRequired(b),
        inputRequestsOf(b).map(_._1) == Chunk("greeting"),
        methodOfRequest(b, "greeting").contains("sampling/createMessage"),
      )
    ,

    test("prompts/get can ask for input and complete on retry"):
      val call1 = modernBody(1, "prompts/get", Chunk("name" -> Json.Str("contextual")))
      for
        port <- Server.install(mrtrServer.routes)
        r1   <- postModern(port, call1, "prompts/get")
        b1   <- bodyJson(r1)
        call2 = modernBody(2, "prompts/get", Chunk(
                  "name" -> Json.Str("contextual"),
                  "inputResponses" -> Json.Obj("user_context" -> elicitedJson("context", "test context")),
                ))
        r2   <- postModern(port, call2, "prompts/get")
        b2   <- bodyJson(r2)
      yield
        val text = resultOf(b2).flatMap(_.get("messages")).flatMap(_.asArray).flatMap(_.headOption)
          .flatMap(_.asObject).flatMap(_.get("content")).flatMap(_.asObject)
          .flatMap(_.get("text")).flatMap(_.asString)
        assertTrue(
          isInputRequired(b1),
          methodOfRequest(b1, "user_context").contains("elicitation/create"),
          text.contains("Context: test context"),
        )
    ,

    test("tools/list never answers with input_required"):
      for
        port <- Server.install(mrtrServer.routes)
        r    <- postModern(port, modernBody(1, "tools/list"), "tools/list")
        b    <- bodyJson(r)
      yield assertTrue(!isInputRequired(b), resultOf(b).flatMap(_.get("tools")).isDefined)
    ,

  ).provide(Server.defaultWith(_.onAnyOpenPort), Client.default, Scope.default, McpServer.State.default) @@
    withLiveClock @@ timeout(1.minute) @@ sequential

  override def spec = suite("NegotiationSpec")(unitSuite, httpSuite, legacySuite, mrtrSuite)
