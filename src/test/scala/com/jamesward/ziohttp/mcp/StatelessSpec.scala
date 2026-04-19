package com.jamesward.ziohttp.mcp

import zio.*
import zio.http.*
import zio.json.*
import zio.json.ast.Json
import zio.schema.*
import zio.test.*
import zio.test.TestAspect.*

given CanEqual[Status, Status] = CanEqual.derived
given CanEqual[MediaType, MediaType] = CanEqual.derived

object StatelessSpec extends ZIOSpecDefault:

  case class AddInput(a: Int, b: Int) derives Schema

  val addTool: McpToolHandler = McpTool("add")
    .description("Add two numbers")
    .handle[Any, Nothing, AddInput, String]: input =>
      ZIO.succeed(s"${input.a + input.b}")

  val failTool: McpToolHandler = McpTool("fail")
    .description("Always fails")
    .handle[Any, ToolError, String]:
      ZIO.fail(ToolError("boom"))

  val testResource: McpResourceHandler = McpResource("test://data", "Test Data")
    .description("A test resource")
    .mimeType("text/plain")
    .read: uri =>
      ZIO.succeed(Chunk(ResourceContents(uri = uri, mimeType = Some("text/plain"), text = Some("resource content"))))

  val testPrompt: McpPromptHandler = McpPrompt("test_prompt")
    .description("A test prompt")
    .argument("name", "A name argument")
    .get: args =>
      ZIO.succeed(PromptGetResult(
        messages = Chunk(PromptMessage(
          role = Role.User,
          content = ToolContent.text(s"Hello, ${args.getOrElse("name", "world")}!"),
        )),
      ))

  val testServer = McpServer("test-server", "0.1.0")
    .tool(addTool)
    .tool(failTool)
    .resource(testResource)
    .prompt(testPrompt)

  private def jsonRpcRequest(id: Int, method: String, params: Option[Json.Obj] = None): String =
    val paramsField = params.fold("")(p => s""","params":${p.toJson}""")
    s"""{"jsonrpc":"2.0","id":$id,"method":"$method"$paramsField}"""

  private def jsonRpcNotification(method: String, params: Option[Json.Obj] = None): String =
    val paramsField = params.fold("")(p => s""","params":${p.toJson}""")
    s"""{"jsonrpc":"2.0","method":"$method"$paramsField}"""

  private def post(port: Int, body: String): ZIO[Client & Scope, Throwable, Response] =
    val url = URL.decode(s"http://localhost:$port/mcp").toOption.get
    val request = Request.post(url, Body.fromString(body))
      .addHeader(Header.ContentType(MediaType.application.json))
    ZClient.request(request)

  private def postWithSession(port: Int, body: String, sessionId: String): ZIO[Client & Scope, Throwable, Response] =
    val url = URL.decode(s"http://localhost:$port/mcp").toOption.get
    val request = Request.post(url, Body.fromString(body))
      .addHeader(Header.ContentType(MediaType.application.json))
      .addHeader("mcp-session-id", sessionId)
    ZClient.request(request)

  private def get(port: Int): ZIO[Client & Scope, Throwable, Response] =
    val url = URL.decode(s"http://localhost:$port/mcp").toOption.get
    ZClient.request(Request.get(url))

  private def delete(port: Int): ZIO[Client & Scope, Throwable, Response] =
    val url = URL.decode(s"http://localhost:$port/mcp").toOption.get
    ZClient.request(Request.delete(url))

  private def bodyJson(response: Response): ZIO[Any, Throwable, Json.Obj] =
    response.body.asString.flatMap: s =>
      ZIO.fromEither(s.fromJson[Json.Obj]).mapError(e => RuntimeException(e))

  override def spec =
    suite("Stateless MCP Server")(

      test("GET returns 405 Method Not Allowed"):
        for
          port     <- Server.install(testServer.statelessRoutes)
          response <- get(port)
        yield assertTrue(response.status == Status.MethodNotAllowed)
      ,

      test("DELETE returns 405 Method Not Allowed"):
        for
          port     <- Server.install(testServer.statelessRoutes)
          response <- delete(port)
        yield assertTrue(response.status == Status.MethodNotAllowed)
      ,

      test("initialize does not return mcp-session-id header"):
        for
          port     <- Server.install(testServer.statelessRoutes)
          response <- post(port, jsonRpcRequest(1, "initialize", Some(Json.Obj(Chunk(
            "protocolVersion" -> Json.Str(McpProtocol.Version),
            "capabilities" -> Json.Obj(),
            "clientInfo" -> Json.Obj(Chunk("name" -> Json.Str("test"), "version" -> Json.Str("1.0"))),
          )))))
          body     <- bodyJson(response)
        yield assertTrue(
          response.status == Status.Ok,
          response.rawHeader("mcp-session-id").isEmpty,
          body.get("result").flatMap(_.asObject).flatMap(_.get("protocolVersion")).flatMap(_.asString).contains(McpProtocol.Version),
        )
      ,

      test("ping works without session"):
        for
          port     <- Server.install(testServer.statelessRoutes)
          response <- post(port, jsonRpcRequest(1, "ping"))
          body     <- bodyJson(response)
        yield assertTrue(
          response.status == Status.Ok,
          body.get("result").isDefined,
        )
      ,

      test("tools/list works without session"):
        for
          port     <- Server.install(testServer.statelessRoutes)
          response <- post(port, jsonRpcRequest(1, "tools/list"))
          body     <- bodyJson(response)
        yield
          val tools = body.get("result").flatMap(_.asObject).flatMap(_.get("tools")).flatMap(_.asArray)
          assertTrue(
            response.status == Status.Ok,
            tools.exists(_.size == 2),
          )
      ,

      test("tools/call returns JSON response (not SSE)"):
        for
          port     <- Server.install(testServer.statelessRoutes)
          response <- post(port, jsonRpcRequest(1, "tools/call", Some(Json.Obj(Chunk(
            "name" -> Json.Str("add"),
            "arguments" -> Json.Obj(Chunk("a" -> Json.Num(3), "b" -> Json.Num(4))),
          )))))
          body     <- bodyJson(response)
        yield
          val contentType = response.header(Header.ContentType)
          val resultText = body.get("result").flatMap(_.asObject)
            .flatMap(_.get("content")).flatMap(_.asArray)
            .flatMap(_.headOption).flatMap(_.asObject)
            .flatMap(_.get("text")).flatMap(_.asString)
          assertTrue(
            response.status == Status.Ok,
            contentType.exists(_.mediaType == MediaType.application.json),
            resultText.contains("7"),
          )
      ,

      test("tools/call error returns JSON response"):
        for
          port     <- Server.install(testServer.statelessRoutes)
          response <- post(port, jsonRpcRequest(1, "tools/call", Some(Json.Obj(Chunk(
            "name" -> Json.Str("fail"),
          )))))
          body     <- bodyJson(response)
        yield
          val isError = body.get("result").flatMap(_.asObject)
            .flatMap(_.get("isError")).flatMap(_.asBoolean)
          assertTrue(
            response.status == Status.Ok,
            isError.contains(true),
          )
      ,

      test("resources/list works without session"):
        for
          port     <- Server.install(testServer.statelessRoutes)
          response <- post(port, jsonRpcRequest(1, "resources/list"))
          body     <- bodyJson(response)
        yield
          val resources = body.get("result").flatMap(_.asObject).flatMap(_.get("resources")).flatMap(_.asArray)
          assertTrue(
            response.status == Status.Ok,
            resources.exists(_.size == 1),
          )
      ,

      test("resources/read works without session"):
        for
          port     <- Server.install(testServer.statelessRoutes)
          response <- post(port, jsonRpcRequest(1, "resources/read", Some(Json.Obj(Chunk(
            "uri" -> Json.Str("test://data"),
          )))))
          body     <- bodyJson(response)
        yield
          val text = body.get("result").flatMap(_.asObject)
            .flatMap(_.get("contents")).flatMap(_.asArray)
            .flatMap(_.headOption).flatMap(_.asObject)
            .flatMap(_.get("text")).flatMap(_.asString)
          assertTrue(
            response.status == Status.Ok,
            text.contains("resource content"),
          )
      ,

      test("prompts/list works without session"):
        for
          port     <- Server.install(testServer.statelessRoutes)
          response <- post(port, jsonRpcRequest(1, "prompts/list"))
          body     <- bodyJson(response)
        yield
          val prompts = body.get("result").flatMap(_.asObject).flatMap(_.get("prompts")).flatMap(_.asArray)
          assertTrue(
            response.status == Status.Ok,
            prompts.exists(_.size == 1),
          )
      ,

      test("prompts/get works without session"):
        for
          port     <- Server.install(testServer.statelessRoutes)
          response <- post(port, jsonRpcRequest(1, "prompts/get", Some(Json.Obj(Chunk(
            "name" -> Json.Str("test_prompt"),
            "arguments" -> Json.Obj(Chunk("name" -> Json.Str("Alice"))),
          )))))
          body     <- bodyJson(response)
        yield
          val text = body.get("result").flatMap(_.asObject)
            .flatMap(_.get("messages")).flatMap(_.asArray)
            .flatMap(_.headOption).flatMap(_.asObject)
            .flatMap(_.get("content")).flatMap(_.asObject)
            .flatMap(_.get("text")).flatMap(_.asString)
          assertTrue(
            response.status == Status.Ok,
            text.contains("Hello, Alice!"),
          )
      ,

      test("notifications return 202 Accepted"):
        for
          port     <- Server.install(testServer.statelessRoutes)
          response <- post(port, jsonRpcNotification("notifications/initialized"))
        yield assertTrue(response.status == Status.Accepted)
      ,

      test("unknown method returns error"):
        for
          port     <- Server.install(testServer.statelessRoutes)
          response <- post(port, jsonRpcRequest(1, "nonexistent/method"))
          body     <- bodyJson(response)
        yield
          val errorCode = body.get("error").flatMap(_.asObject)
            .flatMap(_.get("code")).flatMap(_.asNumber).map(_.value.intValue)
          assertTrue(
            errorCode.contains(ErrorCode.MethodNotFound.code),
          )
      ,

    ).provide(Server.defaultWith(_.onAnyOpenPort), Client.default, Scope.default) @@
      withLiveClock @@
      timeout(1.minute) @@
      sequential
