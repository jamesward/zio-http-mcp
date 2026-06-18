package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.client.*
import zio.*
import zio.http.*
import zio.json.ast.Json
import zio.schema.*
import zio.test.*
import zio.test.TestAspect.*

/**
 * Exercises our [[com.jamesward.ziohttp.mcp.client.McpClient]] against our own
 * [[McpServer]] over a real loopback HTTP connection.
 *
 *   - `statelessRoutes` returns tool calls as plain `application/json`.
 *   - `routes` returns tool calls as a `text/event-stream` (SSE) and uses session
 *     tracking, so it covers the SSE-parsing + `Mcp-Session-Id` paths of the client.
 */
object McpClientSpec extends ZIOSpecDefault:

  case class AddInput(a: Int, b: Int) derives Schema
  case class AddOutput(result: Int) derives Schema

  val addTool: McpToolHandler = McpTool("add")
    .description("Add two numbers")
    .handle[Any, Nothing, AddInput, AddOutput]: input =>
      ZIO.succeed(AddOutput(input.a + input.b))

  val greetTool: McpToolHandler = McpTool("greet")
    .description("Greets someone")
    .handle[Any, Nothing, AddInput, String]: input =>
      ZIO.succeed(s"sum is ${input.a + input.b}")

  val boomTool: McpToolHandler = McpTool("boom")
    .description("Always fails")
    .handle[Any, ToolError, AddInput, AddOutput]: _ =>
      ZIO.fail(ToolError("kaboom"))

  val configResource: McpResourceHandler = McpResource("app://config", "App Config")
    .description("Application configuration")
    .mimeType("application/json")
    .read: uri =>
      ZIO.succeed(Chunk(ResourceContents(uri = uri, mimeType = Some("application/json"), text = Some("""{"debug":false}"""))))

  val testServer = McpServer("test-server", "0.1.0")
    .tool(addTool)
    .tool(greetTool)
    .tool(boomTool)
    .resource(configResource)

  private def addArgs(a: Int, b: Int): Json.Obj =
    Json.Obj(Chunk("a" -> Json.Num(a), "b" -> Json.Num(b)))

  override def spec =
    suite("McpClient (vs our own server)")(
      test("stateless: initialize, list tools, call tool (plain JSON)"):
        ZIO.scoped:
          for
            port   <- Server.install(testServer.statelessRoutes)
            client <- McpClient.connect(s"http://localhost:$port/mcp")
            tools  <- client.listTools
            result <- client.callTool("add", addArgs(5, 3))
          yield
            val names = tools.map(_.name.value).toSet
            assertTrue(
              client.serverInfo.name == "test-server",
              client.protocolVersion == McpProtocol.Version,
              names == Set("add", "greet", "boom"),
              result.isError.forall(!_),
              result.structuredContent.contains(Json.Obj(Chunk("result" -> Json.Num(8)))),
            )
      ,
      test("stateful: SSE tool call + session handshake"):
        ZIO.scoped:
          for
            port   <- Server.install(testServer.routes)
            client <- McpClient.connect(s"http://localhost:$port/mcp")
            _      <- client.ping
            result <- client.callTool("greet", addArgs(2, 2))
          yield
            val text = result.content.collectFirst { case ToolContent.Text(t, _) => t }
            assertTrue(
              result.isError.forall(!_),
              text.contains("sum is 4"),
            )
      ,
      test("resources: list and read"):
        ZIO.scoped:
          for
            port      <- Server.install(testServer.statelessRoutes)
            client    <- McpClient.connect(s"http://localhost:$port/mcp")
            resources <- client.listResources
            contents  <- client.readResource("app://config")
          yield assertTrue(
            resources.map(_.uri).contains("app://config"),
            contents.headOption.flatMap(_.text).contains("""{"debug":false}"""),
          )
      ,
      test("JSON-RPC error surfaces as McpClientError.JsonRpc"):
        ZIO.scoped:
          for
            port   <- Server.install(testServer.statelessRoutes)
            client <- McpClient.connect(s"http://localhost:$port/mcp")
            err    <- client.callTool("does-not-exist").flip
          yield assertTrue(
            err.isInstanceOf[McpClientError.JsonRpc],
            err.getMessage.contains("does-not-exist"),
          )
      ,
      test("typed input + typed output round-trip (deserialization is the validation)"):
        ZIO.scoped:
          for
            port   <- Server.install(testServer.statelessRoutes)
            client <- McpClient.connect(s"http://localhost:$port/mcp")
            r1     <- client.callTool("add", AddInput(5, 3))
            r2     <- client.callToolAs[AddOutput]("add", addArgs(5, 3))
            r3     <- client.callToolAs[AddInput, AddOutput]("add", AddInput(4, 4))
          yield assertTrue(
            r1.structuredContent.contains(Json.Obj(Chunk("result" -> Json.Num(8)))),
            r2.result == 8,
            r3.result == 8,
          )
      ,
      test("callToolAs surfaces a tool error as McpClientError.ToolFailed"):
        ZIO.scoped:
          for
            port   <- Server.install(testServer.statelessRoutes)
            client <- McpClient.connect(s"http://localhost:$port/mcp")
            err    <- client.callToolAs[AddOutput]("boom", addArgs(1, 1)).flip
          yield assertTrue(
            err.isInstanceOf[McpClientError.ToolFailed],
            err.getMessage.contains("kaboom"),
          )
      ,
      test("callToolAs fails with Decode when the result doesn't conform to the type"):
        ZIO.scoped:
          for
            port   <- Server.install(testServer.statelessRoutes)
            client <- McpClient.connect(s"http://localhost:$port/mcp")
            // `greet` returns a plain string, which can't decode into AddOutput
            err    <- client.callToolAs[AddOutput]("greet", addArgs(1, 1)).flip
          yield assertTrue(err.isInstanceOf[McpClientError.Decode])
      ,
    ).provide(
      Server.defaultWith(_.onAnyOpenPort),
      Client.default,
      McpServer.State.default,
    ) @@ withLiveClock @@ timeout(1.minute) @@ sequential
