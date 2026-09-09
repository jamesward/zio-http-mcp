package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.client.*
import zio.*
import zio.http.*
import zio.json.ast.Json
import zio.schema.*
import zio.test.*
import zio.test.TestAspect.*

/**
 * Exercises our [[com.jamesward.ziohttp.mcp.client.McpClient]] negotiating the
 * modern (2026-07-28) era against our own dual-era [[McpServer]]. With the
 * default `preferredVersion` the client probes `server/discover`, settles on
 * the modern version, and drives the server statelessly — including a full
 * MRTR round trip.
 */
object McpClientModernSpec extends ZIOSpecDefault:

  case class AddInput(a: Int, b: Int) derives Schema
  case class AddOutput(result: Int) derives Schema

  val addTool: McpToolHandler = McpTool("add")
    .description("Add two numbers")
    .handle[Any, Nothing, AddInput, AddOutput]: input =>
      ZIO.succeed(AddOutput(input.a + input.b))

  val configResource: McpResourceHandler = McpResource("app://config", "App Config")
    .mimeType("application/json")
    .read: uri =>
      ZIO.succeed(Chunk(ResourceContents(uri = uri, mimeType = Some("application/json"), text = Some("""{"debug":false}"""))))

  val summarizeTool: McpToolHandler = McpTool("summarize")
    .description("Summarizes via sampling")
    .handleWithContext[Any, ToolError, Chunk[ToolContent]]: ctx =>
      ctx.sample("Summarize", 50).map: r =>
        val t = r.content match { case ToolContent.Text(x, _) => x; case _ => "" }
        Chunk(ToolContent.text(s"summary: $t"))

  /**
   * Two rounds of input, resumed from the server's opaque `requestState` rather
   * than by replaying the handler — the client has to echo that state back for
   * this to get past round two.
   */
  val twoStepTool: McpToolHandler = McpTool("two_step")
    .description("Collects a name, then a colour")
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

  val server = McpServer("modern-server", "2.0.0")
    .instructions("modern hints")
    .tool(addTool)
    .tool(summarizeTool)
    .tool(twoStepTool)
    .resource(configResource)

  private def addArgs(a: Int, b: Int): Json.Obj =
    Json.Obj(Chunk("a" -> Json.Num(a), "b" -> Json.Num(b)))

  /** An onInputRequest handler that answers any sampling request with a fixed message. */
  private val samplingHandler: InputRequest => IO[McpClientError, Json] = _ =>
    ZIO.succeed(Json.Obj(
      "role" -> Json.Str("assistant"),
      "model" -> Json.Str("test"),
      "content" -> Json.Obj("type" -> Json.Str("text"), "text" -> Json.Str("done")),
    ))

  /** Answers each elicitation by the id the server asked under. */
  private val elicitationHandler: InputRequest => IO[McpClientError, Json] = request =>
    val field = if request.id == "step1" then "name" else "color"
    val value = if request.id == "step1" then "Ada" else "blue"
    ZIO.succeed(Json.Obj(
      "action" -> Json.Str("accept"),
      "content" -> Json.Obj(field -> Json.Str(value)),
    ))

  override def spec =
    suite("McpClient (modern negotiation vs our own dual-era server)")(

      test("client negotiates the modern version via server/discover"):
        ZIO.scoped:
          for
            port   <- Server.install(server.routes)
            client <- McpClient.connect(s"http://localhost:$port/mcp")
          yield assertTrue(
            client.protocolVersion == ProtocolVersion.V2026_07_28.wire,
            client.serverInfo.name == "modern-server",
            client.instructions.contains("modern hints"),
          )
      ,

      test("modern client lists tools and calls a tool statelessly"):
        ZIO.scoped:
          for
            port   <- Server.install(server.routes)
            client <- McpClient.connect(s"http://localhost:$port/mcp")
            tools  <- client.listTools
            result <- client.callToolAs[AddOutput]("add", addArgs(6, 7))
          yield assertTrue(
            tools.map(_.name.value).toSet == Set("add", "summarize", "two_step"),
            result.result == 13,
          )
      ,

      test("modern client reads a resource statelessly"):
        ZIO.scoped:
          for
            port     <- Server.install(server.routes)
            client   <- McpClient.connect(s"http://localhost:$port/mcp")
            contents <- client.readResource("app://config")
          yield assertTrue(contents.headOption.flatMap(_.text).contains("""{"debug":false}"""))
      ,

      test("modern client drives a full MRTR round trip transparently"):
        ZIO.scoped:
          for
            port   <- Server.install(server.routes)
            client <- McpClient.connect(McpClientConfig(
                        s"http://localhost:$port/mcp",
                        onInputRequest = Some(samplingHandler),
                      ))
            result <- client.callTool("summarize")
          yield
            val text = result.content.collectFirst { case ToolContent.Text(t, _) => t }
            assertTrue(text.contains("summary: done"))
      ,

      test("modern client echoes requestState across a multi-round MRTR exchange"):
        ZIO.scoped:
          for
            port   <- Server.install(server.routes)
            client <- McpClient.connect(McpClientConfig(
                        s"http://localhost:$port/mcp",
                        onInputRequest = Some(elicitationHandler),
                      ))
            result <- client.callTool("two_step")
          yield
            val text = result.content.collectFirst { case ToolContent.Text(t, _) => t }
            // Only reachable if the client echoed the state the server issued
            // in round one and again in round two.
            assertTrue(text.contains("done: blue"))
      ,

      test("modern client without an input handler surfaces a clear error on MRTR"):
        ZIO.scoped:
          for
            port   <- Server.install(server.routes)
            client <- McpClient.connect(s"http://localhost:$port/mcp")
            err    <- client.callTool("summarize").flip
          yield assertTrue(
            err.isInstanceOf[McpClientError.Protocol],
            err.getMessage.contains("MRTR") || err.getMessage.contains("input"),
          )
      ,

    ).provide(
      Server.defaultWith(_.onAnyOpenPort),
      Client.default,
      McpServer.State.default,
    ) @@ withLiveClock @@ timeout(1.minute) @@ sequential
