package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.client.{McpClient, McpClientConfig}
import zio.*
import zio.http.*
import zio.json.*
import zio.json.ast.Json
import zio.schema.*
import zio.test.*
import zio.test.TestAspect.*

import io.modelcontextprotocol.client.McpClient as JMcpClient
import io.modelcontextprotocol.client.transport.HttpClientStreamableHttpTransport
import io.modelcontextprotocol.spec.McpSchema as JMcpSchema

import java.time.Duration as JDuration
import scala.jdk.CollectionConverters.*

given canEqualStatusDualEra: CanEqual[Status, Status] = CanEqual.derived

/**
 * End-to-end integration: one running dual-era [[McpServer]] must serve both an
 * old-protocol client and a new-protocol client interchangeably.
 *
 *   - **Old-protocol client**: the real third-party Java MCP SDK (`2025-11-25`),
 *     which connects with the `initialize` handshake and sessions.
 *   - **New-protocol client**: our own `McpClient` negotiating the modern
 *     `2026-07-28` era via `server/discover`.
 *   - **Minimal new-protocol client**: raw modern requests that omit the
 *     2026-07-28 routing headers (`Mcp-Method` / `Mcp-Name`) — the shape many
 *     real RC-era clients send. The server must accept them (regression guard
 *     for the header-leniency fix).
 */
object DualEraIntegrationSpec extends ZIOSpecDefault:

  case class AddInput(a: Int, b: Int) derives Schema
  case class AddOutput(result: Int) derives Schema

  val addTool: McpToolHandler = McpTool("add")
    .description("Add two numbers")
    .handle[Any, Nothing, AddInput, AddOutput]: in =>
      ZIO.succeed(AddOutput(in.a + in.b))

  val server = McpServer("dual-era-server", "1.0.0")
    .instructions("works for old and new clients")
    .tool(addTool)

  // --- old-protocol client: the real Java MCP SDK ---

  private def withJavaClient[A](port: Int)(f: io.modelcontextprotocol.client.McpSyncClient => A): Task[A] =
    ZIO.attemptBlocking:
      val transport = HttpClientStreamableHttpTransport.builder(s"http://localhost:$port").endpoint("/mcp").build()
      val client = JMcpClient.sync(transport)
        .requestTimeout(JDuration.ofSeconds(10))
        .clientInfo(JMcpSchema.Implementation.builder("old-java-client", "1.0.0").build())
        .build()
      try
        client.initialize()
        f(client)
      finally client.close()

  // --- minimal new-protocol client: raw modern POST WITHOUT routing headers ---

  private def rawModernNoHeaders(port: Int, id: Int, method: String, extra: Chunk[(String, Json)]): ZIO[Client & Scope, Throwable, Json.Obj] =
    val meta = Json.Obj(
      McpMeta.ProtocolVersion    -> Json.Str(ProtocolVersion.V2026_07_28.wire),
      McpMeta.ClientInfo         -> Json.Obj("name" -> Json.Str("minimal"), "version" -> Json.Str("1")),
      McpMeta.ClientCapabilities -> Json.Obj(),
    )
    val params = Json.Obj(extra :+ ("_meta" -> (meta: Json)))
    val body = s"""{"jsonrpc":"2.0","id":$id,"method":"$method","params":${(params: Json).toJson}}"""
    val url = URL.decode(s"http://localhost:$port/mcp").toOption.get
    // Deliberately NO Mcp-Method / Mcp-Name headers — only the mandatory basics.
    val req = Request.post(url, Body.fromString(body))
      .addHeader(Header.ContentType(MediaType.application.json))
      .addHeader("accept", "application/json, text/event-stream")
    ZClient.batched(req).flatMap: r =>
      r.body.asString.flatMap(s => ZIO.fromEither(s.fromJson[Json.Obj]).mapError(e => RuntimeException(s"$e: $s"))).map(j => (r.status, j)).map(_._2)

  override def spec =
    suite("DualEraIntegrationSpec (one server, old + new clients)")(

      test("the same server serves an old-protocol (Java SDK) and a new-protocol (our) client"):
        for
          port <- Server.install(server.routes)

          // --- old-protocol client: Java SDK, legacy initialize handshake ---
          oldTools  <- withJavaClient(port)(_.listTools().tools().asScala.map(_.name()).toList)
          oldResult <- withJavaClient(port): c =>
                         c.callTool(JMcpSchema.CallToolRequest("add",
                           java.util.Map.of[String, Object]("a", Int.box(20), "b", Int.box(22)), null))
          oldText    = oldResult.content().get(0).asInstanceOf[JMcpSchema.TextContent].text()

          // --- new-protocol client: our McpClient, modern negotiation ---
          newVersion <- ZIO.scoped:
                          McpClient.connect(s"http://localhost:$port/mcp").map(_.protocolVersion)
          newResult  <- ZIO.scoped:
                          McpClient.connect(s"http://localhost:$port/mcp").flatMap(_.callToolAs[AddOutput]("add", Json.Obj("a" -> Json.Num(4), "b" -> Json.Num(5))))
        yield assertTrue(
          // old client: negotiated legacy, saw the tool, got the result over the session
          oldTools.contains("add"),
          oldText == """{"result":42}""",
          // new client: negotiated modern, got the structured result statelessly
          newVersion == ProtocolVersion.V2026_07_28.wire,
          newResult.result == 9,
        )
      ,

      test("a minimal new-protocol client with no routing headers is served"):
        for
          port      <- Server.install(server.routes)
          discover  <- rawModernNoHeaders(port, 1, "server/discover", Chunk.empty)
          call      <- rawModernNoHeaders(port, 2, "tools/call",
                         Chunk("name" -> Json.Str("add"), "arguments" -> Json.Obj("a" -> Json.Num(7), "b" -> Json.Num(8))))
        yield
          val supported = discover.get("result").flatMap(_.asObject).flatMap(_.get("supportedVersions"))
            .flatMap(_.asArray).map(_.flatMap(_.asString).toList).getOrElse(Nil)
          val callText = call.get("result").flatMap(_.asObject).flatMap(_.get("content")).flatMap(_.asArray)
            .flatMap(_.headOption).flatMap(_.asObject).flatMap(_.get("text")).flatMap(_.asString)
          assertTrue(
            // discover succeeds and offers the modern version, even with no Mcp-Method header
            supported.contains(ProtocolVersion.V2026_07_28.wire),
            // and a tool call with no Mcp-Name header is accepted
            callText.exists(_.contains("15")),
          )
      ,

    ).provide(Server.defaultWith(_.onAnyOpenPort), Client.default, Scope.default, McpServer.State.default) @@
      withLiveClock @@ timeout(2.minutes) @@ sequential
