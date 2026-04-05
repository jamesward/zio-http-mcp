package com.jamesward.ziohttp.mcp

import zio.*
import zio.http.*
import zio.schema.*
import zio.test.*
import zio.test.TestAspect.*

import io.modelcontextprotocol.client.McpClient
import io.modelcontextprotocol.client.transport.HttpClientStreamableHttpTransport
import io.modelcontextprotocol.spec.McpSchema as JMcpSchema

import java.time.Duration as JDuration

object JavaSdkClientSpec extends ZIOSpecDefault:

  case class AddInput(a: Int, b: Int) derives Schema
  case class AddOutput(result: Int) derives Schema

  val addTool: McpToolHandler = McpTool("add")
    .description("Add two numbers")
    .handle[Any, Nothing, AddInput, AddOutput]: input =>
      ZIO.succeed(AddOutput(input.a + input.b))

  val testServer = McpServer("test-server", "0.1.0")
    .tool(addTool)

  private def withClient[A](port: Int)(f: io.modelcontextprotocol.client.McpSyncClient => A): Task[A] =
    ZIO.attemptBlocking:
      val transport = HttpClientStreamableHttpTransport.builder(s"http://localhost:$port")
        .endpoint("/mcp")
        .build()
      val client = McpClient.sync(transport)
        .requestTimeout(JDuration.ofSeconds(10))
        .clientInfo(JMcpSchema.Implementation("test-client", "1.0.0"))
        .build()
      try
        client.initialize()
        f(client)
      finally
        client.close()

  override def spec =
    suite("Java SDK Client Integration")(
      test("tool call with parameters"):
        for
          port   <- Server.install(testServer.routes)
          result <- withClient(port): client =>
            client.callTool(JMcpSchema.CallToolRequest(
              "add",
              java.util.Map.of[String, Object]("a", Int.box(5), "b", Int.box(3)),
              null,
            ))
        yield
          val text = result.content().get(0).asInstanceOf[JMcpSchema.TextContent].text()
          assertTrue(
            result.isError == null || !result.isError,
            text == """{"result":8}""",
          )
      ,
    ).provide(Server.defaultWith(_.onAnyOpenPort)) @@
      withLiveClock @@
      timeout(1.minute) @@
      sequential
