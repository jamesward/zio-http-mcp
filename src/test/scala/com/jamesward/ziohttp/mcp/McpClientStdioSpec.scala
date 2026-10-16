package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.client.*
import zio.*
import zio.json.ast.Json
import zio.test.*
import zio.test.TestAspect.*

/**
 * Exercises the stdio transport of [[McpClient]]
 * ([[McpClient.connectStdio]]) against servers launched as subprocesses of the
 * test JVM: the scripted [[StdioFixtureServer]] in its legacy, silent-legacy
 * and modern modes, and the official Java SDK's stdio server
 * ([[JavaSdkStdioServer]]). No network.
 */
object McpClientStdioSpec extends ZIOSpecDefault:

  /** A test-classpath main as an MCP server command, with its stderr recorded. */
  private def server(main: String, stderr: Ref[Chunk[String]], env: (String, String)*): Task[McpStdioConfig] =
    for
      javaHome  <- System.property("java.home").someOrFail(IllegalStateException("java.home is not set"))
      classpath <- System.property("test.classpath").someOrFail(IllegalStateException("test.classpath is not set (see build.sbt)"))
    yield McpStdioConfig(
      s"$javaHome/bin/java",
      Chunk("-cp", classpath, main),
      env = env.toMap,
      stderr = line => stderr.update(_ :+ line),
    )

  private def fixture(stderr: Ref[Chunk[String]], mode: String, env: (String, String)*): Task[McpStdioConfig] =
    server("com.jamesward.ziohttp.mcp.StdioFixtureServer", stderr, (("FIXTURE_MODE" -> mode) +: env)*)

  private def textOf(result: CallToolResult): String =
    result.content.collect { case ToolContent.Text(t, _) => t }.mkString

  private def structured(result: CallToolResult, key: String): Option[Json] =
    result.structuredContent.flatMap(_.asObject).flatMap(_.get(key))

  private val echo = Json.Obj("text" -> Json.Str("hi"))

  /**
   * A test whose servers are stopped inside it. ZIO Test closes the Scope it
   * provides after the test, outside `withLiveClock`, where the shutdown's
   * timeouts would wait on the TestClock forever.
   */
  private def stdioTest[E](name: String)(body: => ZIO[Scope, E, TestResult]) =
    test(name)(ZIO.scoped(body))

  def spec = suite("McpClientStdioSpec")(
    stdioTest("a legacy server: discover is refused, then initialize; the client answers the server's ping") {
      for
        stderr <- Ref.make(Chunk.empty[String])
        config <- fixture(stderr, "legacy")
        client <- McpClient.connectStdio(config)
        tools  <- client.listTools
        result <- client.callTool("echo", echo)
      yield assertTrue(
        client.protocolVersion == "2025-11-25",
        client.serverInfo.name == "stdio-fixture",
        tools.map(_.name.value) == Chunk("echo", "slow", "crash"),
        textOf(result) == "echo: hi",
        structured(result, "pinged").contains(Json.Bool(true)),
      )
    },
    stdioTest("a legacy server that never answers server/discover is detected after the probe timeout") {
      for
        stderr  <- Ref.make(Chunk.empty[String])
        config  <- fixture(stderr, "silent").map(_.copy(discoverTimeout = 500.millis))
        timed   <- McpClient.connectStdio(config).timed
        (elapsed, client) = timed
        result  <- client.callTool("echo", echo)
      yield assertTrue(
        client.protocolVersion == "2025-11-25",
        elapsed >= 500.millis,
        textOf(result) == "echo: hi",
      )
    },
    stdioTest("a modern server: server/discover, and every request carries the protocol version in its _meta") {
      for
        stderr <- Ref.make(Chunk.empty[String])
        config <- fixture(stderr, "modern")
        client <- McpClient.connectStdio(config)
        result <- client.callTool("echo", echo)
      yield assertTrue(
        client.protocolVersion == "2026-07-28",
        client.serverInfo.name == "stdio-fixture",
        textOf(result) == "echo: hi",
        structured(result, "protocolVersion").contains(Json.Str("2026-07-28")),
      )
    },
    stdioTest("a modern server slower to start than the probe timeout is still modern") {
      for
        stderr <- Ref.make(Chunk.empty[String])
        config <- fixture(stderr, "modern", "FIXTURE_START_DELAY_MS" -> "1500").map(_.copy(discoverTimeout = 300.millis))
        client <- McpClient.connectStdio(config)
        result <- client.callTool("echo", echo)
      yield assertTrue(
        client.protocolVersion == "2026-07-28",
        structured(result, "protocolVersion").contains(Json.Str("2026-07-28")),
      )
    },
    stdioTest("a slow legacy server that never answers server/discover still completes the handshake") {
      for
        stderr <- Ref.make(Chunk.empty[String])
        config <- fixture(stderr, "silent", "FIXTURE_START_DELAY_MS" -> "1500").map(_.copy(discoverTimeout = 300.millis))
        client <- McpClient.connectStdio(config)
        result <- client.callTool("echo", echo)
      yield assertTrue(client.protocolVersion == "2025-11-25", textOf(result) == "echo: hi")
    },
    stdioTest("an interrupted call is cancelled with notifications/cancelled") {
      for
        stderr    <- Ref.make(Chunk.empty[String])
        config    <- fixture(stderr, "legacy")
        client    <- McpClient.connectStdio(config)
        abandoned <- client.callTool("slow", Json.Obj()).timeout(300.millis)
        _         <- stderr.get.repeatUntil(_.exists(_.startsWith("cancelled "))).timeout(5.seconds)
        logged    <- stderr.get
        after     <- client.callTool("echo", echo)
      yield assertTrue(
        abandoned.isEmpty,
        logged.exists(_.startsWith("cancelled ")),
        textOf(after) == "echo: hi",
      )
    },
    stdioTest("a call fails when the server process dies") {
      for
        stderr <- Ref.make(Chunk.empty[String])
        config <- fixture(stderr, "legacy")
        client <- McpClient.connectStdio(config)
        error  <- client.callTool("crash", Json.Obj()).flip
        later  <- client.callTool("echo", echo).flip
      yield assertTrue(
        error == McpClientError.Transport("the MCP server process closed its stdout"),
        later == McpClientError.Transport("the MCP server process closed its stdout"),
      )
    },
    stdioTest("a command that can't start fails the connect") {
      for error <- McpClient.connectStdio(McpStdioConfig("/nonexistent/mcp-server")).flip
      yield assertTrue(error match
        case McpClientError.Transport(message, Some(_)) => message.contains("/nonexistent/mcp-server")
        case _                                          => false
      )
    },
    stdioTest("closing the scope stops a server that ignores its stdin closing") {
      for
        stderr  <- Ref.make(Chunk.empty[String])
        config  <- fixture(stderr, "legacy", "FIXTURE_IGNORE_EOF" -> "true").map(_.copy(shutdownTimeout = 300.millis))
        // Time only the shutdown: starting the server JVM can be slow on a loaded machine.
        out     <- ZIO.scoped(McpClient.connectStdio(config).flatMap(_.callTool("echo", echo)).zip(Clock.nanoTime))
        (result, closing) = out
        closed  <- Clock.nanoTime
      yield assertTrue(textOf(result) == "echo: hi", (closed - closing).nanos < 5.seconds)
    },
    stdioTest("the official Java SDK's stdio server") {
      for
        stderr <- Ref.make(Chunk.empty[String])
        config <- server("com.jamesward.ziohttp.mcp.JavaSdkStdioServer", stderr).map(_.copy(shutdownTimeout = 1.second))
        client <- McpClient.connectStdio(config)
        tools  <- client.listTools
        result <- client.callTool("echo", echo)
      yield assertTrue(
        client.protocolVersion == "2025-11-25",
        client.serverInfo.name == "java-sdk-stdio",
        tools.map(_.name.value) == Chunk("echo"),
        textOf(result) == "java echo: hi",
      )
    },
  ) @@ withLiveClock @@ withLiveSystem @@ timeout(2.minutes)
