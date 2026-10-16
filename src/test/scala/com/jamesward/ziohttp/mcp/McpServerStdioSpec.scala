package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.client.*
import io.modelcontextprotocol.client.McpClient as JavaMcpClient
import io.modelcontextprotocol.client.transport.{ServerParameters, StdioClientTransport}
import io.modelcontextprotocol.json.McpJsonDefaults
import io.modelcontextprotocol.spec.McpSchema as JMcpSchema
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.schema.*
import zio.test.*
import zio.test.TestAspect.*

import java.io.{BufferedReader, InputStreamReader, OutputStreamWriter, PipedInputStream, PipedOutputStream}
import java.nio.charset.StandardCharsets
import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}
import scala.jdk.CollectionConverters.*

/**
 * The server side of the stdio transport: [[McpServer.serveStream]] spoken to in
 * raw JSON-RPC over in-process pipes, and [[McpServer.serveStdio]] in a
 * subprocess ([[StdioServerMain]]) used by this library's stdio client and by
 * the official Java SDK's. No network.
 */
object McpServerStdioSpec extends ZIOSpecDefault:

  case class Steps(count: Int) derives Schema

  private val server: McpServer[Any] =
    McpServer("stream-server", "1.0.0")
      .tool(McpTool("echo").handle[Any, Nothing, StdioServerMain.EchoInput, String](in => ZIO.succeed(s"echo: ${in.text}")))
      .tool(McpTool("slow").handle[Any, Nothing, String](ZIO.never))
      .tool(McpTool("steps").handleWithContext[Any, Nothing, Steps, String] { (in, ctx) =>
        ZIO.foreachDiscard(1 to in.count)(i => ctx.progress(i.toDouble, in.count.toDouble)).as("done")
      })

  /** A server on in-process pipes, and the raw client end of them. */
  final class Pipes(toServer: OutputStreamWriter, fromServer: BufferedReader):
    private val received = LinkedBlockingQueue[Json.Obj]()
    private val reader = Thread(() =>
      Iterator.continually(fromServer.readLine()).takeWhile(_ != null).foreach(line =>
        received.put(line.fromJson[Json.Obj].fold(e => Json.Obj("unparseable" -> Json.Str(s"$e: $line")), o => o))
      )
    )
    reader.setDaemon(true)
    reader.start()

    def send(line: String): UIO[Unit] = ZIO.succeedBlocking { toServer.write(line + "\n"); toServer.flush() }
    def close: UIO[Unit] = ZIO.succeedBlocking(toServer.close())

    /** Every message until (and including) the response to `id`. */
    def until(id: Json): Task[Chunk[Json.Obj]] = ZIO.attemptBlocking {
      val out = Chunk.newBuilder[Json.Obj]
      var done = false
      while !done do
        val next = received.poll(10, TimeUnit.SECONDS)
        if next == null then throw AssertionError(s"no response to $id")
        out += next
        done = next.get("id").contains(id) && next.get("method").isEmpty
      out.result()
    }

    def next: Task[Json.Obj] =
      ZIO.attemptBlocking(Option(received.poll(10, TimeUnit.SECONDS)).getOrElse(throw AssertionError("no message")))

    def quiet(millis: Long): Task[List[Json.Obj]] =
      ZIO.attemptBlocking { Thread.sleep(millis); val l = java.util.ArrayList[Json.Obj](); received.drainTo(l); l.asScala.toList }

  private def pipes(server: McpServer[Any]): ZIO[Scope, Throwable, (Pipes, Fiber[Throwable, Unit])] =
    for
      clientOut <- ZIO.succeed(PipedOutputStream())
      serverIn  <- ZIO.succeed(PipedInputStream(clientOut, 65536))
      serverOut <- ZIO.succeed(PipedOutputStream())
      clientIn  <- ZIO.succeed(PipedInputStream(serverOut, 65536))
      fiber     <- server.serveStream(serverIn, serverOut).ensuring(ZIO.succeed(serverOut.close())).forkScoped
      p          = Pipes(OutputStreamWriter(clientOut, StandardCharsets.UTF_8), BufferedReader(InputStreamReader(clientIn, StandardCharsets.UTF_8)))
    yield (p, fiber)

  private val modernMeta =
    """"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28","io.modelcontextprotocol/clientCapabilities":{}}"""

  private def field(json: Json.Obj, path: String*): Option[Json] =
    path.foldLeft(Option[Json](json))((j, k) => j.flatMap(_.asObject).flatMap(_.get(k)))

  /** The subprocess server's command line. */
  private def stdioServer: Task[(String, Chunk[String])] =
    for
      javaHome  <- System.property("java.home").someOrFail(IllegalStateException("java.home is not set"))
      classpath <- System.property("test.classpath").someOrFail(IllegalStateException("test.classpath is not set (see build.sbt)"))
    yield (s"$javaHome/bin/java", Chunk("-cp", classpath, "com.jamesward.ziohttp.mcp.StdioServerMain"))

  private def textOf(result: CallToolResult): String =
    result.content.collect { case ToolContent.Text(t, _) => t }.mkString

  def spec = suite("McpServerStdioSpec")(
    suite("serveStream")(
      test("answers each request on its own line, with request-scoped notifications first") {
        ZIO.scoped:
          for
            (p, _)   <- pipes(server)
            _        <- p.send("""{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"t","version":"1"}}}""")
            init     <- p.until(Json.Num(1))
            _        <- p.send("""{"jsonrpc":"2.0","method":"notifications/initialized"}""")
            _        <- p.send(s"""{"jsonrpc":"2.0","id":"d","method":"server/discover","params":{$modernMeta}}""")
            discover <- p.until(Json.Str("d"))
            _        <- p.send(s"""{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28","io.modelcontextprotocol/clientCapabilities":{},"progressToken":"p"},"name":"steps","arguments":{"count":3}}}""")
            steps    <- p.until(Json.Num(2))
          yield assertTrue(
            init.size == 1, // the notification got no answer
            field(init.head, "result", "serverInfo", "name").contains(Json.Str("stream-server")),
            field(discover.head, "result", "supportedVersions").exists(_.toJson.contains("2026-07-28")),
            steps.size == 4,
            steps.init.forall(_.get("method").contains(Json.Str("notifications/progress"))),
            steps.init.flatMap(field(_, "params", "progress")).map(_.toJson) == Chunk("1.0", "2.0", "3.0"),
            field(steps.last, "result", "content").exists(_.toJson.contains("done")),
          )
      },
      test("serves requests concurrently, and a cancelled request gets no answer") {
        ZIO.scoped:
          for
            (p, _) <- pipes(server)
            _      <- p.send("""{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"slow","arguments":{}}}""")
            _      <- p.send("""{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"echo","arguments":{"text":"hi"}}}""")
            echo   <- p.until(Json.Num(2))
            _      <- p.send("""{"jsonrpc":"2.0","method":"notifications/cancelled","params":{"requestId":1,"reason":"test"}}""")
            late   <- p.quiet(500)
          yield assertTrue(
            echo.size == 1,
            field(echo.head, "result", "content").exists(_.toJson.contains("echo: hi")),
            late.isEmpty,
          )
      },
      test("a malformed line is a parse error with a null id; unknown methods are JSON-RPC errors") {
        ZIO.scoped:
          for
            (p, _)  <- pipes(server)
            _       <- p.send("""{"jsonrpc":"2.0","id":1,""")
            parse   <- p.next
            _       <- p.send("""{"jsonrpc":"2.0","id":3,"method":"no/such","params":{}}""")
            unknown <- p.until(Json.Num(3))
          yield assertTrue(
            field(parse, "error", "code").contains(Json.Num(-32700)),
            parse.get("id").contains(Json.Null),
            field(unknown.head, "error", "code").contains(Json.Num(-32601)),
          )
      },
      test("subscriptions/listen is acknowledged with the request's id as the subscription id") {
        ZIO.scoped:
          for
            (p, _) <- pipes(server)
            _      <- p.send(s"""{"jsonrpc":"2.0","id":"sub-1","method":"subscriptions/listen","params":{$modernMeta,"notifications":{"toolsListChanged":true}}}""")
            ack    <- p.next
          yield assertTrue(
            ack.get("method").contains(Json.Str("notifications/subscriptions/acknowledged")),
            field(ack, "params", "_meta", "io.modelcontextprotocol/subscriptionId").contains(Json.Str("sub-1")),
            field(ack, "params", "notifications").contains(Json.Obj()),
          )
      },
      test("returns when its input ends, interrupting requests still running") {
        ZIO.scoped:
          for
            (p, fiber) <- pipes(server)
            _          <- p.send("""{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"slow","arguments":{}}}""")
            _          <- ZIO.sleep(200.millis)
            _          <- p.close
            exit       <- fiber.join.timeout(5.seconds).exit
          yield assertTrue(exit.getOrElse(_ => None).isDefined)
      },
    ),
    suite("serveStdio in a subprocess")(
      test("this library's stdio client, modern and legacy; tool output and logs stay off stdout") {
        ZIO.scoped:
          for
            (java, args) <- stdioServer
            stderr       <- Ref.make(Chunk.empty[String])
            // A cold JVM can take longer than the default probe timeout to answer.
            config        = McpStdioConfig(java, args, stderr = line => stderr.update(_ :+ line), discoverTimeout = 30.seconds)
            modern       <- McpClient.connectStdio(config)
            legacy       <- McpClient.connectStdio(config.copy(preferredVersion = ProtocolVersion.V2025_11_25))
            tools        <- modern.listTools
            echoed       <- modern.callTool("echo", Json.Obj("text" -> Json.Str("hi")))
            old          <- legacy.callTool("echo", Json.Obj("text" -> Json.Str("there")))
            logged       <- stderr.get
          yield assertTrue(
            modern.protocolVersion == "2026-07-28",
            legacy.protocolVersion == "2025-11-25",
            modern.serverInfo.name == "stdio-server",
            tools.map(_.name.value).toSet == Set("echo", "slow"),
            textOf(echoed) == "echo: hi",
            textOf(old) == "echo: there",
            logged.exists(_.contains("noise: hi")),
            logged.exists(_.contains("echoing hi")),
          )
      },
      test("an interrupted call is cancelled in the server, and the server exits when its stdin closes") {
        for
          (java, args) <- stdioServer
          stderr       <- Ref.make(Chunk.empty[String])
          config        = McpStdioConfig(java, args, stderr = line => stderr.update(_ :+ line), shutdownTimeout = 3.seconds,
                            discoverTimeout = 30.seconds)
          closed       <- ZIO.scoped(
                            McpClient.connectStdio(config).flatMap(client =>
                              client.callTool("slow", Json.Obj()).timeout(1.second) *>
                                // the cancellation reached the server and interrupted the tool
                                stderr.get.repeatUntil(_.exists(_.contains("slow interrupted"))).timeout(30.seconds)
                            ).zip(Clock.nanoTime) // closing the scope (the server's stdin) starts here
                          )
          (interrupted, closing) = closed
          closedAt     <- Clock.nanoTime
          logged       <- stderr.get
        yield assertTrue(
          interrupted.isDefined,
          // Closing stdin was enough: it exited before shutdownTimeout, without being terminated.
          (closedAt - closing).nanos < 3.seconds,
          !logged.exists(_.contains("did not exit")),
        )
      },
      test("the official Java SDK's stdio client") {
        for
          (javaCmd, args) <- stdioServer
          result       <- ZIO.acquireReleaseWith(ZIO.attemptBlocking {
                            val transport = StdioClientTransport(
                              ServerParameters.builder(javaCmd).args(args.asJava).build(),
                              McpJsonDefaults.getMapper(),
                            )
                            val client = JavaMcpClient.sync(transport)
                              .requestTimeout(java.time.Duration.ofSeconds(30))
                              .clientInfo(JMcpSchema.Implementation.builder("java-sdk", "1.0.0").build())
                              .build()
                            client.initialize()
                            client
                          })(c => ZIO.attemptBlocking(c.closeGracefully()).ignore) { client =>
                            ZIO.attemptBlocking {
                              val tools = client.listTools().tools().asScala.map(_.name()).toSet
                              val call = client.callTool(
                                JMcpSchema.CallToolRequest.builder("echo").arguments(java.util.Map.of[String, Object]("text", "sdk")).build()
                              )
                              (tools, call.content().asScala.collect { case t: JMcpSchema.TextContent => t.text() }.mkString)
                            }
                          }
          (tools, text) = result
        yield assertTrue(tools == Set("echo", "slow"), text == "echo: sdk")
      },
    ),
  ) @@ withLiveClock @@ withLiveSystem @@ timeout(3.minutes)
