package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.auth.StaticBearer
import org.testcontainers.Testcontainers as TC
import org.testcontainers.containers.GenericContainer
import org.testcontainers.containers.output.ToStringConsumer
import org.testcontainers.containers.startupcheck.OneShotStartupCheckStrategy
import org.testcontainers.images.builder.ImageFromDockerfile
import zio.*
import zio.http.*
import zio.schema.*
import zio.test.*
import zio.test.TestAspect.*

import java.time.Duration as JDuration

/**
 * The official Python MCP SDK (`mcp` 1.30.0, on httpx 0.28.1) against a bearer-only server, in a container. claude.ai's connector check
 * is a Python httpx client: it POSTs `initialize` with protocol 2025-11-25 and no token, and takes a 401 for OAuth ("Sign in now
 * (Detected)"). This pins what such a client gets from [[StaticBearer]]: the SDK raises an `McpError` carrying code -32001 and our message
 * (no OAuth attempt), and with the token it initializes and lists the tools.
 */
object PythonSdkSpec extends ZIOSpecDefault:
  case class NameInput(name: String) derives Schema

  private val expected = "s3cret"

  private val server = McpServer("bearer-only", "1.0.0")
    .tool(McpTool("greet").description("Greets someone").handle: (input: NameInput) =>
      ZIO.succeed(s"Hello, ${input.name}!"))

  private val routes = server.mcpStatelessRoutes @@ StaticBearer.aspect(token => ZIO.succeed(token == expected))
  private val openRoutes = server.mcpStatelessRoutes @@ StaticBearer.aspect(token => ZIO.succeed(token == expected), open = StaticBearer.Handshake)

  private val probe =
    """import asyncio, sys
      |from mcp import ClientSession
      |from mcp.client.streamable_http import streamablehttp_client
      |
      |def leaves(e):
      |    if hasattr(e, "exceptions"):
      |        for x in e.exceptions: yield from leaves(x)
      |    else: yield e
      |
      |async def main(url, token):
      |    headers = {"Authorization": "Bearer " + token} if token else {}
      |    try:
      |        async with streamablehttp_client(url, headers=headers) as (read, write, _):
      |            async with ClientSession(read, write) as s:
      |                init = await s.initialize()
      |                print("RESULT initialized " + init.serverInfo.name + " " + init.protocolVersion)
      |                tools = await s.list_tools()
      |                print("RESULT tools " + ",".join(t.name for t in tools.tools))
      |    except BaseException as e:
      |        for x in leaves(e):
      |            err = getattr(x, "error", None)
      |            print("RESULT error " + str(getattr(err, "code", None)) + " " + str(getattr(err, "message", x)))
      |
      |asyncio.run(main(sys.argv[1], sys.argv[2] if len(sys.argv) > 2 else ""))
      |""".stripMargin

  private val image: ImageFromDockerfile =
    val base = ImageFromDockerfile("mcp-python-sdk", false).withFileFromString("probe.py", probe)
    val withContext = DockerSupport.hostCaBundle.fold(base)(ca => base.withFileFromPath("ca-bundle.crt", ca))
    withContext.withDockerfileFromBuilder: builder =>
      builder.from("python:3.12-slim")
      DockerSupport.hostCaBundle.foreach: _ =>
        builder.copy("ca-bundle.crt", "/usr/local/share/ca-certificates/proxy-ca.crt")
        builder.env("SSL_CERT_FILE", "/usr/local/share/ca-certificates/proxy-ca.crt")
        builder.env("REQUESTS_CA_BUNDLE", "/usr/local/share/ca-certificates/proxy-ca.crt")
        builder.env("PIP_CERT", "/usr/local/share/ca-certificates/proxy-ca.crt")
      builder
        .run("pip install --no-cache-dir mcp==1.30.0")
        .copy("probe.py", "/probe.py")
        .entryPoint("python", "/probe.py")
        .build()

  /** The `RESULT` lines the probe printed. */
  private def run(port: Int, token: Option[String]): Task[List[String]] =
    ZIO.attemptBlocking:
      val url = if DockerSupport.useHostNetwork then s"http://localhost:$port/mcp" else s"http://host.testcontainers.internal:$port/mcp"
      val stdout = ToStringConsumer()
      val container = GenericContainer(image)
      if DockerSupport.useHostNetwork then
        container.withNetworkMode("host")
        ()
      else
        TC.exposeHostPorts(port)
        container.withAccessToHost(true)
        ()
      container.withCommand((url +: token.toList)*)
      container.withStartupCheckStrategy(OneShotStartupCheckStrategy().withTimeout(JDuration.ofSeconds(120)))
      container.withLogConsumer(stdout)
      try
        container.start()
        stdout.toUtf8String.linesIterator.filter(_.startsWith("RESULT ")).toList
      catch
        case _: org.testcontainers.containers.ContainerLaunchException => stdout.toUtf8String.linesIterator.filter(_.startsWith("RESULT ")).toList
      finally
        try container.stop()
        catch case _: Exception => ()

  override def spec =
    suite("Python MCP SDK against a bearer-only server")(
      test("without a token the SDK raises an McpError with code -32001 and our message, not an OAuth failure"):
        for
          port <- Server.install(routes)
          lines <- run(port, None)
        yield assertTrue(
          lines.exists(_.startsWith("RESULT error -32001 ")),
          lines.exists(_.contains(StaticBearer.MissingMessage)),
          !lines.exists(_.contains("Dynamic Client Registration")),
          !lines.exists(_.startsWith("RESULT initialized")),
        )
      ,
      test("with a wrong token it says the token was not accepted"):
        for
          port <- Server.install(routes)
          lines <- run(port, Some("nope"))
        yield assertTrue(lines.exists(_.contains(StaticBearer.RejectedMessage)))
      ,
      test("with open = Handshake and no token it initializes like a plain server, and tools/list raises the McpError"):
        for
          port <- Server.install(openRoutes)
          lines <- run(port, None)
        yield assertTrue(
          lines.contains("RESULT initialized bearer-only 2025-11-25"),
          lines.exists(l => l.startsWith("RESULT error -32001 ") && l.contains(StaticBearer.MissingMessage)),
          !lines.exists(_.startsWith("RESULT tools")),
        )
      ,
      test("with the token it initializes (2025-11-25) and lists the tools"):
        for
          port <- Server.install(routes)
          lines <- run(port, Some(expected))
        yield assertTrue(lines.contains("RESULT initialized bearer-only 2025-11-25"), lines.contains("RESULT tools greet"))
    ).provide(Server.defaultWith(_.onAnyOpenPort)) @@
      withLiveClock @@
      timeout(10.minutes) @@
      sequential
