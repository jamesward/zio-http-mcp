package com.jamesward.ziohttp.mcp

import org.testcontainers.containers.GenericContainer
import org.testcontainers.containers.output.ToStringConsumer
import org.testcontainers.containers.startupcheck.OneShotStartupCheckStrategy
import org.testcontainers.images.builder.ImageFromDockerfile
import zio.*
import zio.http.*
import zio.schema.*
import zio.test.*

import java.time.Duration as JDuration

/**
 * MCP Events end to end with an MCP client in a container, for [[AiSdkEventsSpec]], [[TanStackEventsSpec]],
 * [[MastraEventsSpec]] and [[PythonEventsSpec]]. The container runs a probe, `src/test/resources/mcp-events-js/probe.mjs`
 * or `src/test/resources/mcp-events-python/probe.py`: the client subscribes to `incident.created` with a webhook on its
 * own `127.0.0.1`, the spec emits one incident once the client has its subscribe answer, and the client's webhook
 * receiver verifies it and hands it to the probe, then the client unsubscribes. Each probe prints the same `RESULT`
 * lines.
 *
 * The AI SDK and TanStack accept a plain-http callback only on `127.0.0.1`, so the server must reach the container's
 * loopback: the container shares the host's network, and the specs are ignored where that isn't available
 * ([[DockerSupport.useHostNetwork]]).
 */
object EventsClients:
  case class Incident(id: String, severity: String) derives Schema
  case class IncidentFilter(severity: Option[String]) derives Schema

  val EventName = "incident.created"

  /** The incident the spec emits, and the `eventId` it is emitted with. */
  val incident: Incident = Incident("INC-1", "P1")
  val eventId = "evt_inc_1"

  /** The spec for every client, as `McpWebhookConfig`'s default. */
  val spec: McpWebhookConfig = McpWebhookConfig(
    anonymous = true,
    targets = McpWebhookTargets.unrestricted,
    retryDelays = Chunk(500.millis, 1.second, 2.seconds),
  )

  /** With the workarounds for the known clients, which each of these needs. */
  val knownClients: McpWebhookConfig = spec.copy(compatibility = McpEventsCompatibility.KnownClients)

  private val PythonSdkCommit = "a5e06301315a4f56b9467f7dd30eaecd525ff0cc"

  /** A client in a container: its image, and the arguments before the MCP endpoint URL. */
  final case class Probe(image: ImageFromDockerfile, args: List[String])

  /** A JavaScript client, by the name probe.mjs gives it: `ai-sdk`, `tanstack` or `mastra`. */
  def js(sdk: String): Probe = Probe(jsImage, List(sdk))

  /** The Python MCP SDK's Events client, from python-sdk PR 3822 at its commit of 5 Nov 2026 (not released). */
  lazy val python: Probe = Probe(pythonImage, Nil)

  private val jsFiles = List("package.json", "package-lock.json", "probe.mjs", "ai-sdk.mjs", "tanstack.mjs", "mastra.mjs")

  private val ProxyCa = "/usr/local/share/ca-certificates/proxy-ca.crt"

  private def withHostCa(image: ImageFromDockerfile): ImageFromDockerfile =
    DockerSupport.hostCaBundle.fold(image)(ca => image.withFileFromPath("ca-bundle.crt", ca))

  private lazy val pythonImage: ImageFromDockerfile =
    withHostCa(ImageFromDockerfile("mcp-events-python", false)
      .withFileFromClasspath("probe.py", "mcp-events-python/probe.py")).withDockerfileFromBuilder: builder =>
      builder.from("python:3.12-slim").workDir("/probe")
      DockerSupport.hostCaBundle.foreach: _ =>
        builder.copy("ca-bundle.crt", ProxyCa)
        builder.env("SSL_CERT_FILE", ProxyCa)
        builder.env("PIP_CERT", ProxyCa)
        builder.env("GIT_SSL_CAINFO", ProxyCa)
      builder
        .run("apt-get update && apt-get install -y --no-install-recommends git && rm -rf /var/lib/apt/lists/*")
        // A clone with its tags: the package's version comes from them (uv-dynamic-versioning).
        .run(s"git clone --filter=blob:none https://github.com/modelcontextprotocol/python-sdk /sdk && git -C /sdk checkout $PythonSdkCommit")
        .run("pip install --no-cache-dir /sdk/src/mcp-types /sdk")
        .copy("probe.py", "probe.py")
        .entryPoint("python", "probe.py")
        .build()

  private lazy val jsImage: ImageFromDockerfile =
    val base = jsFiles.foldLeft(ImageFromDockerfile("mcp-events-js", false)): (image, file) =>
      image.withFileFromClasspath(file, s"mcp-events-js/$file")
    withHostCa(base).withDockerfileFromBuilder: builder =>
      builder.from("node:22-slim").workDir("/probe")
      DockerSupport.hostCaBundle.foreach: _ =>
        builder.copy("ca-bundle.crt", ProxyCa)
        builder.env("NODE_EXTRA_CA_CERTS", ProxyCa)
      builder
        .copy("package.json", "package.json")
        .copy("package-lock.json", "package-lock.json")
        .run("npm ci --ignore-scripts --no-audit --no-fund")
        .copy("probe.mjs", "probe.mjs")
        .copy("ai-sdk.mjs", "ai-sdk.mjs")
        .copy("tanstack.mjs", "tanstack.mjs")
        .copy("mastra.mjs", "mastra.mjs")
        .entryPoint("node", "probe.mjs")
        .build()

  /** What one probe run printed (its `RESULT` lines, in order), and the subscriptions left in the store after it. */
  final case class Run(results: List[String], output: String, leftOver: Chunk[McpWebhookSubscription]):
    def has(line: String): Boolean = results.contains(line)
    def startingWith(prefix: String): List[String] = results.filter(_.startsWith(prefix))

  /** Serve `incident.created` with webhook delivery, run `probe`, and emit the incident once it has subscribed. */
  def run(probe: Probe, config: McpWebhookConfig): ZIO[Server & McpServer.State & Scope, McpEventsError | Throwable, Run] =
    for
      store     <- TestWebhookStore.make()
      incidents <- McpEventBuffer.make[Incident]()
      created    = McpEvent(EventName).buffered[IncidentFilter, Incident](incidents, (filter, incident, _) =>
                     Option.when(filter.severity.forall(_ == incident.severity))(incident))
      events    <- McpEvents.withWebhooks(config)(created).provideSomeEnvironment[Scope](_.add[McpWebhookStore](store))
      port      <- Server.install(McpServer("events-interop", "1.0.0").withExtensions(events).routes)
      stdout     = ToStringConsumer()
      client    <- container(probe, s"http://localhost:$port/mcp", stdout).fork
      // Emit once the client has its subscribe answer, so the subscription's starting point is before the event. A
      // probe that fails to subscribe exits without it: then nothing is emitted, and its RESULT lines say why.
      subscribed = ZIO.succeed(stdout.toUtf8String.contains("RESULT subscribed "))
      ready     <- subscribed.delay(100.millis).repeatUntilZIO(ok => if ok then ZIO.succeed(true) else client.poll.map(_.isDefined))
      _         <- incidents.emit(incident, Some(eventId)).when(ready)
      output    <- client.join
      leftOver  <- store.all.mapError(e => RuntimeException(e.toString))
    yield Run(output.linesIterator.filter(_.startsWith("RESULT ")).map(_.stripPrefix("RESULT ")).toList, output, leftOver)

  private def container(probe: Probe, mcpUrl: String, stdout: ToStringConsumer): Task[String] =
    ZIO.attemptBlocking:
      val container = GenericContainer(probe.image)
      container.withNetworkMode("host")
      container.withCommand((probe.args :+ mcpUrl)*)
      container.withStartupCheckStrategy(OneShotStartupCheckStrategy().withTimeout(JDuration.ofMinutes(3)))
      container.withLogConsumer(stdout)
      try
        container.start()
        stdout.toUtf8String
      catch
        // A one-shot container that exits non-zero fails start(); its output still says what happened.
        case _: org.testcontainers.containers.ContainerLaunchException => stdout.toUtf8String
      finally
        try container.stop()
        catch case _: Exception => ()

  /** The full round trip with `probe`'s client: subscribe, challenge, the event in its receiver, unsubscribe. */
  def endToEnd(probe: Probe, config: McpWebhookConfig): Spec[Server & McpServer.State, McpEventsError | Throwable] =
    zio.test.test("subscribes, passes the challenge, receives the event, and unsubscribes"):
      for run <- ZIO.scoped(run(probe, config))
      yield assertTrue(
        run.startingWith("subscribed ").nonEmpty,
        run.startingWith("verification 200 ").nonEmpty,
        run.startingWith("delivery ").exists(_.startsWith("delivery 2")),
        run.has(s"""event $eventId $EventName {"id":"${incident.id}","severity":"${incident.severity}"}"""),
        run.has("unsubscribed"),
        run.startingWith("error ").isEmpty,
        run.leftOver.isEmpty,
      )

  /**
   * A client's suite. The aspects go after `provide`: the server's handlers run on the clock the Server layer was
   * built with, and a test clock would sign webhooks with a 1970 `webhook-timestamp`, which the clients refuse.
   */
  def suite(name: String)(tests: Spec[Server & McpServer.State, McpEventsError | Throwable]*): Spec[Any, McpEventsError | Throwable] =
    zio.test.suite(name)(tests*).provide(Server.defaultWith(_.onAnyOpenPort), McpServer.State.default) @@
      TestAspect.withLiveClock @@ TestAspect.timeout(10.minutes) @@ TestAspect.sequential @@
      (if DockerSupport.useHostNetwork then TestAspect.identity else TestAspect.ignore)
