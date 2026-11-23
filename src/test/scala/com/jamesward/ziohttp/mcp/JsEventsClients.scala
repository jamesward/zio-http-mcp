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
 * MCP Events end to end with a JavaScript MCP client in a container, for [[AiSdkEventsSpec]], [[TanStackEventsSpec]]
 * and [[MastraEventsSpec]].
 * The container runs `src/test/resources/mcp-events-js/probe.mjs`: the client subscribes to `incident.created`
 * with a webhook on its own `127.0.0.1`, the spec emits one incident once the subscription is stored, and the
 * client's webhook handler verifies it and hands it to `onEvent`, then the client unsubscribes.
 *
 * The AI SDK and TanStack accept a plain-http callback only on `127.0.0.1`, so the server must reach the container's
 * loopback: the container shares the host's network, and the specs are ignored where that isn't available
 * ([[DockerSupport.useHostNetwork]]).
 */
object JsEventsClients:
  case class Incident(id: String, severity: String) derives Schema
  case class IncidentFilter(severity: Option[String]) derives Schema

  val EventName = "incident.created"

  /** The incident the spec emits, and the `eventId` it is emitted with. */
  val incident: Incident = Incident("INC-1", "P1")
  val eventId = "evt_inc_1"

  private val config = McpWebhookConfig(
    anonymous = true,
    targets = McpWebhookTargets.unrestricted,
    retryDelays = Chunk(500.millis, 1.second, 2.seconds),
  )

  private val files = List("package.json", "package-lock.json", "probe.mjs", "ai-sdk.mjs", "tanstack.mjs", "mastra.mjs")

  private val image: ImageFromDockerfile =
    val base = files.foldLeft(ImageFromDockerfile("mcp-events-js", false)): (image, file) =>
      image.withFileFromClasspath(file, s"mcp-events-js/$file")
    val withContext = DockerSupport.hostCaBundle.fold(base)(ca => base.withFileFromPath("ca-bundle.crt", ca))
    withContext.withDockerfileFromBuilder: builder =>
      builder.from("node:22-slim").workDir("/probe")
      DockerSupport.hostCaBundle.foreach: _ =>
        builder.copy("ca-bundle.crt", "/usr/local/share/ca-certificates/proxy-ca.crt")
        builder.env("NODE_EXTRA_CA_CERTS", "/usr/local/share/ca-certificates/proxy-ca.crt")
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

  /** Serve `incident.created` with webhook delivery, run the probe for `sdk`, and emit the incident once it has subscribed. */
  def run(sdk: String): ZIO[Server & McpServer.State & Scope, McpEventsError | Throwable, Run] =
    for
      store     <- TestWebhookStore.make()
      incidents <- McpEventBuffer.make[Incident]()
      created    = McpEvent(EventName).buffered[IncidentFilter, Incident](incidents, (filter, incident, _) =>
                     Option.when(filter.severity.forall(_ == incident.severity))(incident))
      events    <- McpEvents.withWebhooks(config)(created).provideSomeEnvironment[Scope](_.add[McpWebhookStore](store))
      port      <- Server.install(McpServer("events-interop", "1.0.0").withExtensions(events).routes)
      stdout     = ToStringConsumer()
      probe     <- container(sdk, s"http://localhost:$port/mcp", stdout).fork
      // Emit once the client has its subscribe answer, so the subscription's starting point is before the event. A
      // probe that fails to subscribe exits without it: then nothing is emitted, and its RESULT lines say why.
      subscribed = ZIO.succeed(stdout.toUtf8String.contains("RESULT subscribed "))
      ready     <- subscribed.delay(100.millis).repeatUntilZIO(ok => if ok then ZIO.succeed(true) else probe.poll.map(_.isDefined))
      _         <- incidents.emit(incident, Some(eventId)).when(ready)
      output    <- probe.join
      leftOver  <- store.all.mapError(e => RuntimeException(e.toString))
    yield Run(output.linesIterator.filter(_.startsWith("RESULT ")).map(_.stripPrefix("RESULT ")).toList, output, leftOver)

  private def container(sdk: String, mcpUrl: String, stdout: ToStringConsumer): Task[String] =
    ZIO.attemptBlocking:
      val container = GenericContainer(image)
      container.withNetworkMode("host")
      container.withCommand(sdk, mcpUrl)
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

  /**
   * The end-to-end checks each spec makes, against the client `sdk` names in probe.mjs. The aspects go after
   * `provide`: the server's handlers run on the clock the Server layer was built with, and a test clock would sign
   * webhooks with a 1970 `webhook-timestamp`, which the clients refuse.
   */
  def suite(name: String, sdk: String): Spec[Any, McpEventsError | Throwable] =
    zio.test.suite(name)(
      zio.test.test("subscribes, passes the challenge, receives the event in onEvent, and unsubscribes"):
        for run <- ZIO.scoped(this.run(sdk))
        yield assertTrue(
          run.startingWith("subscribed ").nonEmpty,
          run.startingWith("verification 200 ").nonEmpty,
          run.startingWith("delivery ").exists(_.startsWith("delivery 2")),
          run.has(s"""event $eventId $EventName {"id":"${incident.id}","severity":"${incident.severity}"}"""),
          run.has("unsubscribed"),
          run.startingWith("error ").isEmpty,
          run.leftOver.isEmpty,
        )
    ).provide(Server.defaultWith(_.onAnyOpenPort), McpServer.State.default) @@ TestAspect.withLiveClock @@ TestAspect.timeout(10.minutes) @@
      (if DockerSupport.useHostNetwork then TestAspect.identity else TestAspect.ignore)
