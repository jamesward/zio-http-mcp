package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.client.*
import zio.*
import zio.http.*
import zio.json.*
import zio.json.ast.Json
import zio.schema.*
import zio.stream.{ZPipeline, ZStream}
import zio.test.*
import zio.test.TestAspect.*

import java.io.{BufferedReader, InputStreamReader, OutputStreamWriter, PipedInputStream, PipedOutputStream}
import java.nio.charset.StandardCharsets
import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}

object McpEventsSpec extends ZIOSpecDefault:

  // --- the README example: events emitted into a buffer, filtered per subscriber ---

  case class Incident(id: String, severity: String, title: String) derives Schema
  case class IncidentFilter(severity: Option[String]) derives Schema

  val incidentsServer: UIO[(McpEventBuffer[Incident], Either[McpEventsError, McpServer[Any]])] =
    for incidents <- McpEventBuffer.make[Incident](maxAge = 10.minutes, maxEvents = 10000)
    yield
      val created = McpEvent("incident.created")
        .description("A new incident")
        .buffered[IncidentFilter, Incident](incidents, (filter, incident, _) =>
          Option.when(filter.severity.forall(_ == incident.severity))(incident))
      (incidents, McpEvents(created, EventFixtures.ticks).map(McpServer("incidents", "1.0.0").withExtensions(_)))

  // --- fixtures for the protocol details ---

  case class Count(n: Int) derives Schema

  /** Fails with `outcome` on every check after the first, which answers the cursor "0". */
  private def failing(name: String, outcome: McpEventError, checks: Ref[Int]): McpEventHandler[Any] =
    McpEvent(name).checkInterval(50.millis).check[Any, Unit, Count]: request =>
      checks.updateAndGet(_ + 1).flatMap: n =>
        if request.cursor.isEmpty then ZIO.succeed(McpEventBatch.empty(Some(McpEventCursor("0"))))
        else if n == 2 then ZIO.fail(outcome)
        else ZIO.succeed(McpEventBatch(Chunk(McpEventEmit(Count(n), eventId = Some(s"e$n"))), Some(McpEventCursor(n.toString))))

  /** Counts its checks, so that a test can see them stop. */
  private def counting(checks: Ref[Int]): McpEventHandler[Any] =
    McpEvent("counted").checkInterval(20.millis).check[Any, Unit, Count]: _ =>
      checks.update(_ + 1).as(McpEventBatch.empty(Some(McpEventCursor("0"))))

  private final case class Fixture(
    port: Int,
    incidents: McpEventBuffer[Incident],
    checks: Ref[Int],
  )

  private def install(heartbeat: Duration = McpEvents.DefaultHeartbeat): ZIO[Server & McpServer.State & Scope, Any, Fixture] =
    for
      incidents <- McpEventBuffer.make[Incident](maxEvents = 5)
      checks    <- Ref.make(0)
      upstream  <- Ref.make(0)
      forbidden <- Ref.make(0)
      created    = McpEvent("incident.created").pollInterval(2.seconds)
                     .buffered[IncidentFilter, Incident](incidents, (filter, incident, _) =>
                       Option.when(filter.severity.forall(_ == incident.severity))(incident))
      events    <- ZIO.fromEither(McpEvents.withHeartbeat(heartbeat)(
                     created,
                     failing("flaky", McpEventError.Upstream("UpstreamError", Some(Json.Obj("reason" -> Json.Str("503")))), upstream),
                     failing("revoked", McpEventError.Forbidden("Access revoked"), forbidden),
                     counting(checks),
                     McpEvent("poll-only").pollOnly.buffered(incidents),
                   ))
      port      <- Server.install(McpServer("events", "1.0.0").withExtensions(events).routes)
    yield Fixture(port, incidents, checks)

  // --- wire helpers ---

  private val Modern = ProtocolVersion.V2026_07_28.wire

  private def body(method: String, params: Json.Obj): String =
    val meta = Json.Obj(
      McpMeta.ProtocolVersion    -> Json.Str(Modern),
      McpMeta.ClientCapabilities -> Json.Obj("extensions" -> Json.Obj(McpEvents.Id.value -> Json.Obj())),
    )
    s"""{"jsonrpc":"2.0","id":7,"method":"$method","params":${(Json.Obj(params.fields :+ ("_meta" -> (meta: Json))): Json).toJson}}"""

  private def request(port: Int, method: String, params: Json.Obj, name: Option[String]): Request =
    val url = URL.decode(s"http://localhost:$port/mcp").toOption.get
    val base = Request.post(url, Body.fromString(body(method, params)))
      .addHeader(Header.ContentType(MediaType.application.json))
      .addHeader("accept", "application/json, text/event-stream")
      .addHeader(Negotiation.ProtocolVersionHeader, Modern)
      .addHeader(Negotiation.MethodHeader, method)
    name.fold(base)(n => base.addHeader(Negotiation.NameHeader, n))

  private def call(port: Int, method: String, params: Json.Obj, name: Option[String]): ZIO[Client & Scope, Throwable, (Int, Json.Obj)] =
    ZClient.batched(request(port, method, params, name)).flatMap: r =>
      r.body.asString.flatMap(s => ZIO.fromEither(s.fromJson[Json.Obj]).mapError(e => RuntimeException(s"$e: $s")))
        .map(r.status.code -> _)

  private def poll(port: Int, params: Json.Obj): ZIO[Client & Scope, Throwable, Json.Obj] =
    call(port, "events/poll", params, params.get("name").flatMap(_.asString)).map(_._2)

  /** An `events/stream` request's open SSE response. */
  private def openStream(port: Int, params: Json.Obj): ZIO[Client & Scope, Throwable, Response] =
    ZClient.streaming(request(port, "events/stream", params, params.get("name").flatMap(_.asString)))

  private def sseMessages(r: Response): ZStream[Any, Throwable, Json.Obj] =
    r.body.asStream
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
      .collect { case line if line.startsWith("data:") => line.drop(5).trim }
      .map(_.fromJson[Json.Obj])
      .collect { case Right(message) => message }

  private def field(json: Json, path: String*): Option[Json] =
    path.foldLeft(Option(json))((j, key) => j.flatMap(_.asObject).flatMap(_.get(key)))

  private def str(json: Json, path: String*): Option[String] = field(json, path*).flatMap(_.asString)
  private def method(m: Json.Obj): Option[String] = str(m, "method")
  private def code(m: Json.Obj): Option[Int] = field(m, "error", "code").flatMap(_.asNumber).map(_.value.intValue)
  private def subscriptionOf(m: Json.Obj): Option[Json] = field(m, "params", "_meta", McpMeta.SubscriptionId)
  private def events(m: Json.Obj): Chunk[Json] = field(m, "result", "events").flatMap(_.asArray).getOrElse(Chunk.empty)
  private def bool(m: Json.Obj, key: String): Option[Boolean] = field(m, "result", key).flatMap {
    case Json.Bool(b) => Some(b)
    case _            => None
  }

  private def subscription(name: String, arguments: Json.Obj = Json.Obj(), cursor: Option[String] = None): Json.Obj =
    Json.Obj(Chunk[(String, Json)]("name" -> Json.Str(name), "arguments" -> arguments) ++
      Chunk.fromIterable(cursor.map(c => "cursor" -> Json.Str(c))))

  private def incident(id: String, severity: String = "P1") = Incident(id, severity, s"incident $id")

  private def clientConfig(port: Int) = McpClientConfig(s"http://localhost:$port/mcp")

  private def eventsOf(signals: Chunk[McpEventSignal]): Chunk[McpEventOccurrence] =
    signals.collect { case McpEventSignal.Event(e) => e }

  // --- stdio: a server on in-process pipes ---

  private final class Pipes(toServer: OutputStreamWriter, fromServer: BufferedReader):
    private val received = LinkedBlockingQueue[Json.Obj]()
    private val reader = Thread(() =>
      Iterator.continually(fromServer.readLine()).takeWhile(_ != null).foreach(line =>
        line.fromJson[Json.Obj].foreach(received.put)))
    reader.setDaemon(true)
    reader.start()

    def send(line: String): UIO[Unit] = ZIO.succeedBlocking { toServer.write(line + "\n"); toServer.flush() }
    def next: Task[Json.Obj] =
      ZIO.attemptBlocking(Option(received.poll(10, TimeUnit.SECONDS)).getOrElse(throw AssertionError("no message")))
    def quiet(millis: Long): Task[Chunk[Json.Obj]] =
      ZIO.attemptBlocking { Thread.sleep(millis); val l = java.util.ArrayList[Json.Obj](); received.drainTo(l); Chunk.fromJavaIterable(l) }

  private def pipes(server: McpServer[Any]): ZIO[Scope, Throwable, Pipes] =
    for
      clientOut <- ZIO.succeed(PipedOutputStream())
      serverIn  <- ZIO.succeed(PipedInputStream(clientOut, 65536))
      serverOut <- ZIO.succeed(PipedOutputStream())
      clientIn  <- ZIO.succeed(PipedInputStream(serverOut, 65536))
      _         <- server.serveStream(serverIn, serverOut).ensuring(ZIO.succeed(serverOut.close())).forkScoped
    yield Pipes(OutputStreamWriter(clientOut, StandardCharsets.UTF_8), BufferedReader(InputStreamReader(clientIn, StandardCharsets.UTF_8)))

  private def stdioServer: Task[McpStdioConfig] =
    for
      javaHome  <- System.property("java.home").someOrFail(IllegalStateException("java.home is not set"))
      classpath <- System.property("test.classpath").someOrFail(IllegalStateException("test.classpath is not set (see build.sbt)"))
    yield McpStdioConfig(s"$javaHome/bin/java", Chunk("-cp", classpath, "com.jamesward.ziohttp.mcp.StdioServerMain"),
      discoverTimeout = 30.seconds)

  def spec = suite("McpEventsSpec")(
    suite("README")(
      test("a buffered event type, received by push through the client, filtered by its arguments"):
        ZIO.scoped:
          for
            (incidents, built) <- incidentsServer
            server             <- ZIO.fromEither(built)
            port               <- Server.install(server.routes)
            client             <- McpEventsClient.connect(clientConfig(port))
            signals            <- client.subscribe(McpEventName("incident.created"), Json.Obj("severity" -> Json.Str("P1")))
                                    .tap:
                                      // Subscribed from now: emit once the subscription is live.
                                      case McpEventSignal.Active(_, _) =>
                                        incidents.emit(incident("INC-1", "P2")) *>
                                          incidents.emit(incident("INC-2"), eventId = Some("INC-2"))
                                      case _ => ZIO.unit
                                    .takeUntil:
                                      case McpEventSignal.Event(_) => true
                                      case _                       => false
                                    .runCollect
          yield
            val received = eventsOf(signals)
            assertTrue(
              received.map(_.eventId) == Chunk("INC-2"),
              received.head.data.toJson == """{"id":"INC-2","severity":"P1","title":"incident INC-2"}""",
              received.head.cursor.isDefined,
            )
      ,
      test("a check-function event type, received by polling through the client"):
        ZIO.scoped:
          for
            (_, built) <- incidentsServer
            server     <- ZIO.fromEither(built)
            port       <- Server.install(server.routes)
            client     <- McpEventsClient.connect(clientConfig(port))
            signals    <- client.subscribe(McpEventName("clock.tick"), delivery = Some(McpEventDelivery.Poll))
                            .filter { case McpEventSignal.Event(_) => true; case _ => false }
                            .take(2).runCollect
          yield
            val received = eventsOf(signals)
            val seconds = received.flatMap(e => field(e.data, "second")).map(_.toJson)
            assertTrue(
              received.size == 2,
              received(1).eventId.toLong == received(0).eventId.toLong + 1,
              seconds == received.map(_.eventId),
            )
      ,
    ),
    suite("discovery")(
      test("server/discover advertises the extension, and events/list its event types"):
        ZIO.scoped:
          for
            fx            <- install()
            (_, discover) <- call(fx.port, "server/discover", Json.Obj(), None)
            (_, listed)   <- call(fx.port, "events/list", Json.Obj(), None)
          yield
            val descriptors = field(listed, "result", "events").flatMap(_.asArray).getOrElse(Chunk.empty)
            val incident = descriptors.find(d => str(d, "name").contains("incident.created"))
            assertTrue(
              field(discover, "result", "capabilities", "extensions", McpEvents.Id.value).contains(Json.Obj()),
              str(listed, "result", "resultType").contains("complete"),
              descriptors.size == 5,
              descriptors.find(d => str(d, "name").contains("poll-only")).flatMap(field(_, "delivery")).contains(Json.Arr(Json.Str("poll"))),
              incident.flatMap(field(_, "delivery")).contains(Json.Arr(Json.Str("poll"), Json.Str("push"))),
              incident.flatMap(field(_, "inputSchema", "properties", "severity")).isDefined,
              incident.flatMap(field(_, "payloadSchema", "properties", "title")).isDefined,
            )
      ,
      test("the client reads the event types"):
        ZIO.scoped:
          for
            fx     <- install()
            client <- McpEventsClient.connect(clientConfig(fx.port))
            all    <- client.listAll
          yield assertTrue(
            all.map(_.name.value).toSet == Set("incident.created", "flaky", "revoked", "counted", "poll-only"),
            all.filterNot(_.name.value == "poll-only").forall(_.delivery.toChunk == Chunk(McpEventDelivery.Poll, McpEventDelivery.Push)),
          )
      ,
    ),
    suite("poll")(
      test("a null cursor starts from now; the next poll has what was emitted since, without per-event cursors"):
        ZIO.scoped:
          for
            fx      <- install()
            _       <- fx.incidents.emit(incident("before"))
            first   <- poll(fx.port, subscription("incident.created"))
            cursor   = str(first, "result", "cursor").get
            _       <- fx.incidents.emit(incident("INC-1"), eventId = Some("INC-1"))
            second  <- poll(fx.port, subscription("incident.created", cursor = Some(cursor)))
          yield
            val event = events(second).head
            assertTrue(
              events(first).isEmpty,
              str(first, "result", "resultType").contains("complete"),
              field(first, "result", "nextPollMs").contains(Json.Num(2000)),
              events(second).size == 1,
              str(event, "eventId").contains("INC-1"),
              str(event, "name").contains("incident.created"),
              str(event, "timestamp").exists(_.contains("T")),
              str(event, "data", "id").contains("INC-1"),
              field(event, "cursor").isEmpty,
              str(second, "result", "cursor").exists(_ != cursor),
              bool(second, "truncated").contains(false),
              bool(second, "hasMore").contains(false),
            )
      ,
      test("maxEvents caps a batch and sets hasMore; arguments filter what is delivered"):
        ZIO.scoped:
          for
            fx      <- install()
            start   <- poll(fx.port, subscription("incident.created")).map(str(_, "result", "cursor").get)
            _       <- ZIO.foreachDiscard(Chunk("1" -> "P1", "2" -> "P2", "3" -> "P1", "4" -> "P1"))((id, sev) => fx.incidents.emit(incident(id, sev), eventId = Some(id)))
            p1       = Json.Obj("severity" -> Json.Str("P1"))
            capped  <- poll(fx.port, Json.Obj(subscription("incident.created", p1, Some(start)).fields :+ ("maxEvents" -> Json.Num(2))))
            rest    <- poll(fx.port, subscription("incident.created", p1, str(capped, "result", "cursor")))
          yield assertTrue(
            events(capped).flatMap(str(_, "eventId")) == Chunk("1", "3"),
            bool(capped, "hasMore").contains(true),
            events(rest).flatMap(str(_, "eventId")) == Chunk("4"),
            bool(rest, "hasMore").contains(false),
          )
      ,
      test("a cursor the buffer no longer covers, or another buffer's, resumes from what it holds, truncated"):
        ZIO.scoped:
          for
            fx       <- install()
            start    <- poll(fx.port, subscription("incident.created")).map(str(_, "result", "cursor").get)
            _        <- ZIO.foreachDiscard(1 to 7)(i => fx.incidents.emit(incident(i.toString), eventId = Some(i.toString)))
            behind   <- poll(fx.port, subscription("incident.created", cursor = Some(start)))
            foreign  <- poll(fx.port, subscription("incident.created", cursor = Some("restarted:3")))
          yield assertTrue(
            bool(behind, "truncated").contains(true),
            events(behind).flatMap(str(_, "eventId")) == Chunk("3", "4", "5", "6", "7"),
            bool(foreign, "truncated").contains(true),
          )
      ,
      test("maxAgeMs skips older events, with truncated"):
        ZIO.scoped:
          for
            fx     <- install()
            start  <- poll(fx.port, subscription("incident.created")).map(str(_, "result", "cursor").get)
            now    <- Clock.instant
            _      <- fx.incidents.emit(incident("old"), eventId = Some("old"), timestamp = Some(now.minusSeconds(120)))
            _      <- fx.incidents.emit(incident("new"), eventId = Some("new"))
            recent <- poll(fx.port, Json.Obj(subscription("incident.created", cursor = Some(start)).fields :+ ("maxAgeMs" -> Json.Num(60000))))
          yield assertTrue(
            events(recent).flatMap(str(_, "eventId")) == Chunk("new"),
            bool(recent, "truncated").contains(true),
          )
      ,
      test("errors: unknown event type -32023, invalid arguments -32602, Mcp-Name missing or wrong 400 -32020"):
        ZIO.scoped:
          for
            fx                  <- install()
            (_, unknown)        <- call(fx.port, "events/poll", subscription("nope"), Some("nope"))
            (_, invalid)        <- call(fx.port, "events/poll", subscription("incident.created", Json.Obj("severity" -> Json.Num(1))), Some("incident.created"))
            (missingStatus, missing)   <- call(fx.port, "events/poll", subscription("incident.created"), None)
            (wrongStatus, wrong)       <- call(fx.port, "events/poll", subscription("incident.created"), Some("other"))
            (streamStatus, _)   <- call(fx.port, "events/stream", subscription("incident.created"), None)
          yield assertTrue(
            code(unknown).contains(-32023),
            field(unknown, "error", "data", "kind").contains(Json.Str("event")),
            code(invalid).contains(-32602),
            missingStatus == 400, code(missing).contains(-32020),
            wrongStatus == 400, code(wrong).contains(-32020),
            streamStatus == 400,
          )
      ,
    ),
    suite("push")(
      test("active, then events and heartbeats, each with the subscription id"):
        ZIO.scoped:
          for
            fx       <- install(heartbeat = 300.millis)
            response <- openStream(fx.port, subscription("incident.created", Json.Obj("severity" -> Json.Str("P1"))))
            messages <- sseMessages(response)
                          .tap(m => ZIO.when(method(m).contains("notifications/events/active"))(
                            fx.incidents.emit(incident("INC-9"), eventId = Some("INC-9")) *> fx.incidents.emit(incident("P2", "P2"))))
                          .takeUntil(m => method(m).contains("notifications/events/heartbeat"))
                          .runCollect
          yield
            val event = messages.find(m => method(m).contains("notifications/events/event"))
            val heartbeat = messages.last
            assertTrue(
              response.header(Header.ContentType).exists(_.mediaType.matches(MediaType.text.`event-stream`)),
              method(messages.head).contains("notifications/events/active"),
              field(messages.head, "params", "truncated").contains(Json.Bool(false)),
              messages.forall(subscriptionOf(_).contains(Json.Num(7))),
              messages.count(m => method(m).contains("notifications/events/event")) == 1,
              event.flatMap(str(_, "params", "eventId")).contains("INC-9"),
              event.flatMap(str(_, "params", "data", "id")).contains("INC-9"),
              event.flatMap(str(_, "params", "cursor")).isDefined,
              // The heartbeat's cursor is past the P2 incident too, which this subscriber skipped.
              str(heartbeat, "params", "cursor") == event.flatMap(str(_, "params", "cursor")).map(_.replaceAll(":1$", ":2")),
            )
      ,
      test("replaying from a cursor delivers the backlog after active"):
        ZIO.scoped:
          for
            fx       <- install()
            start    <- poll(fx.port, subscription("incident.created")).map(str(_, "result", "cursor").get)
            _        <- fx.incidents.emit(incident("A"), eventId = Some("A")) *> fx.incidents.emit(incident("B"), eventId = Some("B"))
            response <- openStream(fx.port, subscription("incident.created", cursor = Some(start)))
            messages <- sseMessages(response).take(3).runCollect
          yield assertTrue(
            method(messages.head).contains("notifications/events/active"),
            str(messages.head, "params", "cursor").contains(start),
            messages.drop(1).flatMap(str(_, "params", "eventId")) == Chunk("A", "B"),
          )
      ,
      test("an upstream error is reported and the stream goes on; a terminating one ends it with the final result"):
        ZIO.scoped:
          for
            fx      <- install()
            flaky   <- openStream(fx.port, subscription("flaky")).flatMap(sseMessages(_).take(3).runCollect)
            revoked <- openStream(fx.port, subscription("revoked")).flatMap(sseMessages(_).runCollect)
          yield
            val error = flaky(1)
            val terminated = revoked(1)
            val last = revoked.last
            assertTrue(
              flaky.flatMap(method) == Chunk("notifications/events/active", "notifications/events/error", "notifications/events/event"),
              field(error, "params", "error", "code").contains(Json.Num(-32603)),
              str(error, "params", "error", "data", "reason").contains("503"),
              revoked.size == 3,
              method(terminated).contains("notifications/events/terminated"),
              field(terminated, "params", "error", "code").contains(Json.Num(-32024)),
              field(last, "id").contains(Json.Num(7)),
              str(last, "result", "resultType").contains("complete"),
              field(last, "result", "_meta", McpMeta.SubscriptionId).contains(Json.Num(7)),
            )
      ,
      test("an invalid subscription is a JSON-RPC error, before any stream opens; so is one for a type without push"):
        ZIO.scoped:
          for
            fx            <- install()
            (_, body)     <- call(fx.port, "events/stream", subscription("nope"), Some("nope"))
            (_, pollOnly) <- call(fx.port, "events/stream", subscription("poll-only"), Some("poll-only"))
            (_, polled)   <- call(fx.port, "events/poll", subscription("poll-only"), Some("poll-only"))
          yield assertTrue(
            code(body).contains(-32023),
            code(pollOnly).contains(-32026),
            field(pollOnly, "error", "data").map(_.toJson).contains("""{"feature":"deliveryMode","value":"push"}"""),
            str(polled, "result", "resultType").contains("complete"),
          )
      ,
      test("closing the stream ends the subscription"):
        ZIO.scoped:
          for
            fx      <- install()
            _       <- ZIO.scoped(openStream(fx.port, subscription("counted")).flatMap(sseMessages(_).take(1).runDrain))
            _       <- ZIO.sleep(300.millis)
            before  <- fx.checks.get
            _       <- ZIO.sleep(300.millis)
            after   <- fx.checks.get
          yield assertTrue(before > 0, after == before)
      ,
      test("the client's subscribe fails with the error that terminated the subscription"):
        ZIO.scoped:
          for
            fx     <- install()
            client <- McpEventsClient.connect(clientConfig(fx.port))
            exit   <- client.subscribe(McpEventName("revoked")).runCollect.exit
          yield assertTrue(exit match
            case Exit.Failure(cause) => cause.failureOption.exists {
              case McpClientError.JsonRpc(code, _, _) => code == -32024
              case _                                  => false
            }
            case _ => false
          )
      ,
    ),
    suite("stdio")(
      test("events/stream on stdio: notifications carry the subscription id, and cancelling stops them"):
        ZIO.scoped:
          for
            incidents <- McpEventBuffer.make[Incident]()
            events    <- ZIO.fromEither(McpEvents(McpEvent("incident.created").buffered(incidents)))
            p         <- pipes(McpServer("stdio-events", "1.0.0").withExtensions(events))
            meta       = s""""_meta":{"${McpMeta.ProtocolVersion}":"$Modern"}"""
            _         <- p.send(s"""{"jsonrpc":"2.0","id":"sub-1","method":"events/stream","params":{"name":"incident.created",$meta}}""")
            active    <- p.next
            _         <- incidents.emit(incident("S1"), eventId = Some("S1"))
            event     <- p.next
            _         <- p.send("""{"jsonrpc":"2.0","method":"notifications/cancelled","params":{"requestId":"sub-1"}}""")
            _         <- ZIO.sleep(200.millis)
            _         <- incidents.emit(incident("S2"))
            after     <- p.quiet(300)
          yield assertTrue(
            method(active).contains("notifications/events/active"),
            subscriptionOf(active).contains(Json.Str("sub-1")),
            str(event, "params", "eventId").contains("S1"),
            subscriptionOf(event).contains(Json.Str("sub-1")),
            after.isEmpty,
          )
      ,
      test("the client subscribes by push to a stdio server"):
        ZIO.scoped:
          for
            config  <- stdioServer
            client  <- McpEventsClient.connectStdio(config)
            signals <- client.subscribe(McpEventName("clock.tick"))
                         .filter { case McpEventSignal.Event(_) => true; case _ => false }
                         .take(2).runCollect
          yield
            val received = eventsOf(signals)
            assertTrue(received.size == 2, received.forall(_.cursor.isDefined))
      ,
    ),
  ).provide(Server.defaultWith(_.onAnyOpenPort), Client.default, McpServer.State.default) @@
    withLiveClock @@ withLiveSystem @@ timeout(2.minutes) @@ sequential
