package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.auth.*
import zio.*
import zio.http.*
import zio.json.*
import zio.json.ast.Json
import zio.schema.*
import zio.test.*
import zio.test.TestAspect.*

import java.net.InetAddress
import java.nio.charset.StandardCharsets.UTF_8
import java.time.Instant
import java.util.Base64
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

object McpWebhooksSpec extends ZIOSpecDefault:

  case class Incident(id: String, severity: String, assignee: Option[String] = None) derives Schema
  case class IncidentFilter(severity: Option[String]) derives Schema
  case class Count(n: Int) derives Schema

  private val Resource = "https://mcp.test"

  // The bearer token is the caller's subject.
  private val verifier: TokenVerifier[Any] =
    TokenVerifier.fromFunction: raw =>
      ZIO.succeed(Principal(Some(raw), None, Set.empty, Set(Resource), Some("https://auth.test"), None, raw, Json.Obj()))

  private def secret(fill: Int): String = "whsec_" + Base64.getEncoder.encodeToString(Array.fill(32)(fill.toByte))
  private val secretA = secret(1)
  private val secretB = secret(2)

  private val testConfig = McpWebhookConfig(
    minTtl = 1.second,
    retryDelays = Chunk(100.millis, 100.millis),
    expiryCheck = 100.millis,
    targets = McpWebhookTargets.unrestricted,
  )

  // --- the receiving end ---

  final case class Received(path: String, headers: Map[String, String], body: String):
    def header(name: String): Option[String] = headers.get(name.toLowerCase)
    def json: Json.Obj = body.fromJson[Json.Obj].toOption.get
    def kind: Option[String] = json.get("type").flatMap(_.asString)
    def str(name: String): Option[String] = json.get(name).flatMap(_.asString)

    /** Whether one of its signatures verifies with `secret`, recomputed here rather than with the code under test. */
    def signedWith(secret: String): Boolean =
      val mac = Mac.getInstance("HmacSHA256")
      mac.init(SecretKeySpec(Base64.getDecoder.decode(secret.drop("whsec_".length)), "HmacSHA256"))
      val expected = "v1," + Base64.getEncoder.encodeToString(
        mac.doFinal(s"${header("webhook-id").get}.${header("webhook-timestamp").get}.$body".getBytes(UTF_8)))
      header("webhook-signature").exists(_.split(' ').contains(expected))

  final case class Receiver(received: Queue[Received], echo: Ref[Boolean], statuses: Ref[List[Int]]):
    val routes: Routes[Any, Response] = Routes(
      Method.POST / "hooks" / string("name") -> handler: (name: String, request: Request) =>
        for
          body    <- request.body.asString.orDie
          got      = Received(name, request.headers.toList.map(h => h.headerName.toLowerCase -> h.renderedValue).toMap, body)
          _       <- received.offer(got)
          echoing <- echo.get
          status  <- statuses.modify(s => (s.headOption.getOrElse(200), s.drop(1)))
        yield got.kind match
          case Some("verification") if echoing =>
            Response.json(Json.Obj("challenge" -> Json.Str(got.str("challenge").get)).toJson)
          case Some("verification") => Response.ok
          case _                    => Response.status(Status.fromInt(status))
    )

    def next: Task[Received] = received.take.timeoutFail(RuntimeException("no webhook delivery"))(5.seconds)

    /** What arrives within `millis`. */
    def quiet(millis: Long): UIO[Chunk[Received]] = ZIO.sleep(millis.millis) *> received.takeAll

  object Receiver:
    def make: UIO[Receiver] =
      for
        q        <- Queue.unbounded[Received]
        echo     <- Ref.make(true)
        statuses <- Ref.make(List.empty[Int])
      yield Receiver(q, echo, statuses)

  // --- the server under test ---

  final case class Fixture(port: Int, incidents: McpEventBuffer[Incident], store: TestWebhookStore, receiver: Receiver):
    def url(name: String = "a"): String = s"http://localhost:$port/hooks/$name"

  private def install(
    config: McpWebhookConfig = testConfig,
    durable: Boolean = false,
    authenticated: Boolean = true,
    before: (Int, TestWebhookStore) => UIO[Unit] = (_, _) => ZIO.unit,
  ): ZIO[Server & McpServer.State & Scope, Any, Fixture] =
    for
      receiver  <- Receiver.make
      port      <- Server.install(receiver.routes)
      store     <- TestWebhookStore.make(durable)
      _         <- before(port, store)
      incidents <- McpEventBuffer.make[Incident]()
      checks    <- Ref.make(0)
      created    = McpEvent("incident.created").buffered[IncidentFilter, Incident](incidents, (filter, incident, _) =>
                     Option.when(filter.severity.forall(_ == incident.severity))(incident))
      // Fails with Forbidden on its third check.
      revoked    = McpEvent("revoked").checkInterval(50.millis).check[Any, Unit, Count]: request =>
                     checks.updateAndGet(_ + 1).flatMap: n =>
                       if request.cursor.isEmpty then ZIO.succeed(McpEventBatch.empty(Some(McpEventCursor("0"))))
                       else if n >= 3 then ZIO.fail(McpEventError.Forbidden("Access revoked"))
                       else ZIO.succeed(McpEventBatch.empty(Some(McpEventCursor(n.toString))))
      events    <- McpEvents.withWebhooks(config)(created, revoked, McpEvent("poll-only").pollOnly.buffered(incidents))
                     .provideSomeEnvironment[Scope](_.add[McpWebhookStore](store))
      base       = McpServer("hooks", "1.0.0").withExtensions(events)
      server     = if authenticated then base.auth(McpAuth(
                     authorizationServers = NonEmptyChunk(AuthorizationServer("https://auth.test")),
                     verifier = verifier,
                     resourceUri = Some(ResourceUri.unsafe(Resource)),
                   )) else base
      _         <- Server.install(server.routes)
    yield Fixture(port, incidents, store, receiver)

  // --- wire helpers ---

  private val Modern = ProtocolVersion.V2026_07_28.wire

  private def call(port: Int, method: String, params: Json.Obj, token: Option[String] = Some("alice")): ZIO[Client & Scope, Throwable, Json.Obj] =
    val meta = Json.Obj(
      McpMeta.ProtocolVersion    -> Json.Str(Modern),
      McpMeta.ClientCapabilities -> Json.Obj("extensions" -> Json.Obj(McpEvents.Id.value -> Json.Obj())),
    )
    val body = s"""{"jsonrpc":"2.0","id":7,"method":"$method","params":${(Json.Obj(params.fields :+ ("_meta" -> (meta: Json))): Json).toJson}}"""
    val base = Request.post(URL.decode(s"http://localhost:$port/mcp").toOption.get, Body.fromString(body))
      .addHeader(Header.ContentType(MediaType.application.json))
      .addHeader("accept", "application/json, text/event-stream")
      .addHeader(Negotiation.ProtocolVersionHeader, Modern)
      .addHeader(Negotiation.MethodHeader, method)
    val named = params.get("name").flatMap(_.asString).fold(base)(n => base.addHeader(Negotiation.NameHeader, n))
    val authed = token.fold(named)(t => named.addHeader(Header.Authorization.Bearer(t)))
    ZClient.batched(authed).flatMap(_.body.asString).flatMap(s => ZIO.fromEither(s.fromJson[Json.Obj]).mapError(RuntimeException(_)))

  private def subscription(
    url: String,
    secret: String = secretA,
    name: String = "incident.created",
    arguments: Json.Obj = Json.Obj("severity" -> Json.Str("P1")),
    ttl: Option[Json] = None,
  ): Json.Obj =
    Json.Obj(Chunk[(String, Json)](
      "name"      -> Json.Str(name),
      "arguments" -> arguments,
      "delivery"  -> Json.Obj("mode" -> Json.Str("webhook"), "url" -> Json.Str(url), "secret" -> Json.Str(secret)),
    ) ++ Chunk.fromIterable(ttl.map("ttlMs" -> _)))

  private def unsubscription(url: String, name: String = "incident.created"): Json.Obj =
    Json.Obj("name" -> Json.Str(name), "arguments" -> Json.Obj("severity" -> Json.Str("P1")),
      "delivery" -> Json.Obj("url" -> Json.Str(url)))

  private def field(json: Json, path: String*): Option[Json] =
    path.foldLeft(Option(json))((acc, key) => acc.flatMap(_.asObject).flatMap(_.get(key)))
  private def str(json: Json, path: String*): Option[String] = field(json, path*).flatMap(_.asString)
  private def code(m: Json.Obj): Option[Int] = field(m, "error", "code").flatMap(_.asNumber).map(_.value.intValue)

  /** Subscribe, and take the verification POST that comes with a first subscription. */
  private def subscribed(fx: Fixture, params: Json.Obj): ZIO[Client & Scope, Throwable, (Json.Obj, Received)] =
    for
      result       <- call(fx.port, "events/subscribe", params)
      verification <- fx.receiver.next
    yield (result, verification)

  // The README example.
  val webhookEvents: ZIO[McpWebhookStore & Scope, McpEventsError, McpExtensions[Any]] =
    for
      incidents <- McpEventBuffer.make[Incident]()
      created    = McpEvent("incident.created")
                     .buffered[IncidentFilter, Incident](incidents, (filter, incident, _) =>
                       Option.when(filter.severity.forall(_ == incident.severity))(incident))
      events    <- McpEvents.withWebhooks(McpWebhookConfig(maxTtl = 1.hour))(created)
    yield events

  def spec = suite("McpWebhooksSpec")(
    test("README: withWebhooks over a store layer offers webhook delivery"):
      ZIO.scoped:
        for
          store  <- TestWebhookStore.make()
          events <- webhookEvents.provideSomeEnvironment[Scope](_.add[McpWebhookStore](store))
          port   <- Server.install(McpServer("incidents", "1.0.0").withExtensions(events).routes)
          listed <- call(port, "events/list", Json.Obj(), token = None)
        yield assertTrue(field(listed, "result", "events").flatMap(_.asArray).flatMap(_.headOption)
          .flatMap(field(_, "delivery")).map(_.toJson).contains("""["poll","push","webhook"]"""))
    ,
    test("events/list offers webhook for the event types that offer push"):
      ZIO.scoped:
        for
          fx     <- install()
          listed <- call(fx.port, "events/list", Json.Obj())
        yield
          val delivery = field(listed, "result", "events").flatMap(_.asArray).getOrElse(Chunk.empty)
            .map(d => str(d, "name").get -> field(d, "delivery").get.toJson).toMap
          assertTrue(
            delivery("incident.created") == """["poll","push","webhook"]""",
            delivery("poll-only") == """["poll"]""",
          )
    ,
    test("a subscription is verified, then gets its matching events, signed, with the watermark"):
      ZIO.scoped:
        for
          fx                     <- install()
          (result, verification) <- subscribed(fx, subscription(fx.url()))
          id                      = str(result, "result", "id").get
          _                      <- fx.incidents.emit(Incident("INC-1", "P2"))
          _                      <- fx.incidents.emit(Incident("INC-2", "P1"), eventId = Some("INC-2"))
          delivery               <- fx.receiver.next
          stored                 <- fx.store.all
        yield assertTrue(
          verification.kind.contains("verification"),
          verification.header("webhook-id").exists(_.startsWith("msg_verification_")),
          verification.header("x-mcp-subscription-id").contains(id),
          verification.signedWith(secretA),
          id.startsWith("sub_"),
          str(result, "result", "refreshBefore").exists(t => Instant.parse(t).isAfter(Instant.now().plusSeconds(3000))),
          field(result, "result", "cursor").exists(_.asString.isDefined),
          delivery.str("eventId").contains("INC-2"),
          delivery.json.get("cursor").flatMap(_.asString).isDefined,
          delivery.header("x-mcp-subscription-id").contains(id),
          delivery.header("webhook-id").exists(w => w.startsWith("msg_") && !w.contains("INC-2")),
          delivery.header("content-type").exists(_.startsWith("application/json")),
          delivery.signedWith(secretA),
          // A None field is left out, as the payload schema describes it, not written as null.
          field(delivery.json, "data").map(_.toJson).contains("""{"id":"INC-2","severity":"P1"}"""),
          stored.map(_.id.value) == Chunk(id),
        )
    ,
    test("refreshing keeps the id, skips verification, reports delivery status, and dual-signs after a new secret"):
      ZIO.scoped:
        for
          fx          <- install()
          (first, _)  <- subscribed(fx, subscription(fx.url()))
          _           <- fx.incidents.emit(Incident("INC-1", "P1"))
          _           <- fx.receiver.next
          refreshed   <- call(fx.port, "events/subscribe", subscription(fx.url(), secret = secretB))
          _           <- fx.incidents.emit(Incident("INC-2", "P1"))
          rotated     <- fx.receiver.next
          // Another subscription for the same principal and URL needs no new verification either.
          other       <- call(fx.port, "events/subscribe",
                           subscription(fx.url(), arguments = Json.Obj("severity" -> Json.Str("P2"))))
          extra       <- fx.receiver.quiet(300)
        yield assertTrue(
          str(refreshed, "result", "id") == str(first, "result", "id"),
          field(refreshed, "result", "deliveryStatus", "active").contains(Json.Bool(true)),
          str(refreshed, "result", "deliveryStatus", "lastDeliveryAt").isDefined,
          field(refreshed, "result", "deliveryStatus", "lastError").contains(Json.Null),
          rotated.str("eventId").isDefined,
          rotated.signedWith(secretA),
          rotated.signedWith(secretB),
          str(other, "result", "id").exists(_ != str(first, "result", "id").get),
          extra.isEmpty,
        )
    ,
    test("a failed delivery is retried with the same webhook-id; 410 is not retried"):
      ZIO.scoped:
        for
          fx      <- install()
          _       <- subscribed(fx, subscription(fx.url()))
          _       <- fx.receiver.statuses.set(List(500))
          _       <- fx.incidents.emit(Incident("INC-1", "P1"))
          failed  <- fx.receiver.next
          retried <- fx.receiver.next
          _       <- fx.receiver.statuses.set(List(410))
          _       <- fx.incidents.emit(Incident("INC-2", "P1"))
          gone    <- fx.receiver.next
          after   <- fx.receiver.quiet(400)
        yield assertTrue(
          failed.header("webhook-id") == retried.header("webhook-id"),
          retried.signedWith(secretA),
          gone.str("eventId").isDefined,
          after.isEmpty,
        )
    ,
    test("an endpoint that doesn't echo the challenge is refused with CallbackEndpointError"):
      ZIO.scoped:
        for
          fx     <- install()
          _      <- fx.receiver.echo.set(false)
          result <- call(fx.port, "events/subscribe", subscription(fx.url()))
          stored <- fx.store.all
        yield assertTrue(
          code(result).contains(-32027),
          str(result, "error", "data", "reason").contains("challenge_failed"),
          stored.isEmpty,
        )
    ,
    test("unsubscribing stops deliveries; a second unsubscribe finds no subscription"):
      ZIO.scoped:
        for
          fx     <- install()
          _      <- subscribed(fx, subscription(fx.url()))
          first  <- call(fx.port, "events/unsubscribe", unsubscription(fx.url()))
          _      <- fx.incidents.emit(Incident("INC-1", "P1"))
          after  <- fx.receiver.quiet(300)
          second <- call(fx.port, "events/unsubscribe", unsubscription(fx.url()))
          stored <- fx.store.all
        yield assertTrue(
          str(first, "result", "resultType").contains("complete"),
          after.isEmpty,
          code(second).contains(-32023),
          str(second, "error", "data", "kind").contains("subscription"),
          stored.isEmpty,
        )
    ,
    test("a subscription that isn't refreshed expires"):
      ZIO.scoped:
        for
          fx     <- install()
          (r, _) <- subscribed(fx, subscription(fx.url(), ttl = Some(Json.Num(1000))))
          _      <- ZIO.sleep(1500.millis)
          stored <- fx.store.all
          _      <- fx.incidents.emit(Incident("INC-1", "P1"))
          after  <- fx.receiver.quiet(300)
        yield assertTrue(
          str(r, "result", "refreshBefore").exists(t => Instant.parse(t).isBefore(Instant.now())),
          stored.isEmpty,
          after.isEmpty,
        )
    ,
    test("a check function's Forbidden ends the subscription with a terminated envelope"):
      ZIO.scoped:
        for
          fx         <- install()
          _          <- subscribed(fx, subscription(fx.url(), name = "revoked", arguments = Json.Obj()))
          terminated <- fx.receiver.next
          _          <- ZIO.sleep(100.millis)
          stored     <- fx.store.all
        yield assertTrue(
          terminated.kind.contains("terminated"),
          terminated.header("webhook-id").exists(_.startsWith("msg_terminated_")),
          field(terminated.json, "error", "code").contains(Json.Num(-32024)),
          stored.isEmpty,
        )
    ,
    test("subscriptions in the store resume delivering when the server starts"):
      def seed(port: Int, store: TestWebhookStore): UIO[Unit] =
        val url = McpWebhookUrl.parse(s"http://localhost:$port/hooks/resumed").toOption.get
        val owner = McpWebhookPrincipalId("https://auth.test|sub:alice")
        val key = McpWebhookKey(owner, url, McpEventName("incident.created"), Json.Obj())
        val principal = Principal(Some("alice"), None, Set.empty, Set(Resource), Some("https://auth.test"), None, "", Json.Obj())
        store.put(McpWebhookSubscription(key, McpWebhookSecret.parse(secretA).toOption.get, None, principal,
          Some(Instant.now().plusSeconds(600)), None)).orDieWith(e => RuntimeException(e.message))
      ZIO.scoped:
        for
          fx       <- install(before = seed)
          _        <- ZIO.sleep(200.millis)
          _        <- fx.incidents.emit(Incident("INC-9", "P3"))
          delivery <- fx.receiver.next
        yield assertTrue(delivery.path == "resumed", delivery.str("eventId").isDefined, delivery.signedWith(secretA))
    ,
    test("no expiry is granted only from a durable store, and only when configured"):
      ZIO.scoped:
        for
          volatile <- install(config = testConfig.copy(noExpiry = true))
          (v, _)   <- subscribed(volatile, subscription(volatile.url(), ttl = Some(Json.Null)))
          durable  <- install(config = testConfig.copy(noExpiry = true), durable = true)
          (d, _)   <- subscribed(durable, subscription(durable.url(), ttl = Some(Json.Null)))
        yield assertTrue(
          str(v, "result", "refreshBefore").isDefined,
          field(d, "result", "refreshBefore").contains(Json.Null),
        )
    ,
    suite("refusals")(
      test("an unauthenticated caller is Forbidden"):
        ZIO.scoped:
          for
            fx     <- install(authenticated = false)
            result <- call(fx.port, "events/subscribe", subscription(fx.url()), token = None)
          yield assertTrue(code(result).contains(-32024))
      ,
      test("a secret that isn't whsec_ of 24 to 64 bytes is InvalidParams"):
        ZIO.scoped:
          for
            fx    <- install()
            short <- call(fx.port, "events/subscribe", subscription(fx.url(), secret = "whsec_" + Base64.getEncoder.encodeToString(Array.fill(16)(1.toByte))))
            plain <- call(fx.port, "events/subscribe", subscription(fx.url(), secret = "hunter2"))
          yield assertTrue(code(short).contains(-32602), code(plain).contains(-32602))
      ,
      test("by default, an http URL is InvalidParams and a host that resolves to a local address can't be reached"):
        ZIO.scoped:
          for
            fx     <- install(config = testConfig.copy(targets = McpWebhookTargets.public))
            http   <- call(fx.port, "events/subscribe", subscription(fx.url()))
            local  <- call(fx.port, "events/subscribe", subscription(s"https://localhost:${fx.port}/hooks/a"))
            posted <- fx.receiver.quiet(100)
          yield assertTrue(
            code(http).contains(-32602),
            code(local).contains(-32027),
            str(local, "error", "data", "reason").contains("connection_refused"),
            posted.isEmpty,
          )
      ,
      test("a poll-only event type is Unsupported for webhook"):
        ZIO.scoped:
          for
            fx     <- install()
            result <- call(fx.port, "events/subscribe", subscription(fx.url(), name = "poll-only", arguments = Json.Obj()))
          yield assertTrue(code(result).contains(-32026))
      ,
    ),
    suite("domain")(
      test("the subscription id doesn't depend on the order of the arguments' members"):
        val url = McpWebhookUrl.parse("https://hooks.example.com/a").toOption.get
        val owner = McpWebhookPrincipalId("issuer|sub:alice")
        def key(args: Json.Obj) = McpWebhookKey(owner, url, McpEventName("e"), args)
        val ab = key(Json.Obj("a" -> Json.Num(1), "b" -> Json.Obj("y" -> Json.Num(2.0), "x" -> Json.Bool(true))))
        val ba = key(Json.Obj("b" -> Json.Obj("x" -> Json.Bool(true), "y" -> Json.Num(2)), "a" -> Json.Num(1)))
        val other = key(Json.Obj("a" -> Json.Num(2)))
        assertTrue(ab.id == ba.id, ab.id != other.id)
      ,
      test("public targets refuse the IANA special-purpose ranges"):
        def global(address: String) = McpWebhookTargets.isGlobal(InetAddress.getByName(address))
        assertTrue(
          !global("127.0.0.1"), !global("10.0.0.1"), !global("172.16.0.1"), !global("192.168.0.1"),
          !global("169.254.169.254"), !global("100.64.0.1"), !global("0.0.0.0"), !global("198.18.0.1"),
          !global("203.0.113.5"), !global("255.255.255.255"), !global("::1"), !global("fd00::1"),
          !global("fe80::1"), !global("2001:db8::1"), !global("64:ff9b::a00:1"),
          global("93.184.215.14"), global("2606:4700::1111"),
        )
      ,
      test("the server grants a TTL within its bounds"):
        val config = McpWebhookConfig(minTtl = 1.minute, maxTtl = 1.hour, defaultTtl = 30.minutes, noExpiry = true)
        assertTrue(
          config.grant(McpWebhookTtl.ServerDefault, durable = false).contains(30.minutes),
          config.grant(McpWebhookTtl.Suggested(1.second), durable = false).contains(1.minute),
          config.grant(McpWebhookTtl.Suggested(1.day), durable = false).contains(1.hour),
          config.grant(McpWebhookTtl.NoExpiry, durable = false).contains(1.hour),
          config.grant(McpWebhookTtl.NoExpiry, durable = true).isEmpty,
        )
      ,
    ),
  ).provide(Server.defaultWithPort(0), Client.default, McpServer.State.default) @@ withLiveClock @@ timeout(60.seconds) @@ sequential
