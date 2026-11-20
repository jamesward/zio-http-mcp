package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.auth.Principal
import zio.*
import zio.http.*
import zio.http.netty.NettyConfig
import zio.json.*
import zio.json.ast.Json
import zio.stream.ZStream

import java.net.{Inet4Address, Inet6Address, InetAddress, UnknownHostException}
import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import java.time.Instant
import java.util.{Base64, UUID}
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

// --- MCP Events extension, webhook delivery (SEP-3415): the server ---

/**
 * Which callback URLs the server delivers to.
 *
 * @param requireHttps refuse `http` callback URLs (`InvalidParams`), as SEP-3415 requires
 * @param allowAddress whether the server may connect to an address a callback
 *                     host resolves to. It is checked at every connection, and
 *                     the connection goes to the address checked, so a DNS answer
 *                     that changes later (rebinding) doesn't get past it.
 */
final case class McpWebhookTargets(requireHttps: Boolean, allowAddress: InetAddress => Boolean)

object McpWebhookTargets:
  /** `https` URLs whose host resolves only to globally routable addresses: the default. */
  val public: McpWebhookTargets = McpWebhookTargets(requireHttps = true, isGlobal)

  /** Any URL, `http` too, at any address, such as a receiver on localhost: for tests and local development only. */
  val unrestricted: McpWebhookTargets = McpWebhookTargets(requireHttps = false, _ => true)

  /** Not in one of the IANA special-purpose ranges a server must not be made to reach. */
  def isGlobal(address: InetAddress): Boolean =
    !(address.isLoopbackAddress || address.isAnyLocalAddress || address.isLinkLocalAddress ||
      address.isSiteLocalAddress || address.isMulticastAddress || special(address))

  private def special(address: InetAddress): Boolean =
    val b = address.getAddress.map(_ & 0xff)
    address match
      case _: Inet4Address =>
        b(0) == 0 ||                                         // 0.0.0.0/8
        (b(0) == 100 && (b(1) & 0xc0) == 64) ||              // 100.64.0.0/10, shared (CGNAT)
        (b(0) == 192 && b(1) == 0 && (b(2) == 0 || b(2) == 2)) || // 192.0.0.0/24, 192.0.2.0/24
        (b(0) == 198 && (b(1) & 0xfe) == 18) ||              // 198.18.0.0/15, benchmarking
        (b(0) == 198 && b(1) == 51 && b(2) == 100) ||        // 198.51.100.0/24, documentation
        (b(0) == 203 && b(1) == 0 && b(2) == 113) ||         // 203.0.113.0/24, documentation
        b(0) >= 240                                          // 240.0.0.0/4, reserved, and broadcast
      case _: Inet6Address =>
        (b(0) & 0xfe) == 0xfc ||                             // fc00::/7, unique local
        (b(0) == 0x20 && b(1) == 0x01 && b(2) == 0x0d && b(3) == 0xb8) || // 2001:db8::/32, documentation
        (b(0) == 0x20 && b(1) == 0x01 && b(2) == 0 && b(3) == 0) ||       // 2001::/32, Teredo
        (b(0) == 0x20 && b(1) == 0x02) ||                    // 2002::/16, 6to4
        (b(0) == 0x00 && b(1) == 0x64 && b(2) == 0xff && b(3) == 0x9b) || // 64:ff9b::/96, NAT64
        (b(0) == 0x01 && b(1) == 0x00 && b.slice(2, 8).forall(_ == 0))    // 100::/64, discard
      case _ => true

/**
 * How a server delivers webhooks.
 *
 * @param maxTtl          the longest lifetime granted; a longer suggestion is cut to it
 * @param minTtl          the shortest; a shorter suggestion is raised to it
 * @param defaultTtl      the lifetime granted when the client suggests none
 * @param noExpiry        grant no expiry when a client asks for it (`ttlMs: null`);
 *                        only with a durable [[McpWebhookStore]], otherwise it gets `maxTtl`
 * @param verificationTtl how long an endpoint's verification of a `(principal, url)` lasts
 * @param deliveryTimeout how long a delivery waits for the endpoint's answer
 * @param retryDelays     the waits between attempts at an event that failed; after
 *                        the last, the event is given up on
 * @param secretGrace     how long a secret replaced on refresh still signs deliveries
 * @param expiryCheck     how often expired subscriptions are ended
 * @param preVerified     callback URLs that need no verification handshake: an allowlist
 * @param anonymous       accept `events/subscribe` and `events/unsubscribe` without
 *                        an authenticated caller, which SEP-3415 forbids. Such
 *                        subscriptions all belong to one anonymous owner, so they
 *                        are told apart by URL, name and arguments alone: anyone
 *                        who knows those can unsubscribe one. A new secret on a
 *                        refresh takes effect only after the endpoint verifies it
 *                        (the challenge is signed with it), and every new anonymous
 *                        subscription is verified, without the per-URL cache.
 */
final case class McpWebhookConfig(
  maxTtl: Duration = 1.hour,
  minTtl: Duration = 1.minute,
  defaultTtl: Duration = 1.hour,
  noExpiry: Boolean = false,
  verificationTtl: Duration = 24.hours,
  deliveryTimeout: Duration = 5.seconds,
  retryDelays: Chunk[Duration] = Chunk(10.seconds, 1.minute, 3.minutes, 6.minutes),
  secretGrace: Duration = 5.minutes,
  expiryCheck: Duration = 30.seconds,
  targets: McpWebhookTargets = McpWebhookTargets.public,
  preVerified: McpWebhookUrl => Boolean = _ => false,
  anonymous: Boolean = false,
):
  /** The lifetime granted for a suggestion: `None` for no expiry. */
  def grant(ttl: McpWebhookTtl, durable: Boolean): Option[Duration] = ttl match
    case McpWebhookTtl.ServerDefault  => Some(clamp(defaultTtl))
    case McpWebhookTtl.NoExpiry       => if noExpiry && durable then None else Some(maxTtl)
    case McpWebhookTtl.Suggested(ttl) => Some(clamp(ttl))

  private def clamp(ttl: Duration): Duration =
    if ttl < minTtl then minTtl else if ttl > maxTtl then maxTtl else ttl

/** Standard Webhooks signatures: `v1,` and the base64 HMAC-SHA256 of `id.timestamp.body`, one per secret. */
private[mcp] object McpWebhookSignature:
  def sign(secrets: NonEmptyChunk[McpWebhookSecret], messageId: String, timestamp: Long, body: Array[Byte]): String =
    secrets.map: secret =>
      val mac = Mac.getInstance("HmacSHA256")
      mac.init(SecretKeySpec(secret.key.toArray, "HmacSHA256"))
      mac.update(s"$messageId.$timestamp.".getBytes(UTF_8))
      "v1," + Base64.getEncoder.encodeToString(mac.doFinal(body))
    .mkString(" ")

/** POSTs signed webhook bodies, connecting only to addresses `targets` allows and never following a redirect. */
private[mcp] final case class McpWebhookSender(client: Client, timeout: Duration):
  import McpWebhookSender.Outcome

  def post(url: McpWebhookUrl, id: McpWebhookSubscriptionId, secrets: NonEmptyChunk[McpWebhookSecret],
    messageId: String, body: Array[Byte]): UIO[Outcome] =
    for
      target  <- ZIO.fromEither(URL.decode(url.value)).orDie
      now     <- Clock.instant
      ts       = now.getEpochSecond
      request  = Request.post(target, Body.fromArray(body)).addHeaders(Headers(
                   Header.ContentType(MediaType.application.json),
                   Header.Custom("webhook-id", messageId),
                   Header.Custom("webhook-timestamp", ts.toString),
                   Header.Custom("webhook-signature", McpWebhookSignature.sign(secrets, messageId, ts, body)),
                   Header.Custom(McpWebhookSender.SubscriptionIdHeader, id.value),
                 ))
      outcome <- ZIO.scoped(ZClient.streaming(request).mapError(McpWebhookSender.failureOf).flatMap(answer).provideSomeLayer[Scope](ZLayer.succeed(client)))
                   .timeoutFail(McpWebhookFailure.Timeout)(timeout)
                   .catchAll(failure => ZIO.succeed(Outcome.Failed(failure)))
    yield outcome

  private def answer(response: Response): ZIO[Any, McpWebhookFailure, Outcome] =
    val status = response.status.code
    if response.status.isSuccess then
      response.body.asStream.take(McpWebhookSender.MaxAnswer).runCollect
        .mapBoth(McpWebhookSender.failureOf, bytes => Outcome.Accepted(String(bytes.toArray, UTF_8)))
    else if status == 410 || status == 413 then ZIO.succeed(Outcome.Refused(status))
    else if status >= 500 then ZIO.succeed(Outcome.Failed(McpWebhookFailure.Http5xx))
    else ZIO.succeed(Outcome.Failed(McpWebhookFailure.Http4xx))
  .catchAllDefect(e => ZIO.fail(McpWebhookSender.failureOf(e)))

private[mcp] object McpWebhookSender:
  val SubscriptionIdHeader: String = "X-MCP-Subscription-Id"
  /** How much of an endpoint's answer is read: enough for a verification echo. */
  val MaxAnswer: Int = 4096

  enum Outcome:
    case Accepted(body: String)
    /** `410` or `413`: the endpoint doesn't want this one again. */
    case Refused(status: Int)
    case Failed(failure: McpWebhookFailure)

  def make(config: McpWebhookConfig): ZIO[Scope, Throwable, McpWebhookSender] =
    val dns: DnsResolver = new DnsResolver:
      def resolve(host: String)(implicit trace: Trace): ZIO[Any, UnknownHostException, Chunk[InetAddress]] =
        ZIO.attemptBlocking(Chunk.fromArray(InetAddress.getAllByName(host)))
          .refineOrDie { case e: UnknownHostException => e }
          .map(_.filter(config.targets.allowAddress))
          .filterOrFail(_.nonEmpty)(UnknownHostException(s"$host has no address webhooks may be delivered to"))
    val layer = (ZLayer.succeed(ZClient.Config.default.idleTimeout(30.seconds)) ++
      ZLayer.succeed(NettyConfig.defaultWithFastShutdown) ++ ZLayer.succeed(dns)) >>> ZClient.live
    layer.build.map(env => McpWebhookSender(env.get[Client], config.deliveryTimeout))

  def failureOf(error: Throwable): McpWebhookFailure =
    val chain = Iterator.iterate(Option(error))(_.flatMap(e => Option(e.getCause))).takeWhile(_.isDefined).flatten.toList
    if chain.exists(_.isInstanceOf[javax.net.ssl.SSLException]) then McpWebhookFailure.TlsError
    else if chain.exists(e => e.isInstanceOf[java.net.SocketTimeoutException] ||
      e.getClass.getSimpleName == "ConnectTimeoutException") then McpWebhookFailure.Timeout
    else McpWebhookFailure.ConnectionRefused

/**
 * The webhook subscriptions of one server: each has a fiber that reads its
 * event type's push stream from the subscription's cursor and POSTs what it
 * reads, one event at a time, retrying each before going on. Delivering in
 * order keeps the watermark simple: an event's own cursor is safe to persist
 * once it is delivered or given up on.
 */
private[mcp] final case class McpWebhookRuntime[R](
  config: McpWebhookConfig,
  env: ZEnvironment[R],
  store: McpWebhookStore,
  sender: McpWebhookSender,
  handlers: Map[McpEventName, McpEventHandler[R]],
  workers: Ref.Synchronized[Map[McpWebhookSubscriptionId, McpWebhookRuntime.Worker]],
  scope: Scope,
):
  import McpWebhookRuntime.{*, given}
  import McpWebhookSender.Outcome

  def subscribe(params: McpEventsSubscribeParams, ctx: McpRequestContext): IO[McpEventError, McpEventsSubscribeResult] =
    for
      (principal, owner) <- caller(ctx)
      _                  <- ZIO.fail(McpEventError.InvalidParams("Invalid 'delivery.url': expected an https URL"))
                              .when(config.targets.requireHttps && !params.url.isHttps)
      handler            <- ZIO.fromOption(handlers.get(params.name)).orElseFail(McpEventError.NotFound(params.name))
                              .filterOrFail(offersWebhook)(McpEventError.Unsupported("deliveryMode", Some(McpEventDelivery.Webhook.wire)))
      key                 = McpWebhookKey(owner, params.url, params.name, params.arguments)
      now                <- Clock.instant
      refreshBefore       = config.grant(params.ttl, store.durable).map(now.plus(_))
      live               <- workers.get.map(_.get(key.id))
      result             <- live match
                              case Some(worker) => refresh(worker, key, params.secret, refreshBefore)
                              case None         => create(handler, key, principal, params, refreshBefore, ctx)
    yield result

  def unsubscribe(params: McpEventsUnsubscribeParams, ctx: McpRequestContext): IO[McpEventError, Unit] =
    for
      (_, owner) <- caller(ctx)
      id          = McpWebhookKey(owner, params.url, params.name, params.arguments).id
      removed    <- workers.modify(m => (m.get(id), m - id))
      stored     <- store.get(id).storeFailure("events/unsubscribe", id)
      _          <- ZIO.fail(McpEventError.SubscriptionNotFound).when(removed.isEmpty && stored.isEmpty)
      _          <- ZIO.foreachDiscard(removed)(_.fiber.interrupt)
      _          <- store.delete(id).storeFailure("events/unsubscribe", id)
    yield ()

  /** Deliver to every subscription in the store, as after a restart. */
  def resume: UIO[Unit] =
    store.all.flatMap(ZIO.foreachDiscard(_)(resumeOne))
      .catchAllCause(c => ZIO.logErrorCause("Webhook subscriptions could not be read from the store", c))

  /** End subscriptions whose expiry passed, every `config.expiryCheck`. */
  def expireLoop: UIO[Unit] =
    (for
      now     <- Clock.instant
      current <- workers.get
      expired <- ZIO.filter(current.toList)((_, w) => w.state.get.map(_.subscription.isExpired(now)))
      _       <- ZIO.foreachDiscard(expired)((id, w) => end(id, Some(w.token)) *> w.fiber.interrupt *>
                   ZIO.logInfo(s"Webhook subscription $id expired"))
    yield ()).repeat(Schedule.spaced(config.expiryCheck)).unit

  private def offersWebhook(handler: McpEventHandler[R]): Boolean = McpWebhookRuntime.offersWebhook(handler.descriptor)

  private def caller(ctx: McpRequestContext): IO[McpEventError, (Option[Principal], McpWebhookPrincipalId)] =
    ctx.principal match
      case Some(p) =>
        ZIO.fromOption(McpWebhookPrincipalId.of(p)).map(Some(p) -> _)
          .orElseFail(McpEventError.Forbidden("The caller's principal names no subject or client"))
      case None if config.anonymous => ZIO.succeed(None -> McpWebhookPrincipalId.Anonymous)
      case None => ZIO.fail(McpEventError.Forbidden("Webhook subscriptions need an authenticated caller"))

  private def refresh(worker: Worker, key: McpWebhookKey, secret: McpWebhookSecret, refreshBefore: Option[Instant])
    : IO[McpEventError, McpEventsSubscribeResult] =
    for
      current <- worker.state.get.map(_.subscription.secret)
      // Anyone can refresh an anonymous subscription: its endpoint has to accept a new secret first.
      _       <- handshake(key, secret).when(key.principal.isAnonymous && current != secret)
      now     <- Clock.instant
      updated <- worker.state.updateAndGet: s =>
                   val sub = s.subscription
                   val previous =
                     if sub.secret == secret then sub.previousSecret
                     else Some(McpWebhookPreviousSecret(sub.secret, now.plus(config.secretGrace)))
                   s.copy(subscription = sub.copy(secret = secret, previousSecret = previous, refreshBefore = refreshBefore),
                     status = s.status.copy(active = true))
      _       <- store.put(updated.subscription).storeFailure("events/subscribe", updated.subscription.id)
    yield McpEventsSubscribeResult(updated.subscription.id, refreshBefore, updated.watermark, truncated = false,
      Some(updated.status))

  private def create(
    handler: McpEventHandler[R],
    key: McpWebhookKey,
    principal: Option[Principal],
    params: McpEventsSubscribeParams,
    refreshBefore: Option[Instant],
    ctx: McpRequestContext,
  ): IO[McpEventError, McpEventsSubscribeResult] =
    for
      pushes  <- handler.push(params.arguments, params.cursor, params.maxAgeMs.map(Duration.fromMillis), ctx).provideEnvironment(env)
      _       <- verify(key, params.secret, refreshBefore)
      sub      = McpWebhookSubscription(key, params.secret, None, principal.map(_.copy(raw = "")), refreshBefore, params.cursor)
      _       <- store.put(sub).storeFailure("events/subscribe", key.id)
      started <- install(sub, pushes, resumed = false)
      result  <- started match
                   // Another subscribe for the same key got there first: this one refreshes it.
                   case Left(existing) => refresh(existing, key, params.secret, refreshBefore)
                   case Right(first)   =>
                     first.await.map((cursor, truncated) => McpEventsSubscribeResult(key.id, refreshBefore, cursor, truncated))
    yield result

  /** Prove the endpoint wants deliveries for `(principal, url)`, unless that's known already. */
  private def verify(key: McpWebhookKey, secret: McpWebhookSecret, refreshBefore: Option[Instant]): IO[McpEventError, Unit] =
    for
      now    <- Clock.instant
      cached <- store.verification(key.principal, key.url).storeFailure("events/subscribe", key.id)
      // The cache is per principal: one anonymous caller's verification can't vouch for another's.
      known   = config.preVerified(key.url) || (!key.principal.isAnonymous && cached.exists(_.isValid(now)))
      _      <- (handshake(key, secret) *>
                  store.putVerification(McpWebhookVerification(key.principal, key.url,
                    refreshBefore.map(r => if r.isAfter(now.plus(config.verificationTtl)) then r else now.plus(config.verificationTtl))))
                    .storeFailure("events/subscribe", key.id)).unless(known)
    yield ()

  private def handshake(key: McpWebhookKey, secret: McpWebhookSecret): IO[McpEventError, Unit] =
    for
      challenge <- randomToken(32)
      messageId <- randomToken(18).map(r => s"msg_verification_$r")
      body       = McpEventsWire.webhookVerification(challenge).toJson.getBytes(UTF_8)
      outcome   <- sender.post(key.url, key.id, NonEmptyChunk(secret), messageId, body)
      _         <- verified(outcome, challenge)
                   .mapError(McpEventError.CallbackEndpoint(_))
                   .tapError(e => ZIO.logWarning(s"Webhook endpoint verification failed for ${key.id}: $e"))
    yield ()

  private def verified(outcome: Outcome, challenge: String): IO[McpWebhookFailure, Unit] =
    outcome match
      case Outcome.Accepted(answer) if echoes(answer, challenge) => ZIO.unit
      case Outcome.Accepted(_)  => ZIO.fail(McpWebhookFailure.ChallengeFailed)
      case Outcome.Refused(_)   => ZIO.fail(McpWebhookFailure.Http4xx)
      case Outcome.Failed(f)    => ZIO.fail(f)

  private def echoes(answer: String, challenge: String): Boolean =
    answer.fromJson[Json.Obj].toOption.flatMap(_.get("challenge")).flatMap(_.asString)
      .exists(echoed => MessageDigest.isEqual(echoed.getBytes(UTF_8), challenge.getBytes(UTF_8)))

  private def resumeOne(sub: McpWebhookSubscription): UIO[Unit] =
    Clock.instant.flatMap: now =>
      handlers.get(sub.key.name).filter(offersWebhook) match
        case _ if sub.isExpired(now) => store.delete(sub.id).ignoreLogged
        case None =>
          ZIO.logWarning(s"Webhook subscription ${sub.id} is for the unknown event type '${sub.key.name.value}': deleted") *>
            store.delete(sub.id).ignoreLogged
        case Some(handler) =>
          val ctx = McpRequestContext(ProtocolVersion.V2026_07_28, callerPrincipal = sub.principal)
          handler.push(sub.key.arguments, sub.cursor, None, ctx).provideEnvironment(env).foldZIO(
            error => ZIO.logWarning(s"Webhook subscription ${sub.id} can't resume: ${error.detail.message}") *>
              terminate(sub, error.detail),
            pushes => install(sub, pushes, resumed = true).unit,
          )

  /** Start delivering `pushes`, unless the subscription already has a worker; `Right` with its first position. */
  private def install(sub: McpWebhookSubscription, pushes: ZStream[R, Nothing, McpEventPush], resumed: Boolean)
    : UIO[Either[Worker, Promise[Nothing, (Option[McpEventCursor], Boolean)]]] =
    workers.modifyZIO: current =>
      current.get(sub.id) match
        case Some(existing) => ZIO.succeed((Left(existing), current))
        case None =>
          for
            token <- Random.nextUUID
            state <- Ref.make(WorkerState(sub, sub.cursor, McpWebhookDeliveryStatus.initial))
            first <- Promise.make[Nothing, (Option[McpEventCursor], Boolean)]
            fiber <- deliver(state, first, pushes, resumed)
                       .provideEnvironment(env)
                       .ensuring(first.succeed((sub.cursor, false)) *> forget(sub.id, token))
                       .forkIn(scope)
          yield (Right(first), current.updated(sub.id, Worker(token, fiber, state)))

  private def deliver(
    state: Ref[WorkerState],
    first: Promise[Nothing, (Option[McpEventCursor], Boolean)],
    pushes: ZStream[R, Nothing, McpEventPush],
    resumed: Boolean,
  ): ZIO[R, Nothing, Unit] =
    pushes.runForeachWhile:
      case McpEventPush.Position(cursor) => advance(state, cursor).as(true)
      case McpEventPush.Signal(McpEventSignal.Active(cursor, truncated)) =>
        first.succeed((cursor, truncated)).flatMap: initial =>
          val gap = !initial || (resumed && truncated)
          (send(state, "gap", McpEventsWire.webhookGap(cursor)).when(gap) *> advance(state, cursor)).as(true)
      case McpEventPush.Signal(McpEventSignal.Event(occurrence)) =>
        (send(state, "event", McpEventsWire.webhookEvent(occurrence)) *> advance(state, occurrence.cursor)).as(true)
      case McpEventPush.Signal(McpEventSignal.Error(error)) =>
        state.get.flatMap(s => ZIO.logWarning(s"Webhook subscription ${s.subscription.id}: ${error.message}")).as(true)
      case McpEventPush.Signal(McpEventSignal.Terminated(error)) =>
        state.get.flatMap(s => terminate(s.subscription, error)).as(false)
      case McpEventPush.Signal(McpEventSignal.Heartbeat(_)) => ZIO.succeed(true)

  /** Tell the endpoint the subscription ended, and forget it. */
  private def terminate(sub: McpWebhookSubscription, error: ErrorDetail): UIO[Unit] =
    for
      state <- Ref.make(WorkerState(sub, sub.cursor, McpWebhookDeliveryStatus.initial))
      _     <- send(state, "terminated", McpEventsWire.webhookTerminated(error))
      _     <- end(sub.id, None)
    yield ()

  private def advance(state: Ref[WorkerState], cursor: Option[McpEventCursor]): UIO[Unit] =
    state.modify(s => (s.watermark != cursor, s.copy(watermark = cursor))).flatMap: moved =>
      state.get.flatMap(s => store.update(s.subscription.id)(_.copy(cursor = cursor)).unit
        .catchAll(e => ZIO.logWarning(s"Webhook subscription ${s.subscription.id}: cursor not stored: ${e.message}")))
        .when(moved && store.durable).unit

  /** POST one message, retrying it after each of `config.retryDelays` until the endpoint takes it. */
  private def send(state: Ref[WorkerState], kind: String, body: Json.Obj): UIO[Unit] =
    val bytes = body.toJson.getBytes(UTF_8)
    def attempt(messageId: String, delays: List[Duration]): UIO[Unit] =
      for
        s       <- state.get
        now     <- Clock.instant
        sub      = s.subscription
        outcome <- sender.post(sub.key.url, sub.id, sub.signingSecrets(now), messageId, bytes)
        done    <- Clock.instant
        _       <- outcome match
                     case Outcome.Accepted(_) =>
                       state.update(s => s.copy(status = s.status.copy(lastDeliveryAt = Some(done), lastError = None, failedSince = None)))
                     case Outcome.Refused(status) =>
                       ZIO.logInfo(s"Webhook endpoint of ${sub.id} refused $messageId with HTTP $status: not retried")
                     case Outcome.Failed(failure) =>
                       state.update(s => s.copy(status = s.status.copy(lastError = Some(failure),
                         failedSince = s.status.failedSince.orElse(Some(done))))) *>
                         (delays match
                           case delay :: rest => attempt(messageId, rest).delay(delay)
                           case Nil => ZIO.logWarning(s"Webhook delivery $messageId to ${sub.id} given up: ${failure.wire}"))
      yield ()
    if bytes.length > MaxBody then
      state.get.flatMap(s => ZIO.logWarning(s"Webhook $kind for ${s.subscription.id} is over $MaxBody bytes: not sent"))
    else
      randomToken(18).flatMap(r => attempt(if kind == "event" then s"msg_$r" else s"msg_${kind}_$r", config.retryDelays.toList))

  /** Delete the subscription; and forget its worker, if it is still `token`'s. */
  private def end(id: McpWebhookSubscriptionId, token: Option[UUID]): UIO[Unit] =
    token.fold(workers.update(_ - id))(forget(id, _)) *> store.delete(id).ignoreLogged

  private def forget(id: McpWebhookSubscriptionId, token: UUID): UIO[Unit] =
    workers.update(m => if m.get(id).exists(_.token == token) then m - id else m)

  extension [A](io: IO[McpWebhookStoreError, A])
    private def storeFailure(method: String, id: McpWebhookSubscriptionId): IO[McpEventError, A] =
      io.tapError(e => ZIO.logError(s"$method for $id: webhook store failed: ${e.message}"))
        .orElseFail(McpEventError.Upstream("Webhook subscription storage failed"))

private[mcp] object McpWebhookRuntime:
  private given CanEqual[UUID, UUID] = CanEqual.derived

  /** Webhook delivery reads an event type's push stream, so it is offered where push is. */
  def offersWebhook(descriptor: McpEventDescriptor): Boolean = descriptor.delivery.contains(McpEventDelivery.Push)

  /** SEP-3415's limit for a delivery body. */
  val MaxBody: Int = 256 * 1024

  final case class Worker(token: UUID, fiber: Fiber.Runtime[Nothing, Unit], state: Ref[WorkerState])

  final case class WorkerState(
    subscription: McpWebhookSubscription,
    watermark: Option[McpEventCursor],
    status: McpWebhookDeliveryStatus,
  )

  def make[R](config: McpWebhookConfig, handlers: Map[McpEventName, McpEventHandler[R]])
    : ZIO[R & McpWebhookStore & Scope, Throwable, McpWebhookRuntime[R]] =
    for
      env     <- ZIO.environment[R]
      store   <- ZIO.service[McpWebhookStore]
      scope   <- ZIO.scope
      sender  <- McpWebhookSender.make(config)
      workers <- Ref.Synchronized.make(Map.empty[McpWebhookSubscriptionId, Worker])
      runtime  = McpWebhookRuntime(config, env, store, sender, handlers, workers, scope)
      _       <- runtime.resume
      _       <- runtime.expireLoop.forkIn(scope)
    yield runtime

  private def randomToken(bytes: Int): UIO[String] =
    Random.nextBytes(bytes).map(b => Base64.getUrlEncoder.withoutPadding.encodeToString(b.toArray))
