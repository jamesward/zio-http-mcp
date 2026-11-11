package com.jamesward.ziohttp.mcp.client

import com.jamesward.ziohttp.mcp.*
import zio.*
import zio.http.Client
import zio.json.ast.Json
import zio.stream.ZStream

/**
 * Tuning for [[McpEventsClient.subscribe]].
 *
 * @param pollFloor      the shortest wait between polls, whatever `nextPollMs` says
 * @param reconnectDelay the wait before reconnecting a dropped push stream, or
 *                       retrying a poll that failed in transport
 * @param idleTimeout    a push stream that sends nothing (no event, no heartbeat)
 *                       for this long is taken as dead and reconnected: twice
 *                       the 30-second heartbeat servers send
 * @param dedupeWindow   how many recent event ids are remembered, to drop the
 *                       duplicates a reconnection can deliver
 */
final case class McpEventsSubscribeSettings(
  pollFloor: Duration = 1.second,
  reconnectDelay: Duration = 1.second,
  idleTimeout: Duration = 60.seconds,
  dedupeWindow: Int = 1000,
)

/**
 * A client of the Events extension (`io.modelcontextprotocol/events`, SEP-3415, a
 * draft: experimental). [[subscribe]] receives an event type's events whichever
 * way the server offers them; [[list]], [[poll]] and [[stream]] are the
 * protocol's own requests.
 */
final case class McpEventsClient private (underlying: McpExtensionClient):

  def list(cursor: Option[String] = None): IO[McpClientError, McpEventsListResult] =
    underlying.request(McpEvents.listOperation, McpEventsListParams(cursor))

  /** Every event type, following `nextCursor`. */
  def listAll: IO[McpClientError, Chunk[McpEventDescriptor]] =
    def loop(cursor: Option[String], seen: Set[String], acc: Chunk[McpEventDescriptor]): IO[McpClientError, Chunk[McpEventDescriptor]] =
      list(cursor).flatMap: page =>
        page.nextCursor match
          case None => ZIO.succeed(acc ++ page.events)
          case Some(next) if seen.contains(next) =>
            ZIO.fail(McpClientError.Protocol(s"events/list repeated pagination cursor '$next'"))
          case Some(next) => loop(Some(next), seen + next, acc ++ page.events)
    loop(None, Set.empty, Chunk.empty)

  def poll(params: McpEventsPollParams): IO[McpClientError, McpEventsPollResult] =
    underlying.request(McpEvents.pollOperation, params)

  /**
   * One `events/stream` push subscription: its signals as the server sends them,
   * heartbeats and `terminated` included. It ends when the server ends it, and
   * ending it early cancels the subscription.
   */
  def stream(params: McpEventsStreamParams): ZStream[Any, McpClientError, McpEventSignal] =
    underlying.requestStream(McpEvents.streamOperation, params).mapZIO: n =>
      ZIO.fromEither(McpEventsWire.signal(n.method.value, n.params))
        .mapError(m => McpClientError.Decode(s"Invalid ${n.method.value}: $m"))
    .takeUntil:
      case McpEventSignal.Terminated(_) => true
      case _                            => false

  /**
   * Receive the events of the subscription `(name, arguments)`, from `cursor`
   * (`None`: from now), by push if the event type offers it, otherwise by
   * polling, or by `delivery` when given. The stream yields:
   *
   *   - `Active` first, and again with `truncated` when events were skipped;
   *   - each `Event` once, though a reconnection may deliver it again;
   *   - `Heartbeat`s, whose cursor (like each event's, on push) is safe to persist
   *     and resume from;
   *   - transient upstream `Error`s.
   *
   * A dropped push stream is reconnected from the last cursor, and a poll is
   * retried. The stream fails when the server ends the subscription
   * (`terminated`, or a poll's error), with its error as [[McpClientError.JsonRpc]].
   */
  def subscribe(
    name: McpEventName,
    arguments: Json.Obj = Json.Obj(),
    cursor: Option[McpEventCursor] = None,
    delivery: Option[McpEventDelivery] = None,
    settings: McpEventsSubscribeSettings = McpEventsSubscribeSettings(),
  ): ZStream[Any, McpClientError, McpEventSignal] =
    ZStream.unwrap:
      for
        descriptors <- listAll
        descriptor  <- ZIO.fromOption(descriptors.find(_.name == name))
                         .orElseFail(McpClientError.Protocol(s"The server has no event type '${name.value}'"))
        mode        <- ZIO.fromEither(McpEventsClient.mode(descriptor, delivery))
        recent      <- Ref.make(McpEventsClient.Recent(Set.empty, Chunk.empty, settings.dedupeWindow.max(1)))
      yield
        val signals = mode match
          case McpEventDelivery.Push => pushed(name, arguments, cursor, settings)
          case _                     => polled(name, arguments, cursor, settings)
        signals.filterZIO:
          case McpEventSignal.Event(occurrence) => recent.modify(_.add(occurrence.eventId))
          case _                                => ZIO.succeed(true)

  private def pushed(
    name: McpEventName,
    arguments: Json.Obj,
    cursor: Option[McpEventCursor],
    settings: McpEventsSubscribeSettings,
  ): ZStream[Any, McpClientError, McpEventSignal] =
    ZStream.unwrap:
      for
        position <- Ref.make(cursor)
        first    <- Ref.make(true)
      yield
        val connection = ZStream.unwrap(position.get.map: from =>
          McpEventsClient.watched(stream(McpEventsStreamParams(name, arguments, from)), settings.idleTimeout)
            ++ ZStream.fail(McpClientError.Transport("the server ended the push stream"))
        ).mapZIO:
          case McpEventSignal.Terminated(error) =>
            ZIO.fail(McpClientError.JsonRpc(error.code, error.message, error.data))
          case signal @ McpEventSignal.Active(at, truncated) =>
            // A reconnection's `active` says nothing new unless it reports a gap.
            position.set(at) *> first.getAndSet(false).map(isFirst => Option.when(isFirst || truncated)(signal))
          case signal @ McpEventSignal.Event(occurrence) => position.set(occurrence.cursor).as(Some(signal))
          case signal @ McpEventSignal.Heartbeat(at)     => position.set(at).as(Some(signal))
          case signal @ McpEventSignal.Error(_)          => ZIO.some(signal)
        .collectSome
        connection.retry(McpEventsClient.reconnecting(settings))

  private def polled(
    name: McpEventName,
    arguments: Json.Obj,
    cursor: Option[McpEventCursor],
    settings: McpEventsSubscribeSettings,
  ): ZStream[Any, McpClientError, McpEventSignal] =
    final case class Next(cursor: Option[McpEventCursor], delay: Duration, first: Boolean)
    ZStream.unfoldChunkZIO(Next(cursor, Duration.Zero, first = true)): next =>
      (ZIO.sleep(next.delay) *> poll(McpEventsPollParams(name, arguments, next.cursor)).retry(McpEventsClient.reconnecting(settings)))
        .map: result =>
          val active = Chunk.fromIterable(Option.when(next.first || result.truncated)(
            McpEventSignal.Active(if result.events.isEmpty then result.cursor else next.cursor, result.truncated)))
          // A poll's events carry no cursor: the heartbeat after them does.
          val signals = active ++ result.events.map(McpEventSignal.Event(_)) :+ McpEventSignal.Heartbeat(result.cursor)
          val delay = if result.hasMore then Duration.Zero else Duration.fromMillis(result.nextPollMs).max(settings.pollFloor)
          Some((signals, Next(result.cursor, delay, first = false)))

object McpEventsClient:
  /** The client's declaration of the Events extension, sent by [[connect]]. */
  val declaration: McpClientExtensions = McpClientExtensions.single(McpClientExtension(McpEvents.Id, Json.Obj()))

  def from(client: McpExtensionClient): Either[McpClientError, McpEventsClient] =
    client.serverCapabilities.extensions.flatMap(_.get(McpEvents.Id.value)) match
      case Some(_) => Right(McpEventsClient(client))
      case None    => Left(McpClientError.Protocol(s"Server does not advertise ${McpEvents.Id.value}"))

  def connect(config: McpClientConfig): ZIO[Client & Scope, McpClientError, McpEventsClient] =
    McpClient.connect(config, declaration).flatMap(client => ZIO.fromEither(from(client)))

  def connectStdio(config: McpStdioConfig): ZIO[Scope, McpClientError, McpEventsClient] =
    McpClient.connectStdio(config, declaration).flatMap(client => ZIO.fromEither(from(client)))

  /** The delivery mode to use: `requested`, or push before poll. */
  private def mode(
    descriptor: McpEventDescriptor,
    requested: Option[McpEventDelivery],
  ): Either[McpClientError, McpEventDelivery] =
    val offered = descriptor.delivery.toChunk
    val usable = offered.filterNot(_ == McpEventDelivery.Webhook)
    requested match
      case Some(McpEventDelivery.Webhook) =>
        Left(McpClientError.Protocol("This client does not support webhook delivery"))
      case Some(mode) if offered.contains(mode) => Right(mode)
      case Some(mode) =>
        Left(McpClientError.Protocol(s"Event type '${descriptor.name.value}' does not offer ${mode.wire} delivery"))
      case None =>
        usable.find(_ == McpEventDelivery.Push).orElse(usable.headOption)
          .toRight(McpClientError.Protocol(s"Event type '${descriptor.name.value}' offers no delivery mode this client supports"))

  /** Retry what failed in transport (a dropped connection, a dead stream), after `reconnectDelay`. */
  private def reconnecting(settings: McpEventsSubscribeSettings): Schedule[Any, McpClientError, Any] =
    Schedule.spaced(settings.reconnectDelay) && Schedule.recurWhile[McpClientError]:
      case McpClientError.Transport(_, _) => true
      case _                              => false

  /** `signals`, failing in transport when nothing arrives for `idle`. */
  private def watched(
    signals: ZStream[Any, McpClientError, McpEventSignal],
    idle: Duration,
  ): ZStream[Any, McpClientError, McpEventSignal] =
    ZStream.unwrap:
      Clock.instant.flatMap(Ref.make).map: lastSeen =>
        val tracked = signals.tap(_ => Clock.instant.flatMap(lastSeen.set))
        val watchdog = ZStream.tick(idle.dividedBy(4).max(10.millis)).mapZIO: _ =>
          (Clock.instant <*> lastSeen.get).flatMap: (now, seen) =>
            ZIO.when(Duration.fromInterval(seen, now) > idle)(ZIO.fail(
              McpClientError.Transport(s"no event or heartbeat on the push stream for $idle")))
        tracked.mergeHaltLeft(watchdog.drain)

  /** The most recent event ids, oldest first, to drop repeats. */
  private final case class Recent(ids: Set[String], order: Chunk[String], max: Int):
    /** Whether `id` is new, and the ids after seeing it. */
    def add(id: String): (Boolean, Recent) =
      if ids.contains(id) then (false, this)
      else
        val kept = (order :+ id).takeRight(max)
        (true, Recent(kept.toSet, kept, max))
