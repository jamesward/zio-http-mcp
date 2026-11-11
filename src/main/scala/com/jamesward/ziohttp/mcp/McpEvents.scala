package com.jamesward.ziohttp.mcp

import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.schema.Schema
import zio.schema.codec.JsonCodec as SchemaJsonCodec
import zio.stream.ZStream

import java.time.Instant

// --- MCP Events extension (io.modelcontextprotocol/events, SEP-3415 draft): the server ---

/**
 * What a check function is asked: the subscription's `arguments`, and where to
 * read from.
 *
 * @param cursor    the position to read from; `None` asks for the current
 *                  position, with no events ("start from now")
 * @param maxAge    when replaying from a cursor, skip events older than this
 *                  (and say so with `truncated`)
 * @param maxEvents the most events to return; set `hasMore` when more are ready
 */
final case class McpEventRequest[+A](
  arguments: A,
  cursor: Option[McpEventCursor],
  maxAge: Option[Duration],
  maxEvents: Int,
  context: McpRequestContext,
)

/**
 * An event a check function found.
 *
 * @param eventId   the upstream event's own id, when it has one (a message id, a
 *                  delivery GUID), so that clients can deduplicate; a random one otherwise
 * @param timestamp when it occurred; now, otherwise
 * @param cursor    the position after this event; otherwise a push stream gives
 *                  the last event of a batch the batch's cursor, and the others
 *                  the cursor the batch was read from
 */
final case class McpEventEmit[+A](
  data: A,
  eventId: Option[String] = None,
  timestamp: Option[Instant] = None,
  cursor: Option[McpEventCursor] = None,
)

/**
 * A check function's answer: the events since the cursor it was given, and the
 * position after them (`None` for an event type that can't replay).
 *
 * @param truncated events were skipped: the cursor was too old, or `maxAge` cut in
 * @param hasMore   more events are ready than `maxEvents` allowed
 */
final case class McpEventBatch[+A](
  events: Chunk[McpEventEmit[A]],
  cursor: Option[McpEventCursor],
  truncated: Boolean = false,
  hasMore: Boolean = false,
)

object McpEventBatch:
  /** Nothing new: the position is `cursor`. */
  def empty(cursor: Option[McpEventCursor]): McpEventBatch[Nothing] = McpEventBatch(Chunk.empty, cursor)

/** The events read for one subscription: from `start`, up to `end`. Each
  * occurrence carries its own cursor. */
private[mcp] final case class McpEventRead(
  start: Option[McpEventCursor],
  events: Chunk[McpEventOccurrence],
  end: Option[McpEventCursor],
  truncated: Boolean,
  hasMore: Boolean,
)

/** One step of a push subscription: a signal to send, or a new position to report in heartbeats. */
private[mcp] enum McpEventPush:
  case Signal(signal: McpEventSignal)
  case Position(cursor: Option[McpEventCursor])

/** An event type and how its events are read. Build one with [[McpEvent]]. */
trait McpEventHandler[-R]:
  def descriptor: McpEventDescriptor
  final def name: McpEventName = descriptor.name
  private[mcp] def pollInterval: Duration
  private[mcp] def read(
    arguments: Json.Obj,
    cursor: Option[McpEventCursor],
    maxAge: Option[Duration],
    maxEvents: Int,
    ctx: McpRequestContext,
  ): ZIO[R, McpEventError, McpEventRead]
  /** Validate the subscription and read from its cursor; the stream starts with `active`. */
  private[mcp] def push(
    arguments: Json.Obj,
    cursor: Option[McpEventCursor],
    maxAge: Option[Duration],
    ctx: McpRequestContext,
  ): ZIO[R, McpEventError, ZStream[R, Nothing, McpEventPush]]

/**
 * A builder for an event type, like [[McpTool]]. Its events come either from a
 * check function, which reads what happened since a cursor (an upstream with a
 * change feed: a mailbox's history, a repository's events), or from an
 * [[McpEventBuffer]] the server emits into (a push-only upstream: webhooks, a
 * message bus). Either way the event type offers poll and push delivery.
 */
final case class McpEvent private (
  eventName: McpEventName,
  eventDescription: Option[String],
  eventPollInterval: Duration,
  eventCheckInterval: Duration,
  eventMeta: Option[Json.Obj],
  eventDelivery: NonEmptyChunk[McpEventDelivery],
):
  def description(text: String): McpEvent = copy(eventDescription = Some(text))

  /** Offer only poll delivery: `events/stream` is refused with `Unsupported`. */
  def pollOnly: McpEvent = copy(eventDelivery = NonEmptyChunk(McpEventDelivery.Poll))

  /** Offer only push delivery: `events/poll` is refused with `Unsupported`. */
  def pushOnly: McpEvent = copy(eventDelivery = NonEmptyChunk(McpEventDelivery.Push))

  /** How long poll clients are told to wait between polls (`nextPollMs`). */
  def pollInterval(interval: Duration): McpEvent = copy(eventPollInterval = interval)

  /** How often a push subscription runs the check function. */
  def checkInterval(interval: Duration): McpEvent = copy(eventCheckInterval = interval)

  def meta(value: Json.Obj): McpEvent = copy(eventMeta = Some(value))

  /**
   * Read events with `check`, which the server calls for every poll, and on a
   * timer (`checkInterval`, or at once while `hasMore`) for every push
   * subscription. Called with no cursor, it answers the current position, and it
   * should always answer one: a push subscription resumes from it, and without
   * one every check would start from now. (An upstream with no history to
   * resume from is what [[buffered]] is for.)
   * Failing with [[McpEventError.Upstream]] on a push stream is reported to the
   * client, and the next check goes on; any other error ends the subscription.
   */
  def check[R, In: McpInput, Out: Schema](
    check: McpEventRequest[In] => ZIO[R, McpEventError, McpEventBatch[Out]]
  ): McpEventHandler[R] =
    val payload = McpEvent.Payload(summon[Schema[Out]])
    McpEvent.Checked(descriptor(summon[McpInput[In]].jsonSchema, payload.schema), summon[McpInput[In]],
      payload.encode, eventPollInterval, eventCheckInterval, check)

  /** Serve the events emitted into `buffer`, every one to every subscriber. */
  def buffered[Out: Schema](buffer: McpEventBuffer[Out]): McpEventHandler[Any] =
    buffered[Unit, Out](buffer, (_, data, _) => Some(data))

  /**
   * Serve the events emitted into `buffer`, each to the subscribers `deliver`
   * gives it to: it sees a subscription's arguments, and answers the payload for
   * it (filtered, or reshaped, e.g. redacted), or `None` to skip it.
   */
  def buffered[In: McpInput, Out: Schema](
    buffer: McpEventBuffer[Out],
    deliver: (In, Out, McpRequestContext) => Option[Out],
  ): McpEventHandler[Any] =
    val payload = McpEvent.Payload(summon[Schema[Out]])
    McpEvent.Buffered(descriptor(summon[McpInput[In]].jsonSchema, payload.schema), summon[McpInput[In]],
      payload.encode, eventPollInterval, buffer, deliver)

  private def descriptor(input: Json.Obj, payload: Json.Obj): McpEventDescriptor =
    McpEventDescriptor(eventName, eventDescription, eventDelivery, input, payload, eventMeta)

object McpEvent:
  val DefaultPollInterval: Duration  = 10.seconds
  val DefaultCheckInterval: Duration = 1.second

  def apply(name: String): McpEvent =
    McpEvent(McpEventName(name), None, DefaultPollInterval, DefaultCheckInterval, None,
      NonEmptyChunk(McpEventDelivery.Poll, McpEventDelivery.Push))

  /** An event payload's schema and encoding. `data` is an object, so another
    * value is wrapped as `{"value": ...}`, as a tool's structured content is. */
  private final case class Payload[A](schemaOf: Schema[A]):
    private val valueSchema = JsonSchemaGen.fromSchema(schemaOf)
    private val isObject = valueSchema.get("type").flatMap(_.asString).contains("object")
    private val encoder = SchemaJsonCodec.jsonEncoder(schemaOf)

    val schema: Json.Obj =
      if isObject then valueSchema
      else Json.Obj("type" -> Json.Str("object"), "properties" -> Json.Obj("value" -> valueSchema),
        "required" -> Json.Arr(Json.Str("value")))

    def encode(value: A): Json.Obj =
      encoder.encodeJson(value, None).toString.fromJson[Json].getOrElse(Json.Null) match
        case obj: Json.Obj => obj
        case other         => Json.Obj("value" -> other)

  private def decode[In](input: McpInput[In], arguments: Json.Obj): IO[McpEventError, In] =
    ZIO.fromEither(input.decode(Some(arguments))).mapError(m => McpEventError.InvalidParams(s"Invalid arguments: $m"))

  private final case class Checked[R, In, Out](
    descriptor: McpEventDescriptor,
    input: McpInput[In],
    encode: Out => Json.Obj,
    pollInterval: Duration,
    checkInterval: Duration,
    checkFn: McpEventRequest[In] => ZIO[R, McpEventError, McpEventBatch[Out]],
  ) extends McpEventHandler[R]:

    def read(
      arguments: Json.Obj,
      cursor: Option[McpEventCursor],
      maxAge: Option[Duration],
      maxEvents: Int,
      ctx: McpRequestContext,
    ): ZIO[R, McpEventError, McpEventRead] =
      decode(input, arguments).flatMap(run(_, cursor, maxAge, maxEvents, ctx))

    def push(
      arguments: Json.Obj,
      cursor: Option[McpEventCursor],
      maxAge: Option[Duration],
      ctx: McpRequestContext,
    ): ZIO[R, McpEventError, ZStream[R, Nothing, McpEventPush]] =
      for
        in    <- decode(input, arguments)
        first <- run(in, cursor, maxAge, McpEvents.DefaultMaxEvents, ctx)
      yield
        // `None` once the subscription has ended.
        val later = ZStream.unfoldChunkZIO(Option((first.end, first.hasMore))):
          case None => ZIO.none
          case Some((position, hasMore)) =>
            (ZIO.sleep(checkInterval).unless(hasMore) *> run(in, position, None, McpEvents.DefaultMaxEvents, ctx)).either.map:
              case Right(next) =>
                val gap = Chunk.fromIterable(Option.when(next.truncated)(
                  McpEventPush.Signal(McpEventSignal.Active(next.start, truncated = true))))
                Some((gap ++ McpEvents.delivered(next), Some((next.end, next.hasMore))))
              case Left(error @ McpEventError.Upstream(_, _)) =>
                Some((Chunk(McpEventPush.Signal(McpEventSignal.Error(error.detail))), Some((position, false))))
              case Left(error) =>
                Some((Chunk(McpEventPush.Signal(McpEventSignal.Terminated(error.detail))), None))
        ZStream.succeed(McpEventPush.Signal(McpEventSignal.Active(first.start, first.truncated))) ++
          ZStream.fromChunk(McpEvents.delivered(first)) ++ later

    private def run(
      in: In,
      cursor: Option[McpEventCursor],
      maxAge: Option[Duration],
      maxEvents: Int,
      ctx: McpRequestContext,
    ): ZIO[R, McpEventError, McpEventRead] =
      checkFn(McpEventRequest(in, cursor, maxAge, maxEvents, ctx)).flatMap: batch =>
        Clock.instant.flatMap: now =>
          ZIO.foreach(batch.events)(e => e.eventId.fold(Random.nextUUID.map(_.toString))(ZIO.succeed(_)).map(e -> _))
            .map: identified =>
              val last = identified.size - 1
              val (_, occurrences) = identified.zipWithIndex.foldLeft((cursor, Chunk.empty[McpEventOccurrence])):
                case ((previous, acc), ((emit, eventId), i)) =>
                  val position = emit.cursor.orElse(if i == last then batch.cursor else previous)
                  (position, acc :+ McpEventOccurrence(eventId, descriptor.name, emit.timestamp.getOrElse(now),
                    encode(emit.data), position))
              McpEventRead(if occurrences.isEmpty then batch.cursor else cursor, occurrences, batch.cursor,
                batch.truncated, batch.hasMore)

  private final case class Buffered[In, Out](
    descriptor: McpEventDescriptor,
    input: McpInput[In],
    encode: Out => Json.Obj,
    pollInterval: Duration,
    buffer: McpEventBuffer[Out],
    deliver: (In, Out, McpRequestContext) => Option[Out],
  ) extends McpEventHandler[Any]:

    def read(
      arguments: Json.Obj,
      cursor: Option[McpEventCursor],
      maxAge: Option[Duration],
      maxEvents: Int,
      ctx: McpRequestContext,
    ): IO[McpEventError, McpEventRead] =
      decode(input, arguments).flatMap: in =>
        buffer.read(descriptor.name, cursor, maxAge, maxEvents)(data => deliver(in, data, ctx).map(encode))

    def push(
      arguments: Json.Obj,
      cursor: Option[McpEventCursor],
      maxAge: Option[Duration],
      ctx: McpRequestContext,
    ): IO[McpEventError, ZStream[Any, Nothing, McpEventPush]] =
      decode(input, arguments).map: in =>
        buffer.follow(descriptor.name, cursor, maxAge)(data => deliver(in, data, ctx).map(encode))

/**
 * An in-memory window of the events a server emits for a push-only upstream,
 * which has no history to read from: poll clients read it, push subscribers
 * follow it. Its cursors belong to this buffer, so they don't survive a
 * restart: a client resuming with an older one is told `truncated` and given
 * what the buffer holds.
 */
trait McpEventBuffer[A]:
  /** Add an event: `eventId` is the upstream's own id when it has one. */
  def emit(data: A, eventId: Option[String] = None, timestamp: Option[Instant] = None): UIO[Unit]

  private[mcp] def read(
    name: McpEventName,
    cursor: Option[McpEventCursor],
    maxAge: Option[Duration],
    maxEvents: Int,
  )(select: A => Option[Json.Obj]): UIO[McpEventRead]

  private[mcp] def follow(
    name: McpEventName,
    cursor: Option[McpEventCursor],
    maxAge: Option[Duration],
  )(select: A => Option[Json.Obj]): ZStream[Any, Nothing, McpEventPush]

object McpEventBuffer:
  /** A buffer that keeps events for `maxAge`, and at most `maxEvents` of them. */
  def make[A](maxAge: Duration = 10.minutes, maxEvents: Int = 10000): UIO[McpEventBuffer[A]] =
    for
      epoch <- Random.nextUUID.map(_.toString.take(8))
      state <- Ref.make(State[A](1L, Chunk.empty))
      // A wake-up per subscriber: one pending is enough, since each reads all it missed.
      wake  <- Hub.sliding[Unit](1)
    yield Live(epoch, maxAge, maxEvents.max(1), state, wake)

  private final case class Entry[A](seq: Long, eventId: String, timestamp: Instant, data: A)

  /** `next` is the next event's sequence number: the head (latest) is `next - 1`. */
  private final case class State[A](next: Long, entries: Chunk[Entry[A]])

  private final case class Live[A](
    epoch: String,
    maxAge: Duration,
    maxEvents: Int,
    state: Ref[State[A]],
    wake: Hub[Unit],
  ) extends McpEventBuffer[A]:

    def emit(data: A, eventId: Option[String], timestamp: Option[Instant]): UIO[Unit] =
      for
        now <- Clock.instant
        id  <- eventId.fold(Random.nextUUID.map(_.toString))(ZIO.succeed(_))
        _   <- state.update: s =>
                 val added = s.entries :+ Entry(s.next, id, timestamp.getOrElse(now), data)
                 State(s.next + 1, retained(added, now).takeRight(maxEvents))
        _   <- wake.publish(())
      yield ()

    def read(
      name: McpEventName,
      cursor: Option[McpEventCursor],
      maxAge: Option[Duration],
      maxEvents: Int,
    )(select: A => Option[Json.Obj]): UIO[McpEventRead] =
      (state.get <*> Clock.instant).map((s, now) => readFrom(s, now, name, cursor, maxAge, maxEvents, select))

    def follow(
      name: McpEventName,
      cursor: Option[McpEventCursor],
      maxAge: Option[Duration],
    )(select: A => Option[Json.Obj]): ZStream[Any, Nothing, McpEventPush] =
      ZStream.unwrapScoped:
        for
          // Subscribe before reading, so nothing emitted in between is missed.
          woken <- wake.subscribe
          first <- read(name, cursor, maxAge, Int.MaxValue)(select)
        yield
          val live = ZStream.fromQueue(woken).mapAccumZIO(first.end): (position, _) =>
            read(name, position, None, Int.MaxValue)(select).map: next =>
              val gap = Chunk.fromIterable(Option.when(next.truncated)(
                McpEventPush.Signal(McpEventSignal.Active(next.start, truncated = true))))
              (next.end, gap ++ McpEvents.delivered(next))
          ZStream.succeed(McpEventPush.Signal(McpEventSignal.Active(first.start, first.truncated))) ++
            ZStream.fromChunk(McpEvents.delivered(first)) ++ live.flattenChunks

    private def retained(entries: Chunk[Entry[A]], now: Instant): Chunk[Entry[A]] =
      val oldest = now.minus(maxAge)
      entries.dropWhile(_.timestamp.isBefore(oldest))

    private def cursorAt(seq: Long): McpEventCursor = McpEventCursor(s"$epoch:$seq")

    /** The sequence number a cursor of this buffer names, if it is one. */
    private def seqOf(cursor: McpEventCursor, head: Long): Option[Long] =
      cursor.value.split(':') match
        case Array(e, n) if e == epoch => n.toLongOption.filter(seq => seq >= 0 && seq <= head)
        case _                         => None

    private def readFrom(
      s: State[A],
      now: Instant,
      name: McpEventName,
      cursor: Option[McpEventCursor],
      maxAge: Option[Duration],
      maxEvents: Int,
      select: A => Option[Json.Obj],
    ): McpEventRead =
      val head = s.next - 1
      cursor match
        case None => McpEventRead(Some(cursorAt(head)), Chunk.empty, Some(cursorAt(head)), truncated = false, hasMore = false)
        case Some(requested) =>
          val entries = retained(s.entries, now)
          val oldest = entries.headOption.fold(s.next)(_.seq)
          // A cursor from before the window (or another buffer: a restart) resumes
          // from the oldest event held, with the gap reported.
          val (from, gap) = seqOf(requested, head) match
            case Some(seq) if seq >= oldest - 1 => (seq, false)
            case _                              => (oldest - 1, true)
          val after = entries.filter(_.seq > from)
          val (tooOld, fresh) = maxAge.fold((Chunk.empty[Entry[A]], after))(age =>
            after.span(_.timestamp.isBefore(now.minus(age))))
          val start = tooOld.lastOption.fold(from)(_.seq)
          val selected = fresh.map(e => e -> select(e.data))
          val matched = selected.zipWithIndex.collect { case ((_, Some(_)), i) => i }
          // Up to `maxEvents` matches; past the last one, skip ahead over entries
          // that match nothing, so the cursor doesn't stay behind them.
          val (taken, hasMore) =
            if matched.size <= maxEvents then (selected, false)
            else (selected.take(matched(maxEvents - 1) + 1), true)
          val events = taken.collect:
            case (e, Some(data)) => McpEventOccurrence(e.eventId, name, e.timestamp, data, Some(cursorAt(e.seq)))
          val end = taken.lastOption.fold(start)(_._1.seq)
          McpEventRead(Some(cursorAt(start)), events, Some(cursorAt(end)), gap || tooOld.nonEmpty, hasMore)

enum McpEventsError:
  case DuplicateEventName(name: McpEventName)

object McpEventsError:
  given CanEqual[McpEventsError, McpEventsError] = CanEqual.derived

/**
 * The Events extension (`io.modelcontextprotocol/events`, SEP-3415, a draft:
 * experimental, and subject to change with it). It serves `events/list`,
 * `events/poll` and `events/stream` (push) for the event types given, on
 * 2026-07-28 requests. Webhook delivery is not implemented.
 */
object McpEvents:
  val Id: McpExtensionId = McpExtensionId.fromValid("io.modelcontextprotocol/events")
  val ListMethod: McpMethodName = McpMethodName.fromValid("events/list")
  val PollMethod: McpMethodName = McpMethodName.fromValid("events/poll")
  val StreamMethod: McpMethodName = McpMethodName.fromValid("events/stream")

  /** How often a push stream sends a heartbeat. */
  val DefaultHeartbeat: Duration = 30.seconds
  /** The most events one poll (or one push check) returns when the client sets no `maxEvents`. */
  val DefaultMaxEvents: Int = 100

  private def routing(name: McpEventName): Option[McpRoutingName] = McpRoutingName.parse(name.value).toOption

  val listOperation: McpOperation[McpEventsListParams, McpEventsListResult] =
    McpOperation.wire(Id, ListMethod, McpEventsWire.listParams, McpEventsWire.listResult,
      protocolSupport = ProtocolSupport.Modern)

  val pollOperation: McpOperation[McpEventsPollParams, McpEventsPollResult] =
    McpOperation.wire(Id, PollMethod, McpEventsWire.pollParams, McpEventsWire.pollResult,
      protocolSupport = ProtocolSupport.Modern, routingName = p => routing(p.name),
      routingHeader = McpRoutingHeader.Required)

  val streamOperation: McpOperation[McpEventsStreamParams, McpEventsStreamResult] =
    McpOperation.wire(Id, StreamMethod, McpEventsWire.streamParams, McpEventsWire.streamResult,
      protocolSupport = ProtocolSupport.Modern, routingName = p => routing(p.name),
      routingHeader = McpRoutingHeader.Required)

  def apply[R](handlers: McpEventHandler[R]*): Either[McpEventsError, McpExtensions[R]] =
    withHeartbeat(DefaultHeartbeat)(handlers*)

  def withHeartbeat[R](heartbeat: Duration)(handlers: McpEventHandler[R]*): Either[McpEventsError, McpExtensions[R]] =
    val all = Chunk.fromIterable(handlers)
    all.groupBy(_.name).collectFirst { case (name, hs) if hs.size > 1 => name } match
      case Some(name) => Left(McpEventsError.DuplicateEventName(name))
      case None =>
        val byName = all.map(h => h.name -> h).toMap
        Right(McpExtensions.trustedVertical(McpServerExtension(
          Id,
          Chunk(listBound(all), pollBound(byName), streamBound(byName, heartbeat)),
          Json.Obj(),
        )))

  /** The occurrences of a read, then its end position. */
  private[mcp] def delivered(read: McpEventRead): Chunk[McpEventPush] =
    read.events.map(e => McpEventPush.Signal(McpEventSignal.Event(e))) :+ McpEventPush.Position(read.end)

  private def listBound[R](handlers: Chunk[McpEventHandler[R]]): McpBoundOperation[R] =
    McpBoundOperation(listOperation): (_, _) =>
      ZIO.succeed(McpEventsListResult(handlers.map(_.descriptor)))

  private def pollBound[R](handlers: Map[McpEventName, McpEventHandler[R]]): McpBoundOperation[R] =
    McpBoundOperation(pollOperation): (params, ctx) =>
      handler(handlers, params.name, McpEventDelivery.Poll).flatMap: h =>
        h.read(params.arguments, params.cursor, params.maxAgeMs.map(Duration.fromMillis), params.maxEvents.getOrElse(DefaultMaxEvents), ctx)
          .map: read =>
            // A poll result carries its cursor on the result, not on each event.
            McpEventsPollResult(read.events.map(_.copy(cursor = None)), read.end, read.truncated, read.hasMore,
              h.pollInterval.toMillis)
      .mapError(methodError)

  private def streamBound[R](handlers: Map[McpEventName, McpEventHandler[R]], heartbeat: Duration): McpBoundOperation[R] =
    McpBoundOperation.streaming(streamOperation): (params, ctx) =>
      (for
        h        <- handler(handlers, params.name, McpEventDelivery.Push)
        pushes   <- h.push(params.arguments, params.cursor, params.maxAgeMs.map(Duration.fromMillis), ctx)
        position <- Ref.make(Option.empty[McpEventCursor])
      yield
        val signals = pushes.mapZIO:
          case McpEventPush.Position(cursor) => position.set(cursor).as(None)
          case McpEventPush.Signal(signal)   => track(position, signal).as(Some(signal))
        .collectSome
        val heartbeats = ZStream.tick(heartbeat).drop(1).mapZIO(_ => position.get).map(McpEventSignal.Heartbeat(_))
        McpOperationStream(
          signals.mergeHaltLeft(heartbeats).map(McpEventsWire.notification(_, ctx.requestId)),
          McpEventsStreamResult(ctx.requestId),
        )
      ).mapError(methodError)

  private def track(position: Ref[Option[McpEventCursor]], signal: McpEventSignal): UIO[Unit] =
    signal match
      case McpEventSignal.Active(cursor, _) => position.set(cursor)
      case McpEventSignal.Event(occurrence) => position.set(occurrence.cursor)
      case _                                => ZIO.unit

  private def handler[R](
    handlers: Map[McpEventName, McpEventHandler[R]],
    name: McpEventName,
    mode: McpEventDelivery,
  ): IO[McpEventError, McpEventHandler[R]] =
    ZIO.fromOption(handlers.get(name)).orElseFail(McpEventError.NotFound(name)).filterOrFail(_.descriptor.delivery.contains(mode))(
      McpEventError.Unsupported("deliveryMode", Some(mode.wire)))

  private def methodError(error: McpEventError): McpMethodError =
    val detail = error.detail
    McpMethodError.Domain(detail.code, detail.message, detail.data)
