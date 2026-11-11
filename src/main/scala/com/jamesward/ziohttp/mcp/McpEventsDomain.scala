package com.jamesward.ziohttp.mcp

import zio.*
import zio.json.*
import zio.json.ast.Json

import java.time.Instant
import scala.util.Try

// --- MCP Events extension (io.modelcontextprotocol/events, SEP-3415 draft): the wire model ---

/** An event type's name, e.g. `email.received`. */
opaque type McpEventName = String

object McpEventName:
  def apply(value: String): McpEventName = value
  extension (name: McpEventName) def value: String = name
  given CanEqual[McpEventName, McpEventName] = CanEqual.derived

/** A subscription's position in an event stream: opaque to the client, which
  * stores it and hands it back to resume. */
opaque type McpEventCursor = String

object McpEventCursor:
  def apply(value: String): McpEventCursor = value
  extension (cursor: McpEventCursor) def value: String = cursor
  given CanEqual[McpEventCursor, McpEventCursor] = CanEqual.derived

/** How a client can receive an event type's events. */
enum McpEventDelivery(val wire: String):
  /** `events/poll`: request/response, no server state. */
  case Poll extends McpEventDelivery("poll")
  /** `events/stream`: a long-lived request whose response streams the events. */
  case Push extends McpEventDelivery("push")
  /** `events/subscribe`: the server POSTs events to a callback URL. */
  case Webhook extends McpEventDelivery("webhook")

object McpEventDelivery:
  given CanEqual[McpEventDelivery, McpEventDelivery] = CanEqual.derived
  def fromWire(value: String): Option[McpEventDelivery] = values.find(_.wire == value)

/** An entry of `events/list`: an event type a client can subscribe to. */
final case class McpEventDescriptor(
  name: McpEventName,
  description: Option[String],
  delivery: NonEmptyChunk[McpEventDelivery],
  inputSchema: Json.Obj,
  payloadSchema: Json.Obj,
  meta: Option[Json.Obj] = None,
)

object McpEventDescriptor:
  given CanEqual[McpEventDescriptor, McpEventDescriptor] = CanEqual.derived

/**
 * One event, as a poll result lists it and a push stream delivers it.
 *
 * @param eventId   identifies the upstream event, for deduplication within a subscription
 * @param timestamp when the event occurred
 * @param data      the payload, conforming to the event type's `payloadSchema`
 * @param cursor    the subscription's position after this event (push only:
 *                  a poll result carries its cursor on the result); `None` for
 *                  an event type without replay
 */
final case class McpEventOccurrence(
  eventId: String,
  name: McpEventName,
  timestamp: Instant,
  data: Json.Obj,
  cursor: Option[McpEventCursor] = None,
  meta: Option[Json.Obj] = None,
)

object McpEventOccurrence:
  given CanEqual[McpEventOccurrence, McpEventOccurrence] = CanEqual.derived

final case class McpEventsListParams(cursor: Option[String] = None)

object McpEventsListParams:
  given CanEqual[McpEventsListParams, McpEventsListParams] = CanEqual.derived

final case class McpEventsListResult(events: Chunk[McpEventDescriptor], nextCursor: Option[String] = None)

object McpEventsListResult:
  given CanEqual[McpEventsListResult, McpEventsListResult] = CanEqual.derived

/**
 * `events/poll`: one subscription's events since `cursor`.
 *
 * @param cursor    where to read from; `None` starts from now, with no events
 * @param maxAgeMs  skip events older than this, when replaying from a cursor
 * @param maxEvents the most events to return; the server's default when unset
 */
final case class McpEventsPollParams(
  name: McpEventName,
  arguments: Json.Obj = Json.Obj(),
  cursor: Option[McpEventCursor] = None,
  maxAgeMs: Option[Long] = None,
  maxEvents: Option[Int] = None,
)

object McpEventsPollParams:
  given CanEqual[McpEventsPollParams, McpEventsPollParams] = CanEqual.derived

/**
 * @param cursor     the position to poll from next
 * @param truncated  events were skipped: the server read from later than the cursor given
 * @param hasMore    more events are ready: poll again at once, ignoring `nextPollMs`
 * @param nextPollMs how long the server suggests waiting before the next poll
 */
final case class McpEventsPollResult(
  events: Chunk[McpEventOccurrence],
  cursor: Option[McpEventCursor],
  truncated: Boolean,
  hasMore: Boolean,
  nextPollMs: Long,
)

object McpEventsPollResult:
  given CanEqual[McpEventsPollResult, McpEventsPollResult] = CanEqual.derived

/** `events/stream`: open a push subscription, delivering from `cursor` (`None`: from now). */
final case class McpEventsStreamParams(
  name: McpEventName,
  arguments: Json.Obj = Json.Obj(),
  cursor: Option[McpEventCursor] = None,
  maxAgeMs: Option[Long] = None,
)

object McpEventsStreamParams:
  given CanEqual[McpEventsStreamParams, McpEventsStreamParams] = CanEqual.derived

/** The final frame of a push stream the server ended. It carries nothing but the
  * subscription id: what matters came in the notifications before it. */
final case class McpEventsStreamResult(subscriptionId: Option[RequestId])

object McpEventsStreamResult:
  given CanEqual[McpEventsStreamResult, McpEventsStreamResult] = CanEqual.derived

/** What a push subscription delivers, in order. */
enum McpEventSignal:
  /** The subscription is live from `cursor`; first, and again after a gap (`truncated`). */
  case Active(cursor: Option[McpEventCursor], truncated: Boolean)
  case Event(occurrence: McpEventOccurrence)
  /** Nothing new up to `cursor`, which is safe to persist. */
  case Heartbeat(cursor: Option[McpEventCursor])
  /** A recoverable failure: the subscription stays active. */
  case Error(error: ErrorDetail)
  /** The subscription ended, e.g. because access was revoked. */
  case Terminated(error: ErrorDetail)

object McpEventSignal:
  given CanEqual[McpEventSignal, McpEventSignal] = CanEqual.derived

/**
 * Why the server can't serve a subscription. The numeric codes are SEP-3415's
 * and stay provisional until it is accepted.
 */
enum McpEventError:
  /** The arguments don't match the event type's `inputSchema`, or the request is malformed. */
  case InvalidParams(message: String)
  /** No such event type. */
  case NotFound(name: McpEventName)
  /** The principal may not subscribe with these arguments, or lost access. */
  case Forbidden(reason: String)
  /** A server limit was reached: `limit` names it, `max` is its ceiling. */
  case ResourceExhausted(limit: String, max: Option[Long] = None)
  /** A well-formed request for something not supported here, e.g. a delivery mode. */
  case Unsupported(feature: String, value: Option[String] = None)
  /** A transient upstream failure: on a push stream it is reported and the stream goes on. */
  case Upstream(message: String, data: Option[Json] = None)

  def detail: ErrorDetail = this match
    case InvalidParams(message) => ErrorDetail(ErrorCode.InvalidParams.code, message)
    case NotFound(name) =>
      ErrorDetail(McpEventError.NotFoundCode, s"Unknown event type '${name.value}'",
        Some(Json.Obj("kind" -> Json.Str("event"))))
    case Forbidden(reason) =>
      ErrorDetail(McpEventError.ForbiddenCode, "Forbidden", Some(Json.Obj("reason" -> Json.Str(reason))))
    case ResourceExhausted(limit, max) =>
      ErrorDetail(McpEventError.ResourceExhaustedCode, "ResourceExhausted", Some(Json.Obj(Chunk[(String, Json)](
        "limit" -> Json.Str(limit)) ++ Chunk.fromIterable(max.map(m => "max" -> Json.Num(m))))))
    case Unsupported(feature, value) =>
      ErrorDetail(McpEventError.UnsupportedCode, "Unsupported", Some(Json.Obj(Chunk[(String, Json)](
        "feature" -> Json.Str(feature)) ++ Chunk.fromIterable(value.map(v => "value" -> Json.Str(v))))))
    case Upstream(message, data) => ErrorDetail(ErrorCode.InternalError.code, message, data)

object McpEventError:
  given CanEqual[McpEventError, McpEventError] = CanEqual.derived

  val NotFoundCode: Int          = -32023
  val ForbiddenCode: Int         = -32024
  val ResourceExhaustedCode: Int = -32025
  val UnsupportedCode: Int       = -32026

/** The JSON shapes of the Events messages: absent optional fields are omitted. */
private[mcp] object McpEventsWire:
  private type Fields = Chunk[(String, Json)]

  val listParams: McpWireCodec[McpEventsListParams] = McpWireCodec(
    obj(_).flatMap(o => optionalString(o, "cursor").map(McpEventsListParams(_))),
    p => Right(Json.Obj(Chunk.fromIterable(p.cursor.map(c => "cursor" -> Json.Str(c))))),
  )

  val listResult: McpWireCodec[McpEventsListResult] = McpWireCodec(
    obj(_).flatMap: o =>
      for
        raw    <- o.get("events").flatMap(_.asArray).toRight("Missing or invalid 'events' array")
        events <- traverse(raw)(descriptor)
        next   <- optionalString(o, "nextCursor")
      yield McpEventsListResult(events, next),
    r => Right(Json.Obj(Chunk[(String, Json)]("events" -> Json.Arr(r.events.map(descriptorJson))) ++
      Chunk.fromIterable(r.nextCursor.map(c => "nextCursor" -> Json.Str(c))))),
  )

  val pollParams: McpWireCodec[McpEventsPollParams] = McpWireCodec(
    obj(_).flatMap: o =>
      for
        name      <- eventName(o)
        arguments <- argumentsOf(o)
        cursor    <- cursorOf(o)
        maxAge    <- optionalLong(o, "maxAgeMs", min = 0)
        maxEvents <- optionalLong(o, "maxEvents", min = 1)
      yield McpEventsPollParams(name, arguments, cursor, maxAge, maxEvents.map(_.min(Int.MaxValue).toInt)),
    p => Right(Json.Obj(subscription(p.name, p.arguments, p.cursor, p.maxAgeMs) ++
      Chunk.fromIterable(p.maxEvents.map(m => "maxEvents" -> Json.Num(m))))),
  )

  val pollResult: McpWireCodec[McpEventsPollResult] = McpWireCodec(
    obj(_).flatMap: o =>
      for
        raw       <- o.get("events").flatMap(_.asArray).toRight("Missing or invalid 'events' array")
        events    <- traverse(raw)(occurrence)
        cursor    <- cursorOf(o)
        truncated <- optionalBoolean(o, "truncated")
        hasMore   <- optionalBoolean(o, "hasMore")
        next      <- optionalLong(o, "nextPollMs", min = 0)
      yield McpEventsPollResult(events, cursor, truncated, hasMore, next.getOrElse(0L)),
    r => Right(Json.Obj(
      "events"     -> Json.Arr(r.events.map(occurrenceJson)),
      "cursor"     -> cursorJson(r.cursor),
      "truncated"  -> Json.Bool(r.truncated),
      "hasMore"    -> Json.Bool(r.hasMore),
      "nextPollMs" -> Json.Num(r.nextPollMs),
    )),
  )

  val streamParams: McpWireCodec[McpEventsStreamParams] = McpWireCodec(
    obj(_).flatMap: o =>
      for
        name      <- eventName(o)
        arguments <- argumentsOf(o)
        cursor    <- cursorOf(o)
        maxAge    <- optionalLong(o, "maxAgeMs", min = 0)
      yield McpEventsStreamParams(name, arguments, cursor, maxAge),
    p => Right(Json.Obj(subscription(p.name, p.arguments, p.cursor, p.maxAgeMs))),
  )

  val streamResult: McpWireCodec[McpEventsStreamResult] = McpWireCodec(
    obj(_).map(o => McpEventsStreamResult(subscriptionIdOf(o))),
    r => Right(Json.Obj(Chunk.fromIterable(r.subscriptionId.map(id => "_meta" -> subscriptionMeta(id))))),
  )

  val ActiveMethod: McpMethodName     = McpMethodName.fromValid("notifications/events/active")
  val EventMethod: McpMethodName      = McpMethodName.fromValid("notifications/events/event")
  val HeartbeatMethod: McpMethodName  = McpMethodName.fromValid("notifications/events/heartbeat")
  val ErrorMethod: McpMethodName      = McpMethodName.fromValid("notifications/events/error")
  val TerminatedMethod: McpMethodName = McpMethodName.fromValid("notifications/events/terminated")

  /** A push signal as the notification that carries it on subscription `id`'s stream. */
  def notification(signal: McpEventSignal, id: Option[RequestId]): McpStreamNotification =
    val (method, fields) = signal match
      case McpEventSignal.Active(cursor, truncated) =>
        ActiveMethod -> Chunk[(String, Json)]("cursor" -> cursorJson(cursor), "truncated" -> Json.Bool(truncated))
      case McpEventSignal.Event(occ)        => EventMethod -> occurrenceJson(occ).fields.filterNot(_._1 == "_meta")
      case McpEventSignal.Heartbeat(cursor) => HeartbeatMethod -> Chunk[(String, Json)]("cursor" -> cursorJson(cursor))
      case McpEventSignal.Error(error)      => ErrorMethod -> Chunk[(String, Json)]("error" -> errorJson(error))
      case McpEventSignal.Terminated(error) => TerminatedMethod -> Chunk[(String, Json)]("error" -> errorJson(error))
    val occurrenceMeta = signal match
      case McpEventSignal.Event(occ) => occ.meta.map(_.fields).getOrElse(Chunk.empty)
      case _                         => Chunk.empty
    val meta = occurrenceMeta.filterNot(_._1 == McpMeta.SubscriptionId) ++
      Chunk.fromIterable(id.map(i => McpMeta.SubscriptionId -> i.asJson))
    val withMeta = if meta.isEmpty then fields else fields :+ ("_meta" -> Json.Obj(meta))
    McpStreamNotification(method, Json.Obj(withMeta))

  /** Read the signal an events notification carries. */
  def signal(method: String, params: Json.Obj): Either[String, McpEventSignal] =
    method match
      case m if m == ActiveMethod.value =>
        for
          cursor    <- cursorOf(params)
          truncated <- optionalBoolean(params, "truncated")
        yield McpEventSignal.Active(cursor, truncated)
      case m if m == EventMethod.value =>
        val meta = params.get("_meta").flatMap(_.asObject).map(o => Json.Obj(o.fields.filterNot(_._1 == McpMeta.SubscriptionId)))
        occurrence(params).map(occ => McpEventSignal.Event(occ.copy(meta = meta.filter(_.fields.nonEmpty))))
      case m if m == HeartbeatMethod.value  => cursorOf(params).map(McpEventSignal.Heartbeat(_))
      case m if m == ErrorMethod.value      => errorOf(params).map(McpEventSignal.Error(_))
      case m if m == TerminatedMethod.value => errorOf(params).map(McpEventSignal.Terminated(_))
      case other                            => Left(s"Not an events notification: $other")

  def subscriptionIdOf(params: Json.Obj): Option[RequestId] =
    params.get("_meta").flatMap(_.asObject).flatMap(_.get(McpMeta.SubscriptionId)).flatMap(_.as[RequestId].toOption)

  private def subscriptionMeta(id: RequestId): Json = Json.Obj(McpMeta.SubscriptionId -> id.asJson)

  private def subscription(
    name: McpEventName,
    arguments: Json.Obj,
    cursor: Option[McpEventCursor],
    maxAgeMs: Option[Long],
  ): Fields =
    Chunk[(String, Json)]("name" -> Json.Str(name.value), "arguments" -> arguments, "cursor" -> cursorJson(cursor)) ++
      Chunk.fromIterable(maxAgeMs.map(m => "maxAgeMs" -> Json.Num(m)))

  private def descriptor(json: Json): Either[String, McpEventDescriptor] =
    for
      o        <- obj(json)
      name     <- eventName(o)
      desc     <- optionalString(o, "description")
      rawModes <- o.get("delivery").flatMap(_.asArray).toRight("Missing or invalid 'delivery' array")
      // A mode this client doesn't know is skipped: the extension may add more.
      modes     = rawModes.flatMap(_.asString).flatMap(McpEventDelivery.fromWire)
      delivery <- NonEmptyChunk.fromChunk(modes).toRight(s"Event type '$name' offers no known delivery mode")
      input    <- o.get("inputSchema").flatMap(_.asObject).toRight("Missing or invalid 'inputSchema'")
      payload  <- o.get("payloadSchema").flatMap(_.asObject).toRight("Missing or invalid 'payloadSchema'")
    yield McpEventDescriptor(name, desc, delivery, input, payload, o.get("_meta").flatMap(_.asObject))

  private def descriptorJson(d: McpEventDescriptor): Json =
    Json.Obj(
      Chunk[(String, Json)]("name" -> Json.Str(d.name.value)) ++
        Chunk.fromIterable(d.description.map(text => "description" -> Json.Str(text))) ++
        Chunk[(String, Json)](
          "delivery"      -> Json.Arr(d.delivery.toChunk.map(mode => Json.Str(mode.wire))),
          "inputSchema"   -> d.inputSchema,
          "payloadSchema" -> d.payloadSchema,
        ) ++
        Chunk.fromIterable(d.meta.map(m => "_meta" -> m))
    )

  private def occurrence(json: Json): Either[String, McpEventOccurrence] =
    for
      o         <- obj(json)
      eventId   <- o.get("eventId").flatMap(_.asString).toRight("Missing or invalid 'eventId'")
      name      <- eventName(o)
      rawTime   <- o.get("timestamp").flatMap(_.asString).toRight("Missing or invalid 'timestamp'")
      timestamp <- Try(Instant.parse(rawTime)).toEither.left.map(_ => s"Invalid 'timestamp' '$rawTime'")
      data      <- o.get("data").flatMap(_.asObject).toRight("Missing or invalid 'data' object")
      cursor    <- cursorOf(o)
    yield McpEventOccurrence(eventId, name, timestamp, data, cursor, o.get("_meta").flatMap(_.asObject))

  private def occurrenceJson(occ: McpEventOccurrence): Json.Obj =
    Json.Obj(
      Chunk[(String, Json)](
        "eventId"   -> Json.Str(occ.eventId),
        "name"      -> Json.Str(occ.name.value),
        "timestamp" -> Json.Str(occ.timestamp.toString),
        "data"      -> occ.data,
      ) ++
        Chunk.fromIterable(occ.cursor.map(c => "cursor" -> Json.Str(c.value))) ++
        Chunk.fromIterable(occ.meta.map(m => "_meta" -> m))
    )

  private def errorJson(error: ErrorDetail): Json =
    Json.Obj(Chunk[(String, Json)]("code" -> Json.Num(error.code), "message" -> Json.Str(error.message)) ++
      Chunk.fromIterable(error.data.map("data" -> _)))

  private def errorOf(params: Json.Obj): Either[String, ErrorDetail] =
    params.get("error").toRight("Missing 'error'").flatMap(_.as[ErrorDetail])

  private def cursorJson(cursor: Option[McpEventCursor]): Json = cursor.fold(Json.Null)(c => Json.Str(c.value))

  // Absent means null: a missing cursor is the same as `cursor: null`.
  private def cursorOf(o: Json.Obj): Either[String, Option[McpEventCursor]] =
    o.get("cursor") match
      case None | Some(_: Json.Null.type) => Right(None)
      case Some(Json.Str(value))  => Right(Some(McpEventCursor(value)))
      case Some(_)                => Left("Invalid 'cursor': expected a string or null")

  private def eventName(o: Json.Obj): Either[String, McpEventName] =
    o.get("name") match
      case Some(Json.Str(value)) if value.nonEmpty => Right(McpEventName(value))
      case _                                       => Left("Missing or invalid 'name': expected a nonempty string")

  private def argumentsOf(o: Json.Obj): Either[String, Json.Obj] =
    o.get("arguments") match
      case None | Some(_: Json.Null.type) => Right(Json.Obj())
      case Some(args: Json.Obj)   => Right(args)
      case Some(_)                => Left("Invalid 'arguments': expected an object")

  private def obj(json: Json): Either[String, Json.Obj] = json.asObject.toRight("Expected a JSON object")

  private def optionalString(o: Json.Obj, field: String): Either[String, Option[String]] =
    o.get(field) match
      case None | Some(_: Json.Null.type) => Right(None)
      case Some(Json.Str(value))  => Right(Some(value))
      case Some(_)                => Left(s"Invalid '$field': expected a string")

  private def optionalBoolean(o: Json.Obj, field: String): Either[String, Boolean] =
    o.get(field) match
      case None | Some(_: Json.Null.type) => Right(false)
      case Some(Json.Bool(value)) => Right(value)
      case Some(_)                => Left(s"Invalid '$field': expected a boolean")

  private def optionalLong(o: Json.Obj, field: String, min: Long): Either[String, Option[Long]] =
    o.get(field) match
      case None | Some(_: Json.Null.type) => Right(None)
      case Some(Json.Num(value)) =>
        Try(value.longValueExact()).toOption.filter(_ >= min).map(Some(_))
          .toRight(s"Invalid '$field': expected an integer of at least $min")
      case Some(_) => Left(s"Invalid '$field': expected an integer")

  private def traverse[A, B](values: Chunk[A])(f: A => Either[String, B]): Either[String, Chunk[B]] =
    values.foldLeft[Either[String, Chunk[B]]](Right(Chunk.empty)):
      case (Right(acc), value) => f(value).map(acc :+ _)
      case (left @ Left(_), _) => left
