package com.jamesward.ziohttp.mcp

import zio.*
import zio.schema.*

/** Event types shared by `McpEventsSpec`, `StdioServerMain` and the conformance fixture. */
object EventFixtures:

  // The README example: an event type read with a check function. Its cursor is
  // the last second it reported, so a client can resume, and catch up, from one.
  case class Tick(second: Long) derives Schema

  val ticks: McpEventHandler[Any] =
    McpEvent("clock.tick")
      .description("Fires every second")
      .pollInterval(1.second)
      .check[Any, Unit, Tick](tick)

  def tick(request: McpEventRequest[?]): UIO[McpEventBatch[Tick]] =
    Clock.instant.map: now =>
      val current = now.getEpochSecond
      request.cursor.flatMap(_.value.toLongOption) match
        case None => McpEventBatch.empty(Some(McpEventCursor(current.toString)))
        case Some(from) =>
          val upTo = math.min(current, from + request.maxEvents)
          McpEventBatch(
            Chunk.fromIterable((from + 1) to upTo).map(s => McpEventEmit(Tick(s), eventId = Some(s.toString))),
            Some(McpEventCursor(upTo.toString)),
            hasMore = upTo < current,
          )
