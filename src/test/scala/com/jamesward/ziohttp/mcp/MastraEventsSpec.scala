package com.jamesward.ziohttp.mcp

import zio.*
import zio.test.*

/**
 * MCP Events webhook delivery to Mastra's MCP client (`@mastra/mcp` 2.10.0), end to end; see [[EventsClients]].
 * It needs `KnownClients`: it doesn't send `Mcp-Name` on the events methods.
 */
object MastraEventsSpec extends ZIOSpecDefault:
  override def spec =
    EventsClients.suite("@mastra/mcp MCP Events over webhooks")(
      EventsClients.endToEnd(EventsClients.js("mastra"), EventsClients.knownClients),
      test("by default, its events/subscribe without Mcp-Name is refused, as SEP-3415 says"):
        for run <- ZIO.scoped(EventsClients.run(EventsClients.js("mastra"), EventsClients.spec))
        yield assertTrue(
          run.startingWith("error ").exists(_.contains("Missing required Mcp-Name header")),
          run.startingWith("subscribed ").isEmpty,
          run.leftOver.isEmpty,
        ),
    )
