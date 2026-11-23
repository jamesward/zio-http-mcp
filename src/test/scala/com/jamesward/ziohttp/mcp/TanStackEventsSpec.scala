package com.jamesward.ziohttp.mcp

import zio.test.*

/**
 * MCP Events webhook delivery to TanStack AI's MCP client (`@tanstack/ai-mcp` 1.3.1), end to end; see [[EventsClients]].
 * It needs `KnownClients`: it wants the `eventId` as `webhook-id`, though it declares the extension.
 */
object TanStackEventsSpec extends ZIOSpecDefault:
  override def spec =
    EventsClients.suite("@tanstack/ai-mcp MCP Events over webhooks")(EventsClients.endToEnd(EventsClients.js("tanstack"), EventsClients.knownClients))
