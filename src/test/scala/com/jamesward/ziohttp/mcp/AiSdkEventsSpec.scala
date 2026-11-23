package com.jamesward.ziohttp.mcp

import zio.test.*

/**
 * MCP Events webhook delivery to the Vercel AI SDK's MCP client (`@ai-sdk/mcp` 2.0.103), end to end; see [[EventsClients]].
 * It needs `KnownClients`: it reads only `capabilities.events`, sends no `Mcp-Name`, and wants the `eventId` as `webhook-id`.
 */
object AiSdkEventsSpec extends ZIOSpecDefault:
  override def spec =
    EventsClients.suite("@ai-sdk/mcp MCP Events over webhooks")(EventsClients.endToEnd(EventsClients.js("ai-sdk"), EventsClients.knownClients))
