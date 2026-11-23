package com.jamesward.ziohttp.mcp

import zio.test.*

/**
 * MCP Events webhook delivery to TanStack AI's MCP client (`@tanstack/ai-mcp` 1.3.1), end to end; see [[JsEventsClients]].
 * It needs `KnownClients`: it wants the `eventId` as `webhook-id`, though it declares the extension.
 */
object TanStackEventsSpec extends ZIOSpecDefault:
  override def spec =
    JsEventsClients.suite("@tanstack/ai-mcp MCP Events over webhooks")(JsEventsClients.endToEnd("tanstack", JsEventsClients.knownClients))
