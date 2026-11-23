package com.jamesward.ziohttp.mcp

import zio.test.*

/** MCP Events webhook delivery to TanStack AI's MCP client (`@tanstack/ai-mcp` 1.3.1), end to end; see [[JsEventsClients]]. */
object TanStackEventsSpec extends ZIOSpecDefault:
  override def spec =
    JsEventsClients.suite("@tanstack/ai-mcp MCP Events over webhooks", "tanstack")
