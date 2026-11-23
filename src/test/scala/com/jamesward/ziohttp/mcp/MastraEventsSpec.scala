package com.jamesward.ziohttp.mcp

import zio.test.*

/** MCP Events webhook delivery to Mastra's MCP client (`@mastra/mcp` 2.10.0), end to end; see [[JsEventsClients]]. */
object MastraEventsSpec extends ZIOSpecDefault:
  override def spec =
    JsEventsClients.suite("@mastra/mcp MCP Events over webhooks", "mastra")
