package com.jamesward.ziohttp.mcp

import zio.test.*

/** MCP Events webhook delivery to the Vercel AI SDK's MCP client (`@ai-sdk/mcp` 2.0.103), end to end; see [[JsEventsClients]]. */
object AiSdkEventsSpec extends ZIOSpecDefault:
  override def spec =
    JsEventsClients.suite("@ai-sdk/mcp MCP Events over webhooks", "ai-sdk")
