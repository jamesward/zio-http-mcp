package com.jamesward.ziohttp.mcp

import zio.*
import zio.test.*

/**
 * MCP Events webhook delivery to Mastra's MCP client (`@mastra/mcp` 2.10.0), end to end; see [[JsEventsClients]].
 * It needs `KnownClients`: it doesn't send `Mcp-Name` on the events methods.
 */
object MastraEventsSpec extends ZIOSpecDefault:
  override def spec =
    JsEventsClients.suite("@mastra/mcp MCP Events over webhooks")(
      JsEventsClients.endToEnd("mastra", JsEventsClients.knownClients),
      test("by default, its events/subscribe without Mcp-Name is refused, as SEP-3415 says"):
        for run <- ZIO.scoped(JsEventsClients.run("mastra", JsEventsClients.spec))
        yield assertTrue(
          run.startingWith("error ").exists(_.contains("Missing required Mcp-Name header")),
          run.startingWith("subscribed ").isEmpty,
          run.leftOver.isEmpty,
        ),
    )
