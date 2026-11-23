package com.jamesward.ziohttp.mcp

import zio.test.*

/**
 * MCP Events webhook delivery to the Python MCP SDK's Events client (python-sdk PR 3822, unreleased), end to end; see
 * [[EventsClients]]. It follows SEP-3415, so it runs against the spec default.
 */
object PythonEventsSpec extends ZIOSpecDefault:
  override def spec =
    EventsClients.suite("Python MCP SDK (PR 3822) MCP Events over webhooks")(
      EventsClients.endToEnd(EventsClients.python, EventsClients.spec))
