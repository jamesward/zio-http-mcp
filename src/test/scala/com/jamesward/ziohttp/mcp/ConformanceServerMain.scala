package com.jamesward.ziohttp.mcp

import zio.*
import zio.http.*

/**
 * Serves [[ConformanceSpec.testServer]] — the fixture server the official MCP
 * conformance kit grades — on a fixed port, so the kit can be pointed at it
 * without Docker:
 *
 * {{{
 * ./sbt "Test/runMain com.jamesward.ziohttp.mcp.ConformanceServerMain" &
 * npx @modelcontextprotocol/conformance@<version> server \
 *   --url http://localhost:3000/mcp --requirements 2026-07-28
 * }}}
 *
 * [[ConformanceSpec]] is still the checked-in way to run the suite: it pins the
 * kit versions and runs them in a container. This entrypoint is for a host that
 * has Node but no Docker daemon, and for iterating on one scenario at a time
 * (`--scenario input-required-result-multi-round`).
 *
 * Set `PORT` to serve somewhere other than 3000.
 */
object ConformanceServerMain extends ZIOAppDefault:

  private val port: Int =
    sys.env.get("PORT").flatMap(_.toIntOption).getOrElse(3000)

  def run: ZIO[Any, Throwable, Nothing] =
    ZIO.logInfo(s"Conformance fixture server on http://localhost:$port/mcp") *>
      Server
        .serve(ConformanceSpec.testServer.routes)
        .provide(Server.defaultWithPort(port), McpServer.State.default)
