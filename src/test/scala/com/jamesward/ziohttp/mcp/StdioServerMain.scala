package com.jamesward.ziohttp.mcp

import zio.*
import zio.schema.*

/**
 * An [[McpServer]] on stdio ([[McpServer.serveStdio]]), launched as a subprocess
 * by `McpServerStdioSpec`. Its tools write to stdout and log, which must not
 * reach the protocol channel, and `slow` reports its interruption on stderr.
 */
object StdioServerMain extends ZIOAppDefault:

  case class EchoInput(text: String) derives Schema

  val server: McpServer[Any] =
    McpServer("stdio-server", "1.0.0")
      .tool(
        McpTool("echo").description("Echoes its text").handle[Any, Nothing, EchoInput, String] { in =>
          // Neither may corrupt the protocol: serveStdio sends both to stderr.
          ZIO.succeed(java.lang.System.out.println(s"noise: ${in.text}")) *>
            ZIO.logInfo(s"echoing ${in.text}").as(s"echo: ${in.text}")
        }
      )
      .tool(
        McpTool("slow").description("Runs until cancelled").handle[Any, Nothing, String](
          ZIO.never.onInterrupt(ZIO.succeed(java.lang.System.err.println("slow interrupted")))
        )
      )

  def run = server.serveStdio
