package com.jamesward.ziohttp.mcp.client

import com.jamesward.ziohttp.mcp.RequestId
import zio.*
import zio.http.Headers
import zio.json.ast.Json

/**
 * How the client's JSON-RPC messages reach a server: Streamable HTTP or a stdio
 * subprocess. The protocol layer (negotiation, `_meta`, MRTR, tasks) sits above
 * it and is the same for both.
 */
private[client] trait McpWire:
  /** Sends a request and gives its raw `result`, or the server's JSON-RPC error. */
  def request(o: Outgoing): IO[McpClientError, Json]

  /** Sends the notification `body` (a serialized `method` notification). */
  def notify(method: String, body: String): IO[McpClientError, Unit]

  /** Releases the connection. */
  def close: IO[McpClientError, Unit]

/**
 * An outgoing request body plus the transport decisions for it: correlation id,
 * extra (modern routing) headers, and whether to attach the session. The last
 * two are Streamable HTTP's; the stdio wire carries everything in the body.
 */
private[client] final case class Outgoing(
  body: String,
  id: RequestId,
  extraHeaders: Headers,
  sendSession: Boolean,
)
