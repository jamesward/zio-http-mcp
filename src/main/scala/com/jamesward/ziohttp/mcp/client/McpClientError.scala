package com.jamesward.ziohttp.mcp.client

import zio.json.ast.Json

/**
 * Error taxonomy for the MCP client.
 *
 *   - [[McpClientError.Transport]] — the HTTP request itself failed (connection
 *     refused, timeout, TLS, malformed URL, IO error reading the body).
 *   - [[McpClientError.Protocol]] — a transport-level violation of the
 *     Streamable HTTP / JSON-RPC framing (bad status, missing/garbled body,
 *     an SSE stream that closed before yielding the matching response).
 *   - [[McpClientError.JsonRpc]] — the server returned a well-formed JSON-RPC
 *     error object for our request.
 *   - [[McpClientError.Decode]] — the JSON-RPC `result` payload didn't match
 *     the type we expected for the method.
 *   - [[McpClientError.ToolFailed]] — the tool ran but reported a failure
 *     (`isError: true`); the message holds the tool's text content. Only the
 *     typed `callToolAs` helpers raise this — the raw `callTool` returns the
 *     [[CallToolResult]] so callers can inspect `isError` themselves.
 *   - [[McpClientError.Auth]] — the OAuth client-credentials flow failed
 *     (discovery, token endpoint, or repeated 401 after a fresh token).
 */
enum McpClientError extends Throwable:
  case Transport(message: String, cause: Option[Throwable] = None)
  case Protocol(message: String)
  case JsonRpc(code: Int, message: String, data: Option[Json] = None)
  case Decode(message: String)
  case Auth(message: String)
  case ToolFailed(message: String)

  override def getMessage: String = this match
    case Transport(m, _) => s"Transport error: $m"
    case Protocol(m)     => s"Protocol error: $m"
    case JsonRpc(c, m, _) => s"JSON-RPC error $c: $m"
    case Decode(m)       => s"Decode error: $m"
    case Auth(m)         => s"Auth error: $m"
    case ToolFailed(m)   => s"Tool returned an error: $m"

  override def getCause: Throwable = this match
    case Transport(_, Some(c)) => c
    case _                     => null

object McpClientError:
  given CanEqual[McpClientError, McpClientError] = CanEqual.derived
