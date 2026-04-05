package com.jamesward.ziohttp.mcp

import zio.json.*
import zio.json.ast.Json

// --- Opaque Domain Types ---

opaque type SessionId = String
object SessionId:
  def apply(s: String): SessionId = s
  def generate: SessionId = java.util.UUID.randomUUID().toString
  extension (s: SessionId) def value: String = s
  given CanEqual[SessionId, SessionId] = CanEqual.derived
  given JsonEncoder[SessionId] = JsonEncoder.string
  given JsonDecoder[SessionId] = JsonDecoder.string

opaque type ToolName = String
object ToolName:
  def apply(s: String): ToolName = s
  extension (n: ToolName) def value: String = n
  given CanEqual[ToolName, ToolName] = CanEqual.derived
  given JsonEncoder[ToolName] = JsonEncoder.string
  given JsonDecoder[ToolName] = JsonDecoder.string

// --- RequestId (String | Int per JSON-RPC 2.0) ---

enum RequestId:
  case Str(value: String)
  case Num(value: Int)

object RequestId:
  given CanEqual[RequestId, RequestId] = CanEqual.derived

  given JsonEncoder[RequestId] = JsonEncoder[Json].contramap:
    case RequestId.Str(s) => Json.Str(s)
    case RequestId.Num(n) => Json.Num(n)

  given JsonDecoder[RequestId] = JsonDecoder[Json].mapOrFail:
    case Json.Str(s) => Right(RequestId.Str(s))
    case Json.Num(n) =>
      Right(RequestId.Num(n.intValueExact()))
    case other => Left(s"RequestId must be string or integer, got: $other")

// --- Error Codes ---

enum ErrorCode(val code: Int):
  case ParseError        extends ErrorCode(-32700)
  case InvalidRequest    extends ErrorCode(-32600)
  case MethodNotFound    extends ErrorCode(-32601)
  case InvalidParams     extends ErrorCode(-32602)
  case InternalError     extends ErrorCode(-32603)
  case ResourceNotFound  extends ErrorCode(-32002)

object ErrorCode:
  given CanEqual[ErrorCode, ErrorCode] = CanEqual.derived

// --- ToolError (business logic failure, maps to isError: true) ---

case class ToolError(message: String):
  override def toString: String = message

object ToolError:
  given CanEqual[ToolError, ToolError] = CanEqual.derived

// --- McpError typeclass: converts tool handler errors to MCP error messages ---

trait McpError[E]:
  def message(e: E): String

object McpError:
  given McpError[ToolError] with
    def message(e: ToolError): String = e.message

  given McpError[String] with
    def message(e: String): String = e

  given McpError[Throwable] with
    def message(e: Throwable): String =
      Option(e.getMessage).getOrElse(e.toString)

  given McpError[Nothing] with
    def message(e: Nothing): String = e

// --- OptBool: three-state boolean for optional annotation hints ---

enum OptBool:
  case True, False, Unset

  def toOption: Option[Boolean] = this match
    case True  => Some(true)
    case False => Some(false)
    case Unset => None

object OptBool:
  given CanEqual[OptBool, OptBool] = CanEqual.derived

// --- Protocol Constants ---

object McpProtocol:
  val Version: String = "2025-11-25"
  val JsonRpcVersion: String = "2.0"
