package com.jamesward.ziohttp.mcp

import zio.*
import zio.json.*
import zio.json.ast.Json

// --- Incoming JSON-RPC messages (Request or Notification) ---

enum JsonRpcMessage:
  case Request(id: RequestId, method: String, params: Option[Json.Obj])
  case Notification(method: String, params: Option[Json.Obj])

object JsonRpcMessage:
  given CanEqual[JsonRpcMessage, JsonRpcMessage] = CanEqual.derived

  given JsonDecoder[JsonRpcMessage] = JsonDecoder[Json.Obj].mapOrFail: obj =>
    val jsonrpc = obj.get("jsonrpc").flatMap(_.asString)
    if !jsonrpc.contains(McpProtocol.JsonRpcVersion) then
      Left(s"Expected jsonrpc: '${McpProtocol.JsonRpcVersion}'")
    else
      val method = obj.get("method").flatMap(_.asString)
      val id     = obj.get("id").flatMap(_.as[RequestId].toOption)
      val params = obj.get("params").flatMap(_.asObject)
      method match
        case Some(m) =>
          id match
            case Some(reqId) => Right(JsonRpcMessage.Request(reqId, m, params))
            case None        => Right(JsonRpcMessage.Notification(m, params))
        case None =>
          Left("Missing 'method' field in JSON-RPC message")

  given JsonEncoder[JsonRpcMessage] = JsonEncoder[Json.Obj].contramap:
    case JsonRpcMessage.Request(id, method, params) =>
      val fields = Chunk(
        "jsonrpc" -> Json.Str(McpProtocol.JsonRpcVersion),
        "id"      -> id.asJson,
        "method"  -> Json.Str(method),
      ) ++ params.map(p => "params" -> (p: Json)).fold(Chunk.empty[(String, Json)])(v => Chunk(v))
      Json.Obj(fields)
    case JsonRpcMessage.Notification(method, params) =>
      val fields = Chunk(
        "jsonrpc" -> Json.Str(McpProtocol.JsonRpcVersion),
        "method"  -> Json.Str(method),
      ) ++ params.map(p => "params" -> (p: Json)).fold(Chunk.empty[(String, Json)])(v => Chunk(v))
      Json.Obj(fields)

// --- Outgoing JSON-RPC response (success) ---

case class JsonRpcResponse(id: RequestId, result: Json)

object JsonRpcResponse:
  given CanEqual[JsonRpcResponse, JsonRpcResponse] = CanEqual.derived

  given JsonEncoder[JsonRpcResponse] = JsonEncoder[Json.Obj].contramap: r =>
    Json.Obj(Chunk(
      "jsonrpc" -> Json.Str(McpProtocol.JsonRpcVersion),
      "id"      -> r.id.asJson,
      "result"  -> r.result,
    ))

// --- Outgoing JSON-RPC error response ---

case class ErrorDetail(code: Int, message: String, data: Option[Json] = None)

object ErrorDetail:
  given CanEqual[ErrorDetail, ErrorDetail] = CanEqual.derived
  given JsonCodec[ErrorDetail] = DeriveJsonCodec.gen[ErrorDetail]

case class JsonRpcError(id: Option[RequestId], error: ErrorDetail)

object JsonRpcError:
  given CanEqual[JsonRpcError, JsonRpcError] = CanEqual.derived

  def fromCode(id: Option[RequestId], code: ErrorCode, message: String): JsonRpcError =
    JsonRpcError(id, ErrorDetail(code.code, message))

  given JsonEncoder[JsonRpcError] = JsonEncoder[Json.Obj].contramap: e =>
    val idJson: Json = e.id.fold(Json.Null)(_.asJson)
    val errorJson = Json.Obj(Chunk(
      "code"    -> Json.Num(e.error.code),
      "message" -> Json.Str(e.error.message),
    ) ++ e.error.data.fold(Chunk.empty[(String, Json)])(d => Chunk("data" -> d)))
    Json.Obj(Chunk(
      "jsonrpc" -> Json.Str(McpProtocol.JsonRpcVersion),
      "id"      -> idJson,
      "error"   -> errorJson,
    ))
