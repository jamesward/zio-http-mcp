package com.jamesward.ziohttp.mcp

import zio.*
import zio.json.ast.Json
import zio.schema.Schema

enum ProtocolSupport:
  case All, Legacy, Modern

  def supports(version: ProtocolVersion): Boolean = this match
    case All    => true
    case Legacy => !version.isStateless
    case Modern => version.isStateless

object ProtocolSupport:
  given CanEqual[ProtocolSupport, ProtocolSupport] = CanEqual.derived

enum ClientSupportPolicy:
  case Optional, Required

object ClientSupportPolicy:
  given CanEqual[ClientSupportPolicy, ClientSupportPolicy] = CanEqual.derived

enum McpCacheScope(val wire: String):
  case Public extends McpCacheScope("public")
  case Private extends McpCacheScope("private")

object McpCacheScope:
  given CanEqual[McpCacheScope, McpCacheScope] = CanEqual.derived

enum McpCacheTtlError:
  case NotPositive(value: Long)

object McpCacheTtlError:
  given CanEqual[McpCacheTtlError, McpCacheTtlError] = CanEqual.derived

opaque type McpCacheTtl = Long

object McpCacheTtl:
  def parse(milliseconds: Long): Either[McpCacheTtlError, McpCacheTtl] =
    if milliseconds > 0 then Right(milliseconds)
    else Left(McpCacheTtlError.NotPositive(milliseconds))

  val OneHour: McpCacheTtl = 3600000L

  extension (ttl: McpCacheTtl) def milliseconds: Long = ttl
  given CanEqual[McpCacheTtl, McpCacheTtl] = CanEqual.derived

enum McpCachePolicy:
  case NotCacheable
  case Cacheable(ttl: McpCacheTtl, scope: McpCacheScope)

object McpCachePolicy:
  val Default: McpCachePolicy = Cacheable(McpCacheTtl.OneHour, McpCacheScope.Public)
  given CanEqual[McpCachePolicy, McpCachePolicy] = CanEqual.derived

enum McpMethodError:
  case InvalidParams(message: String)
  case InvalidResult(message: String)
  case UnsupportedProtocol(version: ProtocolVersion)
  case MissingRequiredClientExtension(id: McpExtensionId)
  case RoutingNameMismatch(expected: McpRoutingName, actual: String)
  case Domain(code: Int, message: String, data: Option[Json] = None)

object McpMethodError:
  given CanEqual[McpMethodError, McpMethodError] = CanEqual.derived

final case class McpOperation[Params, Result] private (
  extensionId: McpExtensionId,
  method: McpMethodName,
  protocolSupport: ProtocolSupport,
  clientSupport: ClientSupportPolicy,
  cachePolicy: McpCachePolicy,
  routingName: Params => Option[McpRoutingName],
  private[mcp] val paramsCodec: McpWireCodec[Params],
  private[mcp] val resultCodec: McpWireCodec[Result],
):
  def decodeParams(json: Json): Either[String, Params] = paramsCodec.decode(json)
  def encodeParams(params: Params): Either[String, Json] = paramsCodec.encode(params)
  def decodeResult(json: Json): Either[String, Result] = resultCodec.decode(json)
  def encodeResult(result: Result): Either[String, Json] = resultCodec.encode(result)

object McpOperation:
  def apply[Params: Schema, Result: Schema](
    extensionId: McpExtensionId,
    method: McpMethodName,
    protocolSupport: ProtocolSupport = ProtocolSupport.All,
    clientSupport: ClientSupportPolicy = ClientSupportPolicy.Optional,
    cachePolicy: McpCachePolicy = McpCachePolicy.NotCacheable,
    routingName: Params => Option[McpRoutingName] = (_: Params) => None,
  ): McpOperation[Params, Result] =
    val paramsSchema = summon[Schema[Params]]
    val resultSchema = summon[Schema[Result]]
    McpOperation(
      extensionId,
      method,
      protocolSupport,
      clientSupport,
      cachePolicy,
      routingName,
      McpWireCodec.schema(paramsSchema),
      McpWireCodec.schema(resultSchema),
    )

  def wire[Params, Result](
    extensionId: McpExtensionId,
    method: McpMethodName,
    paramsCodec: McpWireCodec[Params],
    resultCodec: McpWireCodec[Result],
    protocolSupport: ProtocolSupport = ProtocolSupport.All,
    clientSupport: ClientSupportPolicy = ClientSupportPolicy.Optional,
    cachePolicy: McpCachePolicy = McpCachePolicy.NotCacheable,
    routingName: Params => Option[McpRoutingName] = (_: Params) => None,
  ): McpOperation[Params, Result] =
    McpOperation(
      extensionId,
      method,
      protocolSupport,
      clientSupport,
      cachePolicy,
      routingName,
      paramsCodec,
      resultCodec,
    )

trait McpBoundOperation[-R]:
  def operation: McpOperation[?, ?]
  private[mcp] def invoke(params: Json.Obj, ctx: McpRequestContext): ZIO[R, McpMethodError, Json.Obj]
  private[mcp] def routingName(params: Json.Obj): Either[McpMethodError, Option[McpRoutingName]]

object McpBoundOperation:
  def apply[R, Params, Result](operationValue: McpOperation[Params, Result])(
    handler: (Params, McpRequestContext) => ZIO[R, McpMethodError, Result]
  ): McpBoundOperation[R] =
    Bound(operationValue, handler)

  private final case class Bound[R, Params, Result](
    typedOperation: McpOperation[Params, Result],
    handler: (Params, McpRequestContext) => ZIO[R, McpMethodError, Result],
  ) extends McpBoundOperation[R]:
    val operation: McpOperation[?, ?] = typedOperation

    private def decode(params: Json.Obj): Either[McpMethodError, Params] =
      typedOperation.paramsCodec.decode(params).left.map(McpMethodError.InvalidParams.apply)

    def routingName(params: Json.Obj): Either[McpMethodError, Option[McpRoutingName]] =
      decode(params).map(typedOperation.routingName)

    def invoke(params: Json.Obj, ctx: McpRequestContext): ZIO[R, McpMethodError, Json.Obj] =
      for
        decoded <- ZIO.fromEither(decode(params))
        result  <- handler(decoded, ctx)
        json    <- ZIO.fromEither(typedOperation.resultCodec.encode(result))
                     .mapError(McpMethodError.InvalidResult.apply)
        obj     <- ZIO.fromOption(json.asObject)
                     .orElseFail(McpMethodError.InvalidResult("Extension results must encode to a JSON object"))
      yield obj
