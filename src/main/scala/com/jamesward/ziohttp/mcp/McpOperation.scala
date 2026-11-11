package com.jamesward.ziohttp.mcp

import zio.*
import zio.json.ast.Json
import zio.schema.Schema
import zio.stream.ZStream

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
  case MissingRoutingName(expected: McpRoutingName)
  case Domain(code: Int, message: String, data: Option[Json] = None)

object McpMethodError:
  given CanEqual[McpMethodError, McpMethodError] = CanEqual.derived

/**
 * Whether a Streamable HTTP request for an operation with a routing name must
 * carry the `Mcp-Name` header. Either way a header that is present must match.
 */
enum McpRoutingHeader:
  case Optional, Required

object McpRoutingHeader:
  given CanEqual[McpRoutingHeader, McpRoutingHeader] = CanEqual.derived

final case class McpOperation[Params, Result] private (
  extensionId: McpExtensionId,
  method: McpMethodName,
  protocolSupport: ProtocolSupport,
  clientSupport: ClientSupportPolicy,
  cachePolicy: McpCachePolicy,
  routingName: Params => Option[McpRoutingName],
  private[mcp] val paramsCodec: McpWireCodec[Params],
  private[mcp] val resultCodec: McpWireCodec[Result],
  routingHeader: McpRoutingHeader,
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
    routingHeader: McpRoutingHeader = McpRoutingHeader.Optional,
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
      routingHeader,
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
    routingHeader: McpRoutingHeader = McpRoutingHeader.Optional,
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
      routingHeader,
    )

/** A notification a streaming operation sends before its result. */
final case class McpStreamNotification(method: McpMethodName, params: Json.Obj)

object McpStreamNotification:
  given CanEqual[McpStreamNotification, McpStreamNotification] = CanEqual.derived

/**
 * What a streaming operation answers with: `notifications`, sent as they come
 * (SSE events on Streamable HTTP, lines on stdio), and then `result`, once they
 * end. A client that goes away (or, on stdio, cancels) interrupts the stream.
 */
final case class McpOperationStream[-R, +Result](
  notifications: ZStream[R, Nothing, McpStreamNotification],
  result: Result,
)

/** An operation's answer, as the server sends it. */
private[mcp] enum McpOperationOutput[-R]:
  case Complete(result: Json.Obj)
  case Streaming(notifications: ZStream[R, Nothing, McpStreamNotification], result: Json.Obj)

trait McpBoundOperation[-R]:
  def operation: McpOperation[?, ?]
  private[mcp] def invoke(params: Json.Obj, ctx: McpRequestContext): ZIO[R, McpMethodError, McpOperationOutput[R]]
  private[mcp] def routingName(params: Json.Obj): Either[McpMethodError, Option[McpRoutingName]]

object McpBoundOperation:
  def apply[R, Params, Result](operationValue: McpOperation[Params, Result])(
    handler: (Params, McpRequestContext) => ZIO[R, McpMethodError, Result]
  ): McpBoundOperation[R] =
    Bound(operationValue, (params, ctx) => handler(params, ctx).map(Answer.Complete(_)))

  /**
   * An operation answered with a stream of notifications and then its result,
   * like `subscriptions/listen`. A failure of `handler` is a JSON-RPC error
   * response, sent before any stream opens.
   */
  def streaming[R, Params, Result](operationValue: McpOperation[Params, Result])(
    handler: (Params, McpRequestContext) => ZIO[R, McpMethodError, McpOperationStream[R, Result]]
  ): McpBoundOperation[R] =
    Bound(operationValue, (params, ctx) => handler(params, ctx).map(Answer.Streaming(_)))

  private enum Answer[-R, +Result]:
    case Complete(result: Result)
    case Streaming(stream: McpOperationStream[R, Result])

  private final case class Bound[R, Params, Result](
    typedOperation: McpOperation[Params, Result],
    handler: (Params, McpRequestContext) => ZIO[R, McpMethodError, Answer[R, Result]],
  ) extends McpBoundOperation[R]:
    val operation: McpOperation[?, ?] = typedOperation

    private def decode(params: Json.Obj): Either[McpMethodError, Params] =
      typedOperation.paramsCodec.decode(params).left.map(McpMethodError.InvalidParams.apply)

    def routingName(params: Json.Obj): Either[McpMethodError, Option[McpRoutingName]] =
      decode(params).map(typedOperation.routingName)

    def invoke(params: Json.Obj, ctx: McpRequestContext): ZIO[R, McpMethodError, McpOperationOutput[R]] =
      ZIO.fromEither(decode(params)).flatMap(handler(_, ctx)).flatMap:
        case Answer.Complete(result) => encode(result).map(McpOperationOutput.Complete(_))
        case Answer.Streaming(stream) =>
          encode(stream.result).map(McpOperationOutput.Streaming(stream.notifications, _))

    private def encode(result: Result): IO[McpMethodError, Json.Obj] =
      ZIO.fromEither(typedOperation.resultCodec.encode(result))
        .mapError(McpMethodError.InvalidResult.apply)
        .flatMap: json =>
          ZIO.fromOption(json.asObject)
            .orElseFail(McpMethodError.InvalidResult("Extension results must encode to a JSON object"))
