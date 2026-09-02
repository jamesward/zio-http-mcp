package com.jamesward.ziohttp.mcp

import zio.*
import zio.json.*
import zio.json.ast.Json

import java.net.URI
import scala.util.Try

enum McpUiUriError:
  case InvalidUri(value: String, message: String)
  case WrongScheme(value: String)
  case EmptyLocation(value: String)

object McpUiUriError:
  given CanEqual[McpUiUriError, McpUiUriError] = CanEqual.derived

opaque type McpUiUri = String

object McpUiUri:
  def parse(value: String): Either[McpUiUriError, McpUiUri] =
    Try(URI.create(value)).toEither.left.map(error => McpUiUriError.InvalidUri(value, error.getMessage)).flatMap: uri =>
      val hasAuthority = Option(uri.getRawAuthority).exists(_.nonEmpty)
      val hasPath = Option(uri.getRawPath).exists(_.split('/').exists(_.nonEmpty))
      if !value.startsWith("ui://") || !Option(uri.getScheme).contains("ui") then
        Left(McpUiUriError.WrongScheme(value))
      else if !hasAuthority && !hasPath then Left(McpUiUriError.EmptyLocation(value))
      else Right(value)

  extension (uri: McpUiUri) def value: String = uri

  given CanEqual[McpUiUri, McpUiUri] = CanEqual.derived
  given JsonEncoder[McpUiUri] = JsonEncoder.string
  given JsonDecoder[McpUiUri] = JsonDecoder.string.mapOrFail(value => parse(value).left.map(_.toString))

enum McpAppsMimeTypeError:
  case Empty

object McpAppsMimeTypeError:
  given CanEqual[McpAppsMimeTypeError, McpAppsMimeTypeError] = CanEqual.derived

opaque type McpAppsMimeType = String

object McpAppsMimeType:
  val Html: McpAppsMimeType = "text/html;profile=mcp-app"

  def parse(value: String): Either[McpAppsMimeTypeError, McpAppsMimeType] =
    if value.nonEmpty then Right(value) else Left(McpAppsMimeTypeError.Empty)

  extension (mimeType: McpAppsMimeType) def value: String = mimeType

  given CanEqual[McpAppsMimeType, McpAppsMimeType] = CanEqual.derived
  given JsonEncoder[McpAppsMimeType] = JsonEncoder.string
  given JsonDecoder[McpAppsMimeType] = JsonDecoder.string.mapOrFail(value => parse(value).left.map(_.toString))

enum McpAppsVisibility(val wire: String):
  case Model extends McpAppsVisibility("model")
  case App extends McpAppsVisibility("app")

object McpAppsVisibility:
  given CanEqual[McpAppsVisibility, McpAppsVisibility] = CanEqual.derived
  given JsonEncoder[McpAppsVisibility] = JsonEncoder.string.contramap(_.wire)
  given JsonDecoder[McpAppsVisibility] = JsonDecoder.string.mapOrFail:
    case "model" => Right(McpAppsVisibility.Model)
    case "app"   => Right(McpAppsVisibility.App)
    case other   => Left(s"Unknown MCP Apps visibility: $other")

enum McpAppsPermission(val wire: String):
  case Camera extends McpAppsPermission("camera")
  case Microphone extends McpAppsPermission("microphone")
  case Geolocation extends McpAppsPermission("geolocation")
  case ClipboardWrite extends McpAppsPermission("clipboardWrite")

object McpAppsPermission:
  given CanEqual[McpAppsPermission, McpAppsPermission] = CanEqual.derived

  private val all = Chunk(Camera, Microphone, Geolocation, ClipboardWrite)

  given JsonEncoder[Set[McpAppsPermission]] = JsonEncoder[Json.Obj].contramap: permissions =>
    Json.Obj(all.filter(permissions.contains).map(permission => permission.wire -> Json.Obj()))

  given JsonDecoder[Set[McpAppsPermission]] = JsonDecoder[Json.Obj].map: obj =>
    all.filter(permission => obj.get(permission.wire).flatMap(_.asObject).isDefined).toSet

final case class McpAppsCsp(
  connectDomains: Chunk[String] = Chunk.empty,
  resourceDomains: Chunk[String] = Chunk.empty,
  frameDomains: Chunk[String] = Chunk.empty,
  baseUriDomains: Chunk[String] = Chunk.empty,
)

object McpAppsCsp:
  given CanEqual[McpAppsCsp, McpAppsCsp] = CanEqual.derived
  given JsonCodec[McpAppsCsp] = DeriveJsonCodec.gen[McpAppsCsp]

final case class McpAppsToolMeta(
  resourceUri: McpUiUri,
  visibility: NonEmptyChunk[McpAppsVisibility] = NonEmptyChunk(McpAppsVisibility.Model, McpAppsVisibility.App),
)

object McpAppsToolMeta:
  given CanEqual[McpAppsToolMeta, McpAppsToolMeta] = CanEqual.derived
  given JsonCodec[McpAppsToolMeta] = DeriveJsonCodec.gen[McpAppsToolMeta]

final case class McpAppsResourceMeta(
  csp: Option[McpAppsCsp] = None,
  permissions: Set[McpAppsPermission] = Set.empty,
  domain: Option[String] = None,
  prefersBorder: Option[Boolean] = None,
)

object McpAppsResourceMeta:
  given CanEqual[McpAppsResourceMeta, McpAppsResourceMeta] = CanEqual.derived
  given JsonCodec[McpAppsResourceMeta] = DeriveJsonCodec.gen[McpAppsResourceMeta]

final case class McpAppsClientSettings(mimeTypes: NonEmptyChunk[McpAppsMimeType])

object McpAppsClientSettings:
  val Html: McpAppsClientSettings = McpAppsClientSettings(NonEmptyChunk(McpAppsMimeType.Html))

  given CanEqual[McpAppsClientSettings, McpAppsClientSettings] = CanEqual.derived
  given JsonCodec[McpAppsClientSettings] = DeriveJsonCodec.gen[McpAppsClientSettings]

enum McpAppsFallbackTextError:
  case Empty

object McpAppsFallbackTextError:
  given CanEqual[McpAppsFallbackTextError, McpAppsFallbackTextError] = CanEqual.derived

opaque type McpAppsFallbackText = String

object McpAppsFallbackText:
  def parse(value: String): Either[McpAppsFallbackTextError, McpAppsFallbackText] =
    if value.trim.nonEmpty then Right(value) else Left(McpAppsFallbackTextError.Empty)

  extension (text: McpAppsFallbackText) def value: String = text
  given CanEqual[McpAppsFallbackText, McpAppsFallbackText] = CanEqual.derived
