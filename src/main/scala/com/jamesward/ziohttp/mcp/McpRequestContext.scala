package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.auth.Principal
import zio.*
import zio.json.ast.Json

trait McpRequestContext:
  def protocolVersion: ProtocolVersion = ProtocolVersion.default
  def clientInfo: Option[Implementation] = None
  def extensionCapabilities: Map[McpExtensionId, Json] = Map.empty
  def extensionCapabilityErrors: Chunk[McpExtensionCapabilityParseError] = Chunk.empty
  def principal: Option[Principal] = None
  def pathParams: Map[String, String] = Map.empty

  final def requireClientExtension(id: McpExtensionId): IO[McpMethodError, Json] =
    extensionCapabilities.get(id) match
      case Some(settings) => ZIO.succeed(settings)
      case None           => ZIO.fail(McpMethodError.MissingRequiredClientExtension(id))

object McpRequestContext:
  def apply(
    version: ProtocolVersion,
    info: Option[Implementation] = None,
    capabilities: Map[McpExtensionId, Json] = Map.empty,
    callerPrincipal: Option[Principal] = None,
    callerPathParams: Map[String, String] = Map.empty,
    capabilityErrors: Chunk[McpExtensionCapabilityParseError] = Chunk.empty,
  ): McpRequestContext =
    Context(version, info, capabilities, capabilityErrors, callerPrincipal, callerPathParams)

  private final case class Context(
    override val protocolVersion: ProtocolVersion,
    override val clientInfo: Option[Implementation],
    override val extensionCapabilities: Map[McpExtensionId, Json],
    override val extensionCapabilityErrors: Chunk[McpExtensionCapabilityParseError],
    override val principal: Option[Principal],
    override val pathParams: Map[String, String],
  ) extends McpRequestContext

final case class McpExtensionCapabilityParseError(
  rawId: String,
  settings: Json,
  cause: McpExtensionIdError,
)

object McpExtensionCapabilityParseError:
  given CanEqual[McpExtensionCapabilityParseError, McpExtensionCapabilityParseError] = CanEqual.derived

final case class McpExtensionCapabilitiesParseResult(
  valid: Map[McpExtensionId, Json],
  invalid: Chunk[McpExtensionCapabilityParseError],
)

object McpExtensionCapabilitiesParseResult:
  given CanEqual[McpExtensionCapabilitiesParseResult, McpExtensionCapabilitiesParseResult] = CanEqual.derived

private[mcp] final case class McpSessionClient(
  protocolVersion: ProtocolVersion,
  clientInfo: Option[Implementation],
  extensionCapabilities: Map[McpExtensionId, Json],
  extensionCapabilityErrors: Chunk[McpExtensionCapabilityParseError],
)

object McpSessionClient:
  given CanEqual[McpSessionClient, McpSessionClient] = CanEqual.derived

private[mcp] object McpExtensionCapabilities:
  def parse(capabilities: Json.Obj): McpExtensionCapabilitiesParseResult =
    val extensionFields = capabilities.get("extensions") match
      case None              => Chunk.empty
      case Some(obj: Json.Obj) => obj.fields
      case Some(other)       => Chunk("extensions" -> other)
    val parsed = extensionFields.map: (rawId, settings) =>
      McpExtensionId.parse(rawId) match
        case Right(id) => Right(id -> settings)
        case Left(error) => Left(McpExtensionCapabilityParseError(rawId, settings, error))
    McpExtensionCapabilitiesParseResult(
      parsed.collect { case Right(entry) => entry }.toMap,
      parsed.collect { case Left(error) => error },
    )

  def toClientCapabilities(capabilities: Map[McpExtensionId, Json]): Json.Obj =
    if capabilities.isEmpty then Json.Obj()
    else
      val extensions = Json.Obj(Chunk.fromIterable(
        capabilities.toSeq.map((id, settings) => id.value -> settings)
      ))
      Json.Obj("extensions" -> extensions)
