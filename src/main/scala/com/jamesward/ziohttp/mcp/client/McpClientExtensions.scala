package com.jamesward.ziohttp.mcp.client

import com.jamesward.ziohttp.mcp.*
import zio.*
import zio.json.ast.Json

final case class McpClientExtension(id: McpExtensionId, settings: Json)

object McpClientExtension:
  given CanEqual[McpClientExtension, McpClientExtension] = CanEqual.derived

enum McpClientExtensionsError:
  case DuplicateExtensionId(id: McpExtensionId)

object McpClientExtensionsError:
  given CanEqual[McpClientExtensionsError, McpClientExtensionsError] = CanEqual.derived

sealed trait McpClientExtensions:
  def values: Chunk[McpClientExtension]

  private[mcp] final def capabilities: Map[McpExtensionId, Json] =
    values.map(extension => extension.id -> extension.settings).toMap

object McpClientExtensions:
  private final case class Validated(values: Chunk[McpClientExtension]) extends McpClientExtensions

  val empty: McpClientExtensions = Validated(Chunk.empty)

  def apply(extensions: McpClientExtension*): Either[McpClientExtensionsError, McpClientExtensions] =
    val values = Chunk.fromIterable(extensions)
    values.foldLeft((Set.empty[McpExtensionId], Option.empty[McpExtensionId])):
      case ((seen, duplicate), extension) =>
        duplicate match
          case Some(id) => (seen, Some(id))
          case None if seen.contains(extension.id) => (seen, Some(extension.id))
          case None => (seen + extension.id, None)
    match
      case (_, Some(id)) => Left(McpClientExtensionsError.DuplicateExtensionId(id))
      case _             => Right(Validated(values))
