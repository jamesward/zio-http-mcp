package com.jamesward.ziohttp.mcp.client

import com.jamesward.ziohttp.mcp.*
import zio.*
import zio.http.Client
import zio.json.ast.Json

enum McpSkillsClientAdapterError:
  case ExtensionNotAdvertised
  case InvalidServerSettings(message: String)

object McpSkillsClientAdapterError:
  given CanEqual[McpSkillsClientAdapterError, McpSkillsClientAdapterError] = CanEqual.derived

final case class McpSkillsServerSettings(directoryRead: Boolean)

object McpSkillsServerSettings:
  given CanEqual[McpSkillsServerSettings, McpSkillsServerSettings] = CanEqual.derived

  def parse(json: Json): Either[McpSkillsClientAdapterError, McpSkillsServerSettings] =
    json.asObject.toRight(McpSkillsClientAdapterError.InvalidServerSettings(
      "Skills extension settings must be a JSON object"
    )).flatMap: obj =>
      obj.get("directoryRead") match
        case None                    => Right(McpSkillsServerSettings(directoryRead = false))
        case Some(Json.Bool(value))  => Right(McpSkillsServerSettings(value))
        case Some(_)                 => Left(McpSkillsClientAdapterError.InvalidServerSettings(
                                         "Skills 'directoryRead' setting must be boolean"
                                       ))

final case class McpSkillsClient private (
  underlying: McpExtensionClient,
  settings: McpSkillsServerSettings,
):
  def list(cursor: Option[String] = None): IO[McpClientError, McpSkillsListResult] =
    underlying.request(McpSkills.listOperation, McpSkillsListParams(cursor))

  def get(uri: McpSkillUri): IO[McpClientError, McpSkillEntry] =
    underlying.request(McpSkills.getOperation, McpSkillsGetParams(uri)).map(_.skill)

  def readSkill(uri: McpSkillUri): IO[McpClientError, Chunk[ResourceContents]] =
    underlying.readResource(uri.value)

  def readResource(uri: McpSkillResourceUri): IO[McpClientError, Chunk[ResourceContents]] =
    underlying.readResource(uri.value)

  def readDirectory(
    uri: String,
    cursor: Option[String] = None,
  ): IO[McpClientError, ResourcesListResult] =
    if settings.directoryRead then
      underlying.request(
        McpSkills.directoryReadOperation,
        ResourceDirectoryReadParams(uri, cursor),
      )
    else ZIO.fail(McpClientError.Protocol(
      "Server did not advertise Skills directoryRead support"
    ))

object McpSkillsClient:
  def from(client: McpExtensionClient): Either[McpSkillsClientAdapterError, McpSkillsClient] =
    for
      extensions <- client.serverCapabilities.extensions
                      .toRight(McpSkillsClientAdapterError.ExtensionNotAdvertised)
      settingsJson <- extensions.get(McpSkills.Id.value)
                        .toRight(McpSkillsClientAdapterError.ExtensionNotAdvertised)
      settings <- McpSkillsServerSettings.parse(settingsJson)
    yield McpSkillsClient(client, settings)

  def connect(config: McpClientConfig): ZIO[Client & Scope, McpClientError, McpSkillsClient] =
    for
      client <- McpClient.connect(config, McpClientExtensions.empty)
      skills <- ZIO.fromEither(from(client)).mapError:
                  case McpSkillsClientAdapterError.ExtensionNotAdvertised =>
                    McpClientError.Protocol("Server does not advertise io.modelcontextprotocol/skills")
                  case McpSkillsClientAdapterError.InvalidServerSettings(message) =>
                    McpClientError.Protocol(message)
    yield skills
