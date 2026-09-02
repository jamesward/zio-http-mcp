package com.jamesward.ziohttp.mcp

import zio.*
import zio.json.ast.Json

enum McpSkillsSourceError:
  case InvalidParams(message: String)
  case Internal(message: String)

object McpSkillsSourceError:
  given CanEqual[McpSkillsSourceError, McpSkillsSourceError] = CanEqual.derived

trait McpSkillsSource[-R]:
  def list(
    params: McpSkillsListParams,
    ctx: McpRequestContext,
  ): ZIO[R, McpSkillsSourceError, McpSkillsListResult]

  def get(
    uri: McpSkillUri,
    ctx: McpRequestContext,
  ): ZIO[R, McpSkillsSourceError, McpSkillEntry]

trait McpSkillsDirectorySource[-R]:
  def read(
    params: ResourceDirectoryReadParams,
    ctx: McpRequestContext,
  ): ZIO[R, McpSkillsSourceError, ResourcesListResult]

object McpSkills:
  val Id: McpExtensionId = McpExtensionId.fromValid("io.modelcontextprotocol/skills")
  val ListMethod: McpMethodName = McpMethodName.fromValid("skills/list")
  val GetMethod: McpMethodName = McpMethodName.fromValid("skills/get")
  val DirectoryReadMethod: McpMethodName = McpMethodName.fromValid("resources/directory/read")

  val listOperation: McpOperation[McpSkillsListParams, McpSkillsListResult] =
    McpOperation.wire(
      Id,
      ListMethod,
      McpSkillsWireCodecs.listParams,
      McpSkillsWireCodecs.listResult,
      cachePolicy = McpCachePolicy.Default,
    )

  val getOperation: McpOperation[McpSkillsGetParams, McpSkillsGetResult] =
    McpOperation.wire(
      Id,
      GetMethod,
      McpSkillsWireCodecs.getParams,
      McpSkillsWireCodecs.getResult,
      routingName = params => Some(McpRoutingName.fromValid(params.uri.value)),
    )

  val directoryReadOperation: McpOperation[ResourceDirectoryReadParams, ResourcesListResult] =
    McpOperation.wire(
      Id,
      DirectoryReadMethod,
      McpWireCodec.json[ResourceDirectoryReadParams],
      McpWireCodec.json[ResourcesListResult],
    )

  def apply[R](source: McpSkillsSource[R]): McpExtensions[R] =
    McpExtensions.trustedVertical(McpServerExtension(
      Id,
      Chunk(listBound(source), getBound(source)),
      Json.Obj(),
    ))

  def withDirectory[R, R1](
    source: McpSkillsSource[R],
    directory: McpSkillsDirectorySource[R1],
  ): McpExtensions[R & R1] =
    McpExtensions.trustedVertical(McpServerExtension(
      Id,
      Chunk(listBound(source), getBound(source), directoryBound(directory)),
      Json.Obj("directoryRead" -> Json.Bool(true)),
    ))

  private def listBound[R](source: McpSkillsSource[R]): McpBoundOperation[R] =
    McpBoundOperation(listOperation): (params, ctx) =>
      mapSourceError(ListMethod, source.list(params, ctx))

  private def getBound[R](source: McpSkillsSource[R]): McpBoundOperation[R] =
    McpBoundOperation(getOperation): (params, ctx) =>
      mapSourceError(GetMethod, source.get(params.uri, ctx)).map(McpSkillsGetResult.apply)

  private def directoryBound[R](directory: McpSkillsDirectorySource[R]): McpBoundOperation[R] =
    McpBoundOperation(directoryReadOperation): (params, ctx) =>
      mapSourceError(DirectoryReadMethod, directory.read(params, ctx))

  private def mapSourceError[R, A](
    operation: McpMethodName,
    effect: ZIO[R, McpSkillsSourceError, A],
  ): ZIO[R, McpMethodError, A] =
    effect.tapError:
      case McpSkillsSourceError.InvalidParams(message) =>
        ZIO.logWarning(s"${operation.value} rejected parameters: $message")
      case McpSkillsSourceError.Internal(message) =>
        ZIO.logError(s"${operation.value} failed: $message")
    .mapError:
      case McpSkillsSourceError.InvalidParams(message) => McpMethodError.InvalidParams(message)
      case McpSkillsSourceError.Internal(message) =>
        McpMethodError.Domain(ErrorCode.InternalError.code, message)
