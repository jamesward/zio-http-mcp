package com.jamesward.ziohttp.mcp

import zio.*
import zio.json.ast.Json

trait McpExtensionSettings[-R]:
  def resolve(ctx: McpRequestContext): URIO[R, Json]

object McpExtensionSettings:
  def static(value: Json): McpExtensionSettings[Any] =
    (_: McpRequestContext) => ZIO.succeed(value)

  def dynamic[R](f: McpRequestContext => URIO[R, Json]): McpExtensionSettings[R] =
    (ctx: McpRequestContext) => f(ctx)

final case class McpServerExtension[-R](
  id: McpExtensionId,
  operations: Chunk[McpBoundOperation[R]],
  settings: McpExtensionSettings[R],
)

object McpServerExtension:
  def apply[R](
    id: McpExtensionId,
    operations: Chunk[McpBoundOperation[R]],
    settings: Json,
  ): McpServerExtension[R] =
    McpServerExtension(id, operations, McpExtensionSettings.static(settings))

  def capability(id: McpExtensionId, settings: Json): McpServerExtension[Any] =
    McpServerExtension(id, Chunk.empty, McpExtensionSettings.static(settings))

enum McpExtensionsError:
  case DuplicateExtensionId(id: McpExtensionId)
  case DuplicateMethod(method: McpMethodName)
  case CoreMethodShadow(method: McpMethodName)
  case OperationExtensionMismatch(
    extensionId: McpExtensionId,
    operationExtensionId: McpExtensionId,
    method: McpMethodName,
  )

object McpExtensionsError:
  given CanEqual[McpExtensionsError, McpExtensionsError] = CanEqual.derived

sealed trait McpExtensions[-R]:
  def values: Chunk[McpServerExtension[R]]
  def operation(method: McpMethodName): Option[McpBoundOperation[R]]

  final def add[R1](extension: McpServerExtension[R1]): Either[McpExtensionsError, McpExtensions[R & R1]] =
    McpExtensions.fromChunk(values :+ extension)

  private[mcp] final def settings(ctx: McpRequestContext): URIO[R, Map[McpExtensionId, Json]] =
    ZIO.foreach(values)(extension => extension.settings.resolve(ctx).map(extension.id -> _)).map(_.toMap)

object McpExtensions:
  private final case class Validated[-R](
    values: Chunk[McpServerExtension[R]],
    methods: Map[McpMethodName, McpBoundOperation[R]],
  ) extends McpExtensions[R]:
    def operation(method: McpMethodName): Option[McpBoundOperation[R]] = methods.get(method)

  private[mcp] def trustedVertical[R](extension: McpServerExtension[R]): McpExtensions[R] =
    Validated(
      Chunk(extension),
      extension.operations.map(operation => operation.operation.method -> operation).toMap,
    )

  val empty: McpExtensions[Any] = Validated(Chunk.empty, Map.empty)

  def apply[R](extensions: McpServerExtension[R]*): Either[McpExtensionsError, McpExtensions[R]] =
    fromChunk(Chunk.fromIterable(extensions))

  def fromChunk[R](extensions: Chunk[McpServerExtension[R]]): Either[McpExtensionsError, McpExtensions[R]] =
    for
      _ <- rejectDuplicateIds(extensions)
      _ <- rejectOperationMismatches(extensions)
      operations = extensions.flatMap(_.operations)
      _ <- rejectCoreShadowing(operations)
      _ <- rejectDuplicateMethods(operations)
    yield Validated(extensions, operations.map(op => op.operation.method -> op).toMap)

  private def rejectDuplicateIds[R](extensions: Chunk[McpServerExtension[R]]): Either[McpExtensionsError, Unit] =
    firstDuplicate(extensions.map(_.id)).toLeft(()).left.map(McpExtensionsError.DuplicateExtensionId.apply)

  private def rejectDuplicateMethods[R](operations: Chunk[McpBoundOperation[R]]): Either[McpExtensionsError, Unit] =
    firstDuplicate(operations.map(_.operation.method)).toLeft(()).left.map(McpExtensionsError.DuplicateMethod.apply)

  private def rejectCoreShadowing[R](operations: Chunk[McpBoundOperation[R]]): Either[McpExtensionsError, Unit] =
    operations.find(op => isCore(op.operation.method)) match
      case Some(operation) => Left(McpExtensionsError.CoreMethodShadow(operation.operation.method))
      case None            => Right(())

  private def rejectOperationMismatches[R](extensions: Chunk[McpServerExtension[R]]): Either[McpExtensionsError, Unit] =
    extensions.flatMap(extension => extension.operations.map(extension -> _))
      .find((extension, operation) => extension.id != operation.operation.extensionId) match
        case Some((extension, operation)) =>
          Left(McpExtensionsError.OperationExtensionMismatch(
            extension.id,
            operation.operation.extensionId,
            operation.operation.method,
          ))
        case None => Right(())

  private val coreMethods = Set(
    "initialize",
    "ping",
    "tools/list",
    "tools/call",
    "resources/list",
    "resources/templates/list",
    "resources/read",
    "resources/subscribe",
    "resources/unsubscribe",
    "prompts/list",
    "prompts/get",
    "logging/setLevel",
    "completion/complete",
    "server/discover",
    "subscriptions/listen",
    "tasks/get",
    "tasks/update",
    "tasks/cancel",
    "sampling/createMessage",
    "elicitation/create",
    "roots/list",
    "notifications/initialized",
    "notifications/cancelled",
    "notifications/progress",
    "notifications/message",
    "notifications/resources/list_changed",
    "notifications/resources/updated",
    "notifications/tools/list_changed",
    "notifications/prompts/list_changed",
    "notifications/roots/list_changed",
    "notifications/subscriptions/acknowledged",
  )

  private def isCore(method: McpMethodName): Boolean =
    coreMethods.contains(method.value)

  private def firstDuplicate[A](values: Chunk[A]): Option[A] =
    values.foldLeft((Set.empty[A], Option.empty[A])):
      case ((seen, duplicate), value) =>
        duplicate match
          case Some(existing) => (seen, Some(existing))
          case None if seen.contains(value) => (seen, Some(value))
          case None => (seen + value, None)
    ._2
