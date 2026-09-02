package com.jamesward.ziohttp.mcp

import zio.*

enum McpSkillResources:
  case Static(values: NonEmptyChunk[McpSkillResource])
  case Dynamic

object McpSkillResources:
  given CanEqual[McpSkillResources, McpSkillResources] = CanEqual.derived

enum McpSkillEntryError:
  case MissingFrontmatterName
  case InvalidFrontmatterName(value: String, cause: McpSkillNameError)
  case MissingFrontmatterDescription
  case FrontmatterDescriptionTooLong(actual: Int, maximum: Int)
  case InvalidFrontmatterLicense
  case InvalidFrontmatterCompatibility
  case FrontmatterCompatibilityTooLong(actual: Int, maximum: Int)
  case InvalidFrontmatterMetadata
  case InvalidFrontmatterAllowedTools
  case UriNameMismatch(uriName: String, frontmatterName: String)
  case TooManyResources(actual: Int, maximum: Int)
  case TotalSizeTooLarge(actual: BigInt, maximum: Long)
  case DuplicateResourceUri(uri: McpSkillResourceUri)
  case MissingEntryResource(uri: McpSkillUri)
  case ResourceOutsideSkill(uri: McpSkillResourceUri, skill: McpSkillUri)

object McpSkillEntryError:
  given CanEqual[McpSkillEntryError, McpSkillEntryError] = CanEqual.derived

final case class McpSkillEntry private (
  uri: McpSkillUri,
  frontmatter: McpSkillFrontmatter,
  resources: McpSkillResources,
)

object McpSkillEntry:
  val MaximumResources: Int = 512
  val MaximumTotalBytes: Long = 16777216L

  def dynamic(
    uri: McpSkillUri,
    frontmatter: McpSkillFrontmatter,
  ): Either[McpSkillEntryError, McpSkillEntry] =
    validateFrontmatter(uri, frontmatter).map(_ => McpSkillEntry(uri, frontmatter, McpSkillResources.Dynamic))

  def static(
    uri: McpSkillUri,
    frontmatter: McpSkillFrontmatter,
    resources: NonEmptyChunk[McpSkillResource],
  ): Either[McpSkillEntryError, McpSkillEntry] =
    for
      _ <- validateFrontmatter(uri, frontmatter)
      _ <- Either.cond(
             resources.size <= MaximumResources,
             (),
             McpSkillEntryError.TooManyResources(resources.size, MaximumResources),
           )
      total = resources.foldLeft(BigInt(0))((sum, resource) => sum + BigInt(resource.size.bytes))
      _ <- Either.cond(
             total <= BigInt(MaximumTotalBytes),
             (),
             McpSkillEntryError.TotalSizeTooLarge(total, MaximumTotalBytes),
           )
      _ <- firstDuplicate(resources.map(_.uri)).toLeft(()).left.map(McpSkillEntryError.DuplicateResourceUri.apply)
      _ <- Either.cond(
             resources.count(_.uri.value == uri.value) == 1,
             (),
             McpSkillEntryError.MissingEntryResource(uri),
           )
      _ <- resources.find(resource => !uri.contains(resource.uri)) match
             case Some(resource) => Left(McpSkillEntryError.ResourceOutsideSkill(resource.uri, uri))
             case None           => Right(())
    yield McpSkillEntry(uri, frontmatter, McpSkillResources.Static(resources))

  private val MaximumDescriptionLength = 1024
  private val MaximumCompatibilityLength = 500

  private def validateFrontmatter(
    uri: McpSkillUri,
    frontmatter: McpSkillFrontmatter,
  ): Either[McpSkillEntryError, Unit] =
    val json = frontmatter.value
    for
      rawName <- json.get("name").flatMap(_.asString)
                   .toRight(McpSkillEntryError.MissingFrontmatterName)
      name <- McpSkillName.parse(rawName)
                .left.map(McpSkillEntryError.InvalidFrontmatterName(rawName, _))
      description <- json.get("description").flatMap(_.asString).filter(_.trim.nonEmpty)
                       .toRight(McpSkillEntryError.MissingFrontmatterDescription)
      _ <- Either.cond(
             description.length <= MaximumDescriptionLength,
             (),
             McpSkillEntryError.FrontmatterDescriptionTooLong(
               description.length,
               MaximumDescriptionLength,
             ),
           )
      _ <- validateOptionalString(json, "license", allowEmpty = true)
             .left.map(_ => McpSkillEntryError.InvalidFrontmatterLicense)
      compatibility <- validateOptionalString(json, "compatibility", allowEmpty = false)
                         .left.map(_ => McpSkillEntryError.InvalidFrontmatterCompatibility)
      _ <- compatibility match
             case Some(value) if value.length > MaximumCompatibilityLength =>
               Left(McpSkillEntryError.FrontmatterCompatibilityTooLong(
                 value.length,
                 MaximumCompatibilityLength,
               ))
             case _ => Right(())
      _ <- json.get("metadata") match
             case None => Right(())
             case Some(obj: zio.json.ast.Json.Obj) if obj.fields.forall(_._2.asString.isDefined) => Right(())
             case Some(_) => Left(McpSkillEntryError.InvalidFrontmatterMetadata)
      _ <- validateOptionalString(json, "allowed-tools", allowEmpty = true)
             .left.map(_ => McpSkillEntryError.InvalidFrontmatterAllowedTools)
      _ <- Either.cond(
             name == uri.name,
             (),
             McpSkillEntryError.UriNameMismatch(uri.name.value, name.value),
           )
    yield ()

  private def validateOptionalString(
    json: zio.json.ast.Json.Obj,
    field: String,
    allowEmpty: Boolean,
  ): Either[Unit, Option[String]] =
    json.get(field) match
      case None => Right(None)
      case Some(value) =>
        value.asString.filter(string => allowEmpty || string.trim.nonEmpty)
          .map(Some(_)).toRight(())

  private def firstDuplicate(values: NonEmptyChunk[McpSkillResourceUri]): Option[McpSkillResourceUri] =
    values.foldLeft((Set.empty[McpSkillResourceUri], Option.empty[McpSkillResourceUri])):
      case ((seen, duplicate), value) =>
        duplicate match
          case some @ Some(_)              => (seen, some)
          case None if seen.contains(value) => (seen, Some(value))
          case None                         => (seen + value, None)
    ._2

  given CanEqual[McpSkillEntry, McpSkillEntry] = CanEqual.derived

final case class McpSkillsListParams(cursor: Option[String] = None)
final case class McpSkillsListResult(skills: Chunk[McpSkillEntry], nextCursor: Option[String] = None)
final case class McpSkillsGetParams(uri: McpSkillUri)
final case class McpSkillsGetResult(skill: McpSkillEntry)

object McpSkillsListParams:
  given CanEqual[McpSkillsListParams, McpSkillsListParams] = CanEqual.derived
object McpSkillsListResult:
  given CanEqual[McpSkillsListResult, McpSkillsListResult] = CanEqual.derived
object McpSkillsGetParams:
  given CanEqual[McpSkillsGetParams, McpSkillsGetParams] = CanEqual.derived
object McpSkillsGetResult:
  given CanEqual[McpSkillsGetResult, McpSkillsGetResult] = CanEqual.derived
