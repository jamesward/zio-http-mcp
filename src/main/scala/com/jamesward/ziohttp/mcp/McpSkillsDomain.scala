package com.jamesward.ziohttp.mcp

import zio.*
import zio.json.ast.Json

import java.net.URI
import java.util.regex.Pattern
import scala.util.Try

enum McpSkillNameError:
  case Empty
  case TooLong(actual: Int, maximum: Int)
  case InvalidCharacters(value: String)

object McpSkillNameError:
  given CanEqual[McpSkillNameError, McpSkillNameError] = CanEqual.derived

opaque type McpSkillName = String

object McpSkillName:
  val MaximumLength: Int = 64
  private val Valid = Pattern.compile("[a-z0-9]+(?:-[a-z0-9]+)*")

  def parse(value: String): Either[McpSkillNameError, McpSkillName] =
    if value.isEmpty then Left(McpSkillNameError.Empty)
    else if value.length > MaximumLength then Left(McpSkillNameError.TooLong(value.length, MaximumLength))
    else if !Valid.matcher(value).matches() then Left(McpSkillNameError.InvalidCharacters(value))
    else Right(value)

  extension (name: McpSkillName) def value: String = name
  given CanEqual[McpSkillName, McpSkillName] = CanEqual.derived

enum McpSkillUriError:
  case InvalidUri(value: String, message: String)
  case MissingScheme(value: String)
  case QueryNotAllowed(value: String)
  case FragmentNotAllowed(value: String)
  case PathTraversal(value: String)
  case NotSkillMarkdown(value: String)
  case MissingSkillName(value: String)
  case InvalidSkillName(value: String, cause: McpSkillNameError)

object McpSkillUriError:
  given CanEqual[McpSkillUriError, McpSkillUriError] = CanEqual.derived

private final case class ParsedMcpSkillUri(value: String, name: McpSkillName, javaUri: URI)

opaque type McpSkillUri = ParsedMcpSkillUri

object McpSkillUri:
  def parse(value: String): Either[McpSkillUriError, McpSkillUri] =
    parseJava(value).flatMap: uri =>
      val segments = pathSegments(uri)
      if Option(uri.getScheme).forall(_.isEmpty) then Left(McpSkillUriError.MissingScheme(value))
      else if Option(uri.getRawQuery).isDefined then Left(McpSkillUriError.QueryNotAllowed(value))
      else if Option(uri.getRawFragment).isDefined then Left(McpSkillUriError.FragmentNotAllowed(value))
      else if segments.exists(segment => segment == "." || segment == "..") then
        Left(McpSkillUriError.PathTraversal(value))
      else if !segments.lastOption.contains("SKILL.md") then Left(McpSkillUriError.NotSkillMarkdown(value))
      else skillName(uri) match
        case None => Left(McpSkillUriError.MissingSkillName(value))
        case Some(rawName) =>
          McpSkillName.parse(rawName)
            .left.map(McpSkillUriError.InvalidSkillName(rawName, _))
            .map(name => ParsedMcpSkillUri(value, name, uri))

  extension (uri: McpSkillUri)
    def value: String = uri.value
    def name: McpSkillName = uri.name

    private[mcp] def contains(resource: McpSkillResourceUri): Boolean =
      val skill = uri.javaUri
      val candidate = resource.javaUri
      val skillRoot = pathSegments(skill).dropRight(1)
      val candidatePath = pathSegments(candidate)
      Option(skill.getScheme).map(_.toLowerCase) == Option(candidate.getScheme).map(_.toLowerCase) &&
        Option(skill.getRawAuthority) == Option(candidate.getRawAuthority) &&
        candidatePath.length > skillRoot.length &&
        candidatePath.startsWith(skillRoot)

  private def parseJava(value: String): Either[McpSkillUriError, URI] =
    Try(URI.create(value)).toEither.left.map(error => McpSkillUriError.InvalidUri(value, error.getMessage))

  private def pathSegments(uri: URI): Chunk[String] =
    Chunk.fromIterable(Option(uri.getPath).toList.flatMap(_.split('/')).filter(_.nonEmpty))

  private def skillName(uri: URI): Option[String] =
    pathSegments(uri).dropRight(1).lastOption.orElse(Option(uri.getRawAuthority).filter(_.nonEmpty))

  given CanEqual[McpSkillUri, McpSkillUri] = CanEqual.derived

enum McpSkillResourceUriError:
  case InvalidUri(value: String, message: String)
  case MissingScheme(value: String)
  case QueryNotAllowed(value: String)
  case FragmentNotAllowed(value: String)
  case PathTraversal(value: String)

object McpSkillResourceUriError:
  given CanEqual[McpSkillResourceUriError, McpSkillResourceUriError] = CanEqual.derived

private final case class ParsedMcpSkillResourceUri(value: String, javaUri: URI)

opaque type McpSkillResourceUri = ParsedMcpSkillResourceUri

object McpSkillResourceUri:
  def parse(value: String): Either[McpSkillResourceUriError, McpSkillResourceUri] =
    Try(URI.create(value)).toEither.left.map(error =>
      McpSkillResourceUriError.InvalidUri(value, error.getMessage)
    ).flatMap: uri =>
      val segments = Chunk.fromIterable(Option(uri.getPath).toList.flatMap(_.split('/')).filter(_.nonEmpty))
      if Option(uri.getScheme).forall(_.isEmpty) then Left(McpSkillResourceUriError.MissingScheme(value))
      else if Option(uri.getRawQuery).isDefined then Left(McpSkillResourceUriError.QueryNotAllowed(value))
      else if Option(uri.getRawFragment).isDefined then Left(McpSkillResourceUriError.FragmentNotAllowed(value))
      else if segments.exists(segment => segment == "." || segment == "..") then
        Left(McpSkillResourceUriError.PathTraversal(value))
      else Right(ParsedMcpSkillResourceUri(value, uri))

  extension (uri: McpSkillResourceUri)
    def value: String = uri.value
    private[mcp] def javaUri: URI = uri.javaUri

  given CanEqual[McpSkillResourceUri, McpSkillResourceUri] = CanEqual.derived

enum McpSkillDigestError:
  case Invalid(value: String)

object McpSkillDigestError:
  given CanEqual[McpSkillDigestError, McpSkillDigestError] = CanEqual.derived

opaque type McpSkillDigest = String

object McpSkillDigest:
  private val PatternSha256 = Pattern.compile("sha256:[0-9a-f]{64}")

  def parse(value: String): Either[McpSkillDigestError, McpSkillDigest] =
    if PatternSha256.matcher(value).matches() then Right(value)
    else Left(McpSkillDigestError.Invalid(value))

  extension (digest: McpSkillDigest) def value: String = digest
  given CanEqual[McpSkillDigest, McpSkillDigest] = CanEqual.derived

enum McpSkillSizeError:
  case Negative(value: Long)

object McpSkillSizeError:
  given CanEqual[McpSkillSizeError, McpSkillSizeError] = CanEqual.derived

opaque type McpSkillSize = Long

object McpSkillSize:
  def parse(value: Long): Either[McpSkillSizeError, McpSkillSize] =
    if value >= 0 then Right(value) else Left(McpSkillSizeError.Negative(value))

  extension (size: McpSkillSize) def bytes: Long = size
  given CanEqual[McpSkillSize, McpSkillSize] = CanEqual.derived

opaque type McpSkillFrontmatter = Json.Obj

object McpSkillFrontmatter:
  def apply(value: Json.Obj): McpSkillFrontmatter = value
  extension (frontmatter: McpSkillFrontmatter) def value: Json.Obj = frontmatter
  given CanEqual[McpSkillFrontmatter, McpSkillFrontmatter] = CanEqual.derived

final case class McpSkillResource(
  uri: McpSkillResourceUri,
  digest: McpSkillDigest,
  size: McpSkillSize,
)

object McpSkillResource:
  given CanEqual[McpSkillResource, McpSkillResource] = CanEqual.derived
