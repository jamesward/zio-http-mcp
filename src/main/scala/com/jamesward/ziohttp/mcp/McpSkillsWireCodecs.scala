package com.jamesward.ziohttp.mcp

import zio.*
import zio.json.ast.Json

import scala.util.Try

private[mcp] object McpSkillsWireCodecs:
  val listParams: McpWireCodec[McpSkillsListParams] = McpWireCodec(
    decodeObject(_).flatMap: obj =>
      optionalString(obj, "cursor").map(McpSkillsListParams.apply),
    params => Right(Json.Obj(params.cursor.map(value => "cursor" -> (Json.Str(value): Json)).toList*)),
  )

  val listResult: McpWireCodec[McpSkillsListResult] = McpWireCodec(
    decodeObject(_).flatMap: obj =>
      for
        skillsJson <- obj.get("skills").flatMap(_.asArray).toRight("Missing or invalid 'skills' array")
        skills     <- traverse(skillsJson)(decodeEntry)
        nextCursor <- optionalString(obj, "nextCursor")
      yield McpSkillsListResult(skills, nextCursor),
    result => Right(Json.Obj(Chunk(
      Some("skills" -> (Json.Arr(result.skills.map(encodeEntry)): Json)),
      result.nextCursor.map(value => "nextCursor" -> (Json.Str(value): Json)),
    ).flatten)),
  )

  val getParams: McpWireCodec[McpSkillsGetParams] = McpWireCodec(
    decodeObject(_).flatMap: obj =>
      requiredString(obj, "uri").flatMap(value =>
        McpSkillUri.parse(value).left.map(_.toString)
      ).map(McpSkillsGetParams.apply),
    params => Right(Json.Obj("uri" -> Json.Str(params.uri.value))),
  )

  val getResult: McpWireCodec[McpSkillsGetResult] = McpWireCodec(
    decodeObject(_).flatMap: obj =>
      obj.get("skill").toRight("Missing 'skill'").flatMap(decodeEntry).map(McpSkillsGetResult.apply),
    result => Right(Json.Obj("skill" -> encodeEntry(result.skill))),
  )

  private def decodeEntry(json: Json): Either[String, McpSkillEntry] =
    for
      obj         <- decodeObject(json)
      uriString   <- requiredString(obj, "uri")
      uri         <- McpSkillUri.parse(uriString).left.map(_.toString)
      frontmatter <- obj.get("frontmatter").flatMap(_.asObject)
                       .toRight("Missing or invalid 'frontmatter' object")
      resources   <- obj.get("resources").toRight("Missing 'resources'")
      entry       <- resources match
                       case Json.Str("dynamic") =>
                         McpSkillEntry.dynamic(uri, McpSkillFrontmatter(frontmatter)).left.map(_.toString)
                       case array: Json.Arr =>
                         for
                           values   <- traverse(array.elements)(decodeResource)
                           nonEmpty <- NonEmptyChunk.fromChunk(values).toRight("Static resources must be nonempty")
                           entry    <- McpSkillEntry.static(uri, McpSkillFrontmatter(frontmatter), nonEmpty)
                                         .left.map(_.toString)
                         yield entry
                       case _ => Left("'resources' must be a nonempty array or the literal string 'dynamic'")
    yield entry

  private def encodeEntry(entry: McpSkillEntry): Json =
    val resources: Json = entry.resources match
      case McpSkillResources.Dynamic => Json.Str("dynamic")
      case McpSkillResources.Static(values) => Json.Arr(values.map(encodeResource))
    Json.Obj(
      "uri" -> Json.Str(entry.uri.value),
      "frontmatter" -> entry.frontmatter.value,
      "resources" -> resources,
    )

  private def decodeResource(json: Json): Either[String, McpSkillResource] =
    for
      obj       <- decodeObject(json)
      uriString <- requiredString(obj, "uri")
      uri       <- McpSkillResourceUri.parse(uriString).left.map(_.toString)
      digestRaw <- requiredString(obj, "digest")
      digest    <- McpSkillDigest.parse(digestRaw).left.map(_.toString)
      sizeJson  <- obj.get("size").flatMap(_.asNumber).toRight("Missing or invalid 'size'")
      sizeLong  <- Try(sizeJson.value.longValueExact()).toEither.left.map(_ => "'size' must be an integer")
      size      <- McpSkillSize.parse(sizeLong).left.map(_.toString)
    yield McpSkillResource(uri, digest, size)

  private def encodeResource(resource: McpSkillResource): Json =
    Json.Obj(
      "uri" -> Json.Str(resource.uri.value),
      "digest" -> Json.Str(resource.digest.value),
      "size" -> Json.Num(resource.size.bytes),
    )

  private def decodeObject(json: Json): Either[String, Json.Obj] =
    json.asObject.toRight("Expected a JSON object")

  private def requiredString(obj: Json.Obj, field: String): Either[String, String] =
    obj.get(field).flatMap(_.asString).toRight(s"Missing or invalid '$field'")

  private def optionalString(obj: Json.Obj, field: String): Either[String, Option[String]] =
    obj.get(field) match
      case None                 => Right(None)
      case Some(Json.Str(value)) => Right(Some(value))
      case Some(_)              => Left(s"Invalid '$field': expected a string")

  private def traverse[A, B](values: Chunk[A])(f: A => Either[String, B]): Either[String, Chunk[B]] =
    values.foldLeft[Either[String, Chunk[B]]](Right(Chunk.empty)):
      case (Right(acc), value) => f(value).map(acc :+ _)
      case (left @ Left(_), _) => left
