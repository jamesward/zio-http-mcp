package com.jamesward.ziohttp.mcp

import zio.*
import zio.json.*
import zio.json.ast.Json

// --- Role ---

enum Role:
  case User, Assistant

object Role:
  given CanEqual[Role, Role] = CanEqual.derived

  given JsonEncoder[Role] = JsonEncoder.string.contramap:
    case Role.User      => "user"
    case Role.Assistant => "assistant"

  given JsonDecoder[Role] = JsonDecoder.string.mapOrFail:
    case "user"      => Right(Role.User)
    case "assistant" => Right(Role.Assistant)
    case other       => Left(s"Unknown role: $other")

// --- ContentAnnotations ---

case class ContentAnnotations(
  audience: Option[NonEmptyChunk[Role]] = None,
  priority: Option[Double] = None,
)

object ContentAnnotations:
  given CanEqual[ContentAnnotations, ContentAnnotations] = CanEqual.derived
  given JsonCodec[ContentAnnotations] = DeriveJsonCodec.gen[ContentAnnotations]

// --- ResourceContents (for embedded resources) ---

case class ResourceContents(
  uri: String,
  mimeType: Option[String] = None,
  text: Option[String] = None,
  blob: Option[String] = None,
)

object ResourceContents:
  given CanEqual[ResourceContents, ResourceContents] = CanEqual.derived
  given JsonCodec[ResourceContents] = DeriveJsonCodec.gen[ResourceContents]

// --- ToolContent ADT ---

enum ToolContent:
  case Text(text: String, annotations: Option[ContentAnnotations] = None)
  case Image(data: String, mimeType: String, annotations: Option[ContentAnnotations] = None)
  case Audio(data: String, mimeType: String, annotations: Option[ContentAnnotations] = None)
  case EmbeddedResource(resource: ResourceContents, annotations: Option[ContentAnnotations] = None)

object ToolContent:
  def text(s: String): ToolContent = ToolContent.Text(s)
  def image(data: String, mimeType: String): ToolContent = ToolContent.Image(data, mimeType)
  def audio(data: String, mimeType: String): ToolContent = ToolContent.Audio(data, mimeType)
  def embeddedResource(resource: ResourceContents): ToolContent = ToolContent.EmbeddedResource(resource)

  given CanEqual[ToolContent, ToolContent] = CanEqual.derived

  private def roleAsJson(r: Role): Json = r match
    case Role.User      => Json.Str("user")
    case Role.Assistant => Json.Str("assistant")

  private def annFields(ann: Option[ContentAnnotations]): Chunk[(String, Json)] =
    ann.fold(Chunk.empty): a =>
      val audFields = a.audience.fold(Chunk.empty[(String, Json)])(aud => Chunk("audience" -> Json.Arr(aud.map(roleAsJson))))
      val priFields = a.priority.fold(Chunk.empty[(String, Json)])(p => Chunk("priority" -> Json.Num(p)))
      Chunk("annotations" -> Json.Obj(audFields ++ priFields))

  private def resourceContentsAsJson(r: ResourceContents): Json.Obj =
    val base = Chunk("uri" -> Json.Str(r.uri))
    val mime = r.mimeType.fold(Chunk.empty[(String, Json)])(m => Chunk("mimeType" -> Json.Str(m)))
    val text = r.text.fold(Chunk.empty[(String, Json)])(t => Chunk("text" -> Json.Str(t)))
    val blob = r.blob.fold(Chunk.empty[(String, Json)])(b => Chunk("blob" -> Json.Str(b)))
    Json.Obj(base ++ mime ++ text ++ blob)

  given JsonEncoder[ToolContent] = JsonEncoder[Json.Obj].contramap:
    case ToolContent.Text(text, ann) =>
      Json.Obj(Chunk(
        "type" -> Json.Str("text"),
        "text" -> Json.Str(text),
      ) ++ annFields(ann))
    case ToolContent.Image(data, mimeType, ann) =>
      Json.Obj(Chunk(
        "type" -> Json.Str("image"),
        "data" -> Json.Str(data),
        "mimeType" -> Json.Str(mimeType),
      ) ++ annFields(ann))
    case ToolContent.Audio(data, mimeType, ann) =>
      Json.Obj(Chunk(
        "type" -> Json.Str("audio"),
        "data" -> Json.Str(data),
        "mimeType" -> Json.Str(mimeType),
      ) ++ annFields(ann))
    case ToolContent.EmbeddedResource(resource, ann) =>
      Json.Obj(Chunk(
        "type" -> Json.Str("resource"),
        "resource" -> resourceContentsAsJson(resource),
      ) ++ annFields(ann))

  given JsonDecoder[ToolContent] = JsonDecoder[Json.Obj].mapOrFail: obj =>
    obj.get("type").flatMap(_.asString) match
      case Some("text") =>
        obj.get("text").flatMap(_.asString) match
          case Some(text) =>
            val ann = obj.get("annotations").flatMap(_.as[ContentAnnotations].toOption)
            Right(ToolContent.Text(text, ann))
          case None => Left("Missing 'text' field")
      case Some("image") =>
        for
          data     <- obj.get("data").flatMap(_.asString).toRight("Missing 'data' field")
          mimeType <- obj.get("mimeType").flatMap(_.asString).toRight("Missing 'mimeType' field")
        yield
          val ann = obj.get("annotations").flatMap(_.as[ContentAnnotations].toOption)
          ToolContent.Image(data, mimeType, ann)
      case Some("audio") =>
        for
          data     <- obj.get("data").flatMap(_.asString).toRight("Missing 'data' field")
          mimeType <- obj.get("mimeType").flatMap(_.asString).toRight("Missing 'mimeType' field")
        yield
          val ann = obj.get("annotations").flatMap(_.as[ContentAnnotations].toOption)
          ToolContent.Audio(data, mimeType, ann)
      case Some("resource") =>
        obj.get("resource").flatMap(_.as[ResourceContents].toOption) match
          case Some(resource) =>
            val ann = obj.get("annotations").flatMap(_.as[ContentAnnotations].toOption)
            Right(ToolContent.EmbeddedResource(resource, ann))
          case None => Left("Missing or invalid 'resource' field")
      case Some(other) => Left(s"Unknown content type: $other")
      case None        => Left("Missing 'type' field")
