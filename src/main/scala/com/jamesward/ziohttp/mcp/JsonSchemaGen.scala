package com.jamesward.ziohttp.mcp

import zio.*
import zio.json.ast.Json
import zio.schema.*

import scala.annotation.tailrec

object JsonSchemaGen:

  def fromSchema[A](schema: Schema[A]): Json.Obj =
    convert(schema)

  private def convert(schema: Schema[?]): Json.Obj =
    schema match
      case Schema.Primitive(standardType, _) =>
        primitiveSchema(standardType)

      case record: Schema.Record[?] =>
        recordSchema(record)

      case Schema.Optional(innerSchema, _) =>
        convert(innerSchema)

      case s @ Schema.Sequence(elementSchema, _, _, _, _) =>
        describedArray(elementSchema, sequenceKind(s.identity), s.annotations)

      case s @ Schema.Set(elementSchema, _) =>
        describedArray(elementSchema, "set", s.annotations)

      case s @ Schema.Map(keySchema, valueSchema, _) =>
        withDescription(
          Json.Obj(Chunk(
            "type"                 -> Json.Str("object"),
            "additionalProperties" -> convert(valueSchema),
            "description"          -> Json.Str(s"a map of ${elementName(keySchema)} to ${elementName(valueSchema)}"),
          )),
          s.annotations,
        )

      case enum0: Schema.Enum[?] =>
        enumSchema(enum0)

      case Schema.Transform(innerSchema, _, _, _, _) =>
        convert(innerSchema)

      case Schema.Lazy(schema0) =>
        convert(schema0())

      case _ =>
        Json.Obj(Chunk.empty)

  private def primitiveSchema(st: StandardType[?]): Json.Obj =
    // Use `eq` for reference equality to avoid strictEquality issues with wildcard types
    if st eq StandardType.StringType then          Json.Obj(Chunk("type" -> Json.Str("string")))
    else if st eq StandardType.BoolType then        Json.Obj(Chunk("type" -> Json.Str("boolean")))
    else if st eq StandardType.IntType then          Json.Obj(Chunk("type" -> Json.Str("integer")))
    else if st eq StandardType.LongType then         Json.Obj(Chunk("type" -> Json.Str("integer")))
    else if st eq StandardType.ShortType then        Json.Obj(Chunk("type" -> Json.Str("integer")))
    else if st eq StandardType.ByteType then         Json.Obj(Chunk("type" -> Json.Str("integer")))
    else if st eq StandardType.FloatType then        Json.Obj(Chunk("type" -> Json.Str("number")))
    else if st eq StandardType.DoubleType then       Json.Obj(Chunk("type" -> Json.Str("number")))
    else if st eq StandardType.BigDecimalType then   Json.Obj(Chunk("type" -> Json.Str("number")))
    else if st eq StandardType.BigIntegerType then   Json.Obj(Chunk("type" -> Json.Str("integer")))
    else if st eq StandardType.UUIDType then         Json.Obj(Chunk("type" -> Json.Str("string"), "format" -> Json.Str("uuid")))
    else                                             Json.Obj(Chunk("type" -> Json.Str("string")))

  private def recordSchema(record: Schema.Record[?]): Json.Obj =
    val properties = record.fields.map: field =>
      val fieldSchema = withDescription(convert(field.schema), field.annotations)
      field.name -> (fieldSchema: Json)

    val required = record.fields.collect:
      case field if !isOptional(field.schema) =>
        Json.Str(field.name)

    val fields = Chunk(
      "type"       -> Json.Str("object"),
      "properties" -> Json.Obj(properties),
    ) ++ (if required.nonEmpty then Chunk("required" -> Json.Arr(required)) else Chunk.empty)

    withDescription(Json.Obj(fields), record.annotations)

  /** Pull a JSON Schema `description` out of zio-schema's
   *  `@zio.schema.annotation.description` annotation. zio-schema's derivation
   *  also synthesizes that annotation from a Scaladoc comment, so doc comments
   *  on record types and fields flow through as descriptions automatically. */
  private def descriptionOf(annotations: Chunk[Any]): Option[String] =
    annotations.collectFirst:
      case d: zio.schema.annotation.description => d.text

  /** Attach a `description` to a schema object. Any existing `description` is
   *  replaced (so a field-level annotation overrides the referenced type's own
   *  description) and a `None` annotation leaves the object untouched. */
  private def withDescription(obj: Json.Obj, annotations: Chunk[Any]): Json.Obj =
    descriptionOf(annotations) match
      case Some(desc) =>
        Json.Obj(obj.fields.filterNot(_._1 == "description") :+ ("description" -> Json.Str(desc)))
      case None => obj

  /** An `array` schema for a collection, auto-described from the collection kind
   *  and element type (e.g. `Set[Foo]` -> "a set of Foo"). An explicit
   *  `@description` annotation on the collection schema overrides the generated
   *  text. */
  private def describedArray(elementSchema: Schema[?], kind: String, annotations: Chunk[Any]): Json.Obj =
    withDescription(
      Json.Obj(Chunk(
        "type"        -> Json.Str("array"),
        "items"       -> convert(elementSchema),
        "description" -> Json.Str(s"a $kind of ${elementName(elementSchema)}"),
      )),
      annotations,
    )

  /** The kind label for a `Schema.Sequence`, taken from its identity tag
   *  ("List", "Vector", "Chunk", ...), lowercased. */
  private def sequenceKind(identity: Any): String =
    identity match
      case s: String => s.toLowerCase
      case _         => "list"

  /** A readable simple type name for a schema's element, used in generated
   *  collection descriptions (e.g. "GroupArtifact", "String"). */
  private def elementName(schema: Schema[?]): String =
    Schema.getTypeId(schema) match
      case TypeId.Nominal(_, _, typeName) => typeName
      case _                              => "item"

  @tailrec
  private def isOptional(schema: Schema[?]): Boolean = schema match
    case _: Schema.Optional[?]               => true
    case Schema.Lazy(s)                      => isOptional(s())
    case Schema.Transform(schema = inner)    => isOptional(inner)
    case _                                   => false

  private def enumSchema(enum0: Schema.Enum[?]): Json.Obj =
    val cases = enum0.cases
    if cases.forall(_.schema.isInstanceOf[Schema.CaseClass0[?]]) then
      val values = cases.map(c => Json.Str(c.id))
      Json.Obj(Chunk("enum" -> Json.Arr(values)))
    else
      val oneOf = cases.map: c =>
        convert(c.schema): Json
      Json.Obj(Chunk("oneOf" -> Json.Arr(oneOf)))
