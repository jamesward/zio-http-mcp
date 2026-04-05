package com.jamesward.ziohttp.mcp

import zio.*
import zio.json.ast.Json
import zio.schema.*

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

      case Schema.Sequence(elementSchema, _, _, _, _) =>
        Json.Obj(Chunk(
          "type"  -> Json.Str("array"),
          "items" -> convert(elementSchema),
        ))

      case Schema.Set(elementSchema, _) =>
        Json.Obj(Chunk(
          "type"  -> Json.Str("array"),
          "items" -> convert(elementSchema),
        ))

      case Schema.Map(keySchema, valueSchema, _) =>
        Json.Obj(Chunk(
          "type"                 -> Json.Str("object"),
          "additionalProperties" -> convert(valueSchema),
        ))

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
      val fieldSchema = convert(field.schema)
      field.name -> (fieldSchema: Json)

    val required = record.fields.collect:
      case field if !field.schema.isInstanceOf[Schema.Optional[?]] =>
        Json.Str(field.name)

    val fields = Chunk(
      "type"       -> Json.Str("object"),
      "properties" -> Json.Obj(properties),
    ) ++ (if required.nonEmpty then Chunk("required" -> Json.Arr(required)) else Chunk.empty)

    Json.Obj(fields)

  private def enumSchema(enum0: Schema.Enum[?]): Json.Obj =
    val cases = enum0.cases
    if cases.forall(_.schema.isInstanceOf[Schema.CaseClass0[?]]) then
      val values = cases.map(c => Json.Str(c.id))
      Json.Obj(Chunk("enum" -> Json.Arr(values)))
    else
      val oneOf = cases.map: c =>
        convert(c.schema): Json
      Json.Obj(Chunk("oneOf" -> Json.Arr(oneOf)))
