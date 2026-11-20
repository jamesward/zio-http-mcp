package com.jamesward.ziohttp.mcp

import zio.json.*
import zio.json.ast.Json
import zio.schema.Schema
import zio.schema.codec.JsonCodec as SchemaJsonCodec

/** Domain-oriented JSON wire codec for extension operations.
  *
  * Most operations use [[McpWireCodec.schema]]. Extensions whose wire shape
  * cannot be represented faithfully by a derived schema keep their exceptional
  * codecs encapsulated in their domain companions.
  */
trait McpWireCodec[A]:
  def decode(json: Json): Either[String, A]
  def encode(value: A): Either[String, Json]

object McpWireCodec:
  def apply[A](
    decodeValue: Json => Either[String, A],
    encodeValue: A => Either[String, Json],
  ): McpWireCodec[A] =
    Codec(decodeValue, encodeValue)

  def schema[A](schema: Schema[A]): McpWireCodec[A] =
    val decoder = SchemaJsonCodec.jsonDecoder(schema)
    val encoder = McpJson.encoder(schema)
    McpWireCodec(
      json => decoder.decodeJson(json.toJson),
      value => encoder.encodeJson(value, None).toString.fromJson[Json],
    )

  def json[A: JsonCodec]: McpWireCodec[A] =
    McpWireCodec(
      value => value.as[A],
      value => value.toJsonAST,
    )

  private final case class Codec[A](
    decodeValue: Json => Either[String, A],
    encodeValue: A => Either[String, Json],
  ) extends McpWireCodec[A]:
    def decode(json: Json): Either[String, A] = decodeValue(json)
    def encode(value: A): Either[String, Json] = encodeValue(value)

/** JSON encoding of values that go on the wire as objects a JSON Schema describes. */
private[mcp] object McpJson:
  /**
   * Encode with `schema`, leaving a `None` field out instead of writing `null`.
   * The JSON Schema generated for an `Option` field (see [[JsonSchemaGen]]) is
   * its inner type, left out of `required`: a validator accepts a missing field
   * but rejects `null` (Claude Code reports "must be integer").
   */
  def encoder[A](schema: Schema[A]): JsonEncoder[A] =
    SchemaJsonCodec.jsonEncoder(SchemaJsonCodec.Configuration(explicitNulls = SchemaJsonCodec.ExplicitConfig(encoding = false)))(schema)
