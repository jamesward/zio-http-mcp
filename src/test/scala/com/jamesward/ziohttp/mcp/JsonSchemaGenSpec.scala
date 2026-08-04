package com.jamesward.ziohttp.mcp

import zio.json.ast.Json
import zio.schema.*
import zio.schema.annotation.description
import zio.test.*

object JsonSchemaGenSpec extends ZIOSpecDefault:

  @description("A registered user of the system.")
  final case class User(
    @description("The user's unique id.") id: String,
    @description("Display name shown in the UI.") name: String,
    age: Int,
  ) derives Schema

  extension (j: Json)
    private def field(name: String): Option[Json] = j match
      case Json.Obj(fields) => fields.collectFirst { case (k, v) if k == name => v }
      case _                => None
    private def str: Option[String] = j match
      case Json.Str(s) => Some(s)
      case _           => None

  def spec = suite("JsonSchemaGen descriptions")(

    test("emits a description for the annotated record type and its annotated fields"):
      val json = JsonSchemaGen.fromSchema(summon[Schema[User]])
      val typeDesc = json.field("description").flatMap(_.str)
      val idDesc   = json.field("properties").flatMap(_.field("id")).flatMap(_.field("description")).flatMap(_.str)
      val nameDesc = json.field("properties").flatMap(_.field("name")).flatMap(_.field("description")).flatMap(_.str)
      assertTrue(
        json.field("type").flatMap(_.str).contains("object"),
        typeDesc.contains("A registered user of the system."),
        idDesc.contains("The user's unique id."),
        nameDesc.contains("Display name shown in the UI."),
      )
    ,

    test("omits a description for fields that are not annotated"):
      val json = JsonSchemaGen.fromSchema(summon[Schema[User]])
      val ageProp = json.field("properties").flatMap(_.field("age"))
      assertTrue(
        ageProp.isDefined,
        ageProp.flatMap(_.field("description")).isEmpty,
      )
    ,

    test("adds no description key when the schema has no @description annotations"):
      // Unit derives an empty record; nothing to describe.
      val json = JsonSchemaGen.fromSchema(Schema[Int])
      assertTrue(json.field("description").isEmpty)
    ,

    test("auto-generates a description for a Set from the collection + element type"):
      val json = JsonSchemaGen.fromSchema(summon[Schema[Set[User]]])
      assertTrue(
        json.field("type").flatMap(_.str).contains("array"),
        json.field("description").flatMap(_.str).contains("a set of User"),
        // element (User) field docs still flow through under items
        json.field("items").flatMap(_.field("properties")).flatMap(_.field("id"))
          .flatMap(_.field("description")).flatMap(_.str).contains("The user's unique id."),
      )
    ,

    test("auto-generates a description for a List of a primitive element"):
      val json = JsonSchemaGen.fromSchema(summon[Schema[List[String]]])
      assertTrue(
        json.field("type").flatMap(_.str).contains("array"),
        json.field("description").flatMap(_.str).contains("a list of String"),
      )
  )
