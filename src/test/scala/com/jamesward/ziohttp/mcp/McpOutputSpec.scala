package com.jamesward.ziohttp.mcp

import zio.Chunk
import zio.json.*
import zio.json.ast.Json
import zio.schema.*
import zio.schema.annotation.description
import zio.test.*

/**
 * The MCP spec constrains a tool's `outputSchema` to `{ "type": "object" }` and
 * its `structuredContent` to a JSON object. These tests verify that the generic
 * `McpOutput[A]` derived from a `Schema[A]` always satisfies that: object-typed
 * values pass through, and any other value (array, scalar) is nested under a
 * `result` property.
 */
object McpOutputSpec extends ZIOSpecDefault:

  case class Point(x: Int, y: Int) derives Schema

  @description("A documented widget.")
  case class Widget(
    @description("The widget's unique id.") id: String,
    @description("How many are in stock.") qty: Int,
  ) derives Schema

  extension (j: Json)
    private def field(name: String): Option[Json] = j match
      case Json.Obj(fs) => fs.collectFirst { case (k, v) if k == name => v }
      case _            => None
    private def str: Option[String] = j match
      case Json.Str(s) => Some(s)
      case _           => None

  private def schemaType(out: McpOutput[?]): Option[String] =
    out.outputSchema.flatMap(_.field("type")).flatMap(_.str)

  private def isJsonObject(j: Json): Boolean = j match
    case _: Json.Obj => true
    case _           => false

  def spec = suite("McpOutput — spec-compliant structured output for any Schema[A]")(

    test("object-typed value: schema is used verbatim; structuredContent is the object (not double-wrapped)"):
      val out = summon[McpOutput[Point]]
      val res = out.toResult(Point(1, 2))
      assertTrue(
        schemaType(out).contains("object"),
        out.outputSchema.flatMap(_.field("properties")).flatMap(_.field("x")).isDefined,
        res.structuredContent.exists(isJsonObject),
        res.structuredContent.map((_: Json).toJson).contains("""{"x":1,"y":2}"""),
      )
    ,

    test("scalar value: outputSchema wraps under `result`; structuredContent is an object"):
      val out = summon[McpOutput[Int]]
      val res = out.toResult(8)
      assertTrue(
        schemaType(out).contains("object"),
        out.outputSchema.flatMap(_.field("required")).map((_: Json).toJson).contains("""["result"]"""),
        out.outputSchema.flatMap(_.field("properties")).flatMap(_.field("result"))
          .flatMap(_.field("type")).flatMap(_.str).contains("integer"),
        res.structuredContent.exists(isJsonObject),
        res.structuredContent.map((_: Json).toJson).contains("""{"result":8}"""),
      )
    ,

    test("collection value: outputSchema wraps the (auto-described) array under `result`"):
      val out = summon[McpOutput[List[Int]]]
      val res = out.toResult(List(1, 2))
      val resultProp = out.outputSchema.flatMap(_.field("properties")).flatMap(_.field("result"))
      assertTrue(
        schemaType(out).contains("object"),
        resultProp.flatMap(_.field("type")).flatMap(_.str).contains("array"),
        resultProp.flatMap(_.field("description")).flatMap(_.str).contains("a list of Int"),
        res.structuredContent.exists(isJsonObject),
        res.structuredContent.map((_: Json).toJson).contains("""{"result":[1,2]}"""),
      )
    ,

    test("set of records: nested element schema keeps its object shape under result.items"):
      val out = summon[McpOutput[Set[Point]]]
      val res = out.toResult(Set(Point(1, 2)))
      val items = out.outputSchema.flatMap(_.field("properties")).flatMap(_.field("result")).flatMap(_.field("items"))
      assertTrue(
        schemaType(out).contains("object"),
        items.flatMap(_.field("type")).flatMap(_.str).contains("object"),
        res.structuredContent.exists(isJsonObject),
        res.structuredContent.map((_: Json).toJson).contains("""{"result":[{"x":1,"y":2}]}"""),
      )
    ,

    test("every derived output advertises an object outputSchema (spec compliance)"):
      assertTrue(
        List(
          schemaType(summon[McpOutput[Int]]),
          schemaType(summon[McpOutput[Boolean]]),
          schemaType(summon[McpOutput[List[String]]]),
          schemaType(summon[McpOutput[Set[Point]]]),
          schemaType(summon[McpOutput[Point]]),
          schemaType(summon[McpOutput[Map[String, Int]]]),
        ).forall(_.contains("object")),
      )
    ,

    test("returning a String stays plain text (explicit opt-out, no outputSchema)"):
      val out = summon[McpOutput[String]]
      val res = out.toResult("hello")
      val text = res.content.headOption match
        case Some(ToolContent.Text(t, _)) => t
        case _                            => ""
      assertTrue(
        out.outputSchema.isEmpty,
        res.structuredContent.isEmpty,
        text == "hello",
      )
    ,

    test("data-class field descriptions flow through the object outputSchema"):
      val out = summon[McpOutput[Widget]]
      val props = out.outputSchema.flatMap(_.field("properties"))
      assertTrue(
        out.outputSchema.flatMap(_.field("description")).flatMap(_.str).contains("A documented widget."),
        props.flatMap(_.field("id")).flatMap(_.field("description")).flatMap(_.str).contains("The widget's unique id."),
        props.flatMap(_.field("qty")).flatMap(_.field("description")).flatMap(_.str).contains("How many are in stock."),
      )
    ,

    test("data-class field descriptions survive collection wrapping (result.items.properties)"):
      val out = summon[McpOutput[Set[Widget]]]
      val itemProps = out.outputSchema
        .flatMap(_.field("properties")).flatMap(_.field("result"))
        .flatMap(_.field("items")).flatMap(_.field("properties"))
      assertTrue(
        itemProps.flatMap(_.field("id")).flatMap(_.field("description")).flatMap(_.str).contains("The widget's unique id."),
        itemProps.flatMap(_.field("qty")).flatMap(_.field("description")).flatMap(_.str).contains("How many are in stock."),
      )
  )
