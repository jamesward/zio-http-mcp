package com.jamesward.ziohttp.mcp

import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.schema.Schema
import zio.schema.codec.JsonCodec as SchemaJsonCodec

import scala.annotation.tailrec

// --- McpInput type class: provides JSON schema + decoding for tool inputs ---

trait McpInput[A]:
  def jsonSchema: Json.Obj
  def decode(args: Option[Json.Obj]): Either[String, A]

object McpInput:
  private val emptyObjectSchema = Json.Obj(Chunk("type" -> Json.Str("object"), "properties" -> Json.Obj()))

  given unit: McpInput[Unit] with
    val jsonSchema: Json.Obj = emptyObjectSchema
    def decode(args: Option[Json.Obj]): Either[String, Unit] = Right(())

  given [A](using schema: Schema[A]): McpInput[A] with
    val jsonSchema: Json.Obj = JsonSchemaGen.fromSchema(schema)
    private val decoder = SchemaJsonCodec.jsonDecoder(schema)
    def decode(args: Option[Json.Obj]): Either[String, A] =
      decoder.decodeJson(args.getOrElse(Json.Obj()).toJson)

  def raw(schema: Json.Obj): McpInput[Option[Json.Obj]] =
    new McpInput[Option[Json.Obj]]:
      val jsonSchema: Json.Obj = schema
      def decode(args: Option[Json.Obj]): Either[String, Option[Json.Obj]] = Right(args)

// --- McpOutput type class: converts tool output to CallToolResult ---

trait McpOutput[A]:
  def outputSchema: Option[Json.Obj]
  def toResult(output: A): CallToolResult

object McpOutput:
  given McpOutput[String] with
    val outputSchema: Option[Json.Obj] = None
    def toResult(output: String): CallToolResult =
      CallToolResult(content = Chunk(ToolContent.text(output)))

  given [A](using schema: Schema[A]): McpOutput[A] with
    private val jsonSchema = JsonSchemaGen.fromSchema(schema)
    // MCP spec requires outputSchema to have type "object"
    val outputSchema: Option[Json.Obj] =
      jsonSchema.get("type").flatMap(_.asString) match
        case Some("object") => Some(jsonSchema)
        case _ => None
    private val encoder = SchemaJsonCodec.jsonEncoder(schema)
    private val isStringLike: Boolean =
      import zio.schema.{Schema as S, StandardType as ST}
      @tailrec
      def check(s: S[?]): Boolean = s match
        case S.Primitive(st, _) => st eq ST.StringType
        case S.Transform(inner, _, _, _, _) => check(inner)
        case S.Lazy(s0) => check(s0())
        case _ => false
      check(schema)
    def toResult(output: A): CallToolResult =
      if isStringLike then
        CallToolResult(content = Chunk(ToolContent.text(output.toString)))
      else
        val jsonStr = encoder.encodeJson(output, None).toString
        // MCP spec requires structuredContent to be a JSON object
        val structured = jsonStr.fromJson[Json].toOption.collect:
          case obj: Json.Obj => obj
        CallToolResult(
          content = Chunk(ToolContent.text(jsonStr)),
          structuredContent = structured,
        )

  given McpOutput[ToolContent] with
    val outputSchema: Option[Json.Obj] = None
    def toResult(output: ToolContent): CallToolResult =
      CallToolResult(content = Chunk(output))

  given McpOutput[Chunk[ToolContent]] with
    val outputSchema: Option[Json.Obj] = None
    def toResult(output: Chunk[ToolContent]): CallToolResult =
      CallToolResult(content = output)

// --- Tool handler with environment requirement (contravariant, like ZIO/Routes) ---

trait McpToolHandlerR[-R]:
  def name: ToolName
  def definition: ToolDefinition
  def call(args: Option[Json.Obj]): ZIO[R, Nothing, CallToolResult]
  def callWithContext(args: Option[Json.Obj], ctx: McpToolContext): ZIO[R, Nothing, CallToolResult] =
    call(args)

// R=Any means no environment needed
type McpToolHandler = McpToolHandlerR[Any]

// --- Builder ---

final class McpTool private (
  val toolName: ToolName,
  val toolDescription: Option[String],
  val toolAnnotations: Option[ToolAnnotations],
):
  def description(d: String): McpTool =
    new McpTool(toolName, Some(d), toolAnnotations)

  def annotations(
    title: Option[String] = None,
    readOnly: OptBool = OptBool.Unset,
    destructive: OptBool = OptBool.Unset,
    idempotent: OptBool = OptBool.Unset,
    openWorld: OptBool = OptBool.Unset,
  ): McpTool =
    new McpTool(toolName, toolDescription, Some(ToolAnnotations(title, readOnly.toOption, destructive.toOption, idempotent.toOption, openWorld.toOption)))

  // --- handle: typed input/output ---

  def handle[R, E: McpError, In: McpInput, Out: McpOutput](f: In => ZIO[R, E, Out]): McpToolHandlerR[R] =
    handleWithContext[R, E, In, Out]((in, _) => f(in))

  // No error
  def handle[R, In: McpInput, Out: McpOutput](f: In => ZIO[R, Nothing, Out]): McpToolHandlerR[R] =
    handleWithContext[R, Nothing, In, Out]((in, _) => f(in))

  // No input
  def handle[R, E: McpError, Out: McpOutput](f: ZIO[R, E, Out]): McpToolHandlerR[R] =
    handleWithContext[R, E, Unit, Out]((_, _) => f)

  // No input, no error
  def handle[R, Out: McpOutput](f: ZIO[R, Nothing, Out]): McpToolHandlerR[R] =
    handleWithContext[R, Nothing, Unit, Out]((_, _) => f)

  // --- handleWithContext: typed input/output + McpToolContext ---

  def handleWithContext[R, E: McpError, In: McpInput, Out: McpOutput](f: (In, McpToolContext) => ZIO[R, E, Out]): McpToolHandlerR[R] =
    val mcpInput  = summon[McpInput[In]]
    val mcpOutput = summon[McpOutput[Out]]
    val mcpError  = summon[McpError[E]]

    val toolDef = ToolDefinition(
      name = toolName,
      description = toolDescription,
      inputSchema = mcpInput.jsonSchema,
      outputSchema = mcpOutput.outputSchema,
      annotations = toolAnnotations,
    )

    val capturedName = toolName
    new McpToolHandlerR[R]:
      def name: ToolName = capturedName
      def definition: ToolDefinition = toolDef

      def call(args: Option[Json.Obj]): ZIO[R, Nothing, CallToolResult] =
        callWithContext(args, McpToolContext.noop)

      override def callWithContext(args: Option[Json.Obj], ctx: McpToolContext): ZIO[R, Nothing, CallToolResult] =
        mcpInput.decode(args) match
          case Left(decodeError) =>
            ZIO.succeed(CallToolResult(
              content = Chunk(ToolContent.text(s"Invalid arguments: $decodeError")),
              isError = Some(true),
            ))
          case Right(input) =>
            f(input, ctx).fold(
              error => CallToolResult(
                content = Chunk(ToolContent.text(mcpError.message(error))),
                isError = Some(true),
              ),
              output => mcpOutput.toResult(output),
            ).catchAllDefect: defect =>
              ZIO.succeed(CallToolResult(
                content = Chunk(ToolContent.text(Option(defect.getMessage).getOrElse(defect.toString))),
                isError = Some(true),
              ))

  // No error
  def handleWithContext[R, In: McpInput, Out: McpOutput](f: (In, McpToolContext) => ZIO[R, Nothing, Out]): McpToolHandlerR[R] =
    handleWithContext[R, Nothing, In, Out](f)

  // No input
  def handleWithContext[R, E: McpError, Out: McpOutput](f: McpToolContext => ZIO[R, E, Out]): McpToolHandlerR[R] =
    handleWithContext[R, E, Unit, Out]((_, ctx) => f(ctx))

  // No input, no error
  def handleWithContext[R, Out: McpOutput](f: McpToolContext => ZIO[R, Nothing, Out]): McpToolHandlerR[R] =
    handleWithContext[R, Nothing, Unit, Out]((_, ctx) => f(ctx))

object McpTool:
  def apply(name: String): McpTool =
    new McpTool(ToolName(name), None, None)
