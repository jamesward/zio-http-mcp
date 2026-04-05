package com.jamesward.ziohttp.mcp

import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.schema.Schema
import zio.schema.codec.JsonCodec as SchemaJsonCodec

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
    readOnly: Option[Boolean] = None,
    destructive: Option[Boolean] = None,
    idempotent: Option[Boolean] = None,
    openWorld: Option[Boolean] = None,
  ): McpTool =
    new McpTool(toolName, toolDescription, Some(ToolAnnotations(title, readOnly, destructive, idempotent, openWorld)))

  def handle[R, E: McpError, In: Schema, Out: Schema](f: In => ZIO[R, E, Out]): McpToolHandlerR[R] =
    val inSchema  = summon[Schema[In]]
    val outSchema = summon[Schema[Out]]
    val mcpError  = summon[McpError[E]]
    val inDecoder = SchemaJsonCodec.jsonDecoder(inSchema)
    val outEncoder = SchemaJsonCodec.jsonEncoder(outSchema)
    val inputJsonSchema  = JsonSchemaGen.fromSchema(inSchema)
    val outputJsonSchema = JsonSchemaGen.fromSchema(outSchema)

    val toolDef = ToolDefinition(
      name = toolName.value,
      description = toolDescription,
      inputSchema = inputJsonSchema,
      outputSchema = Some(outputJsonSchema),
      annotations = toolAnnotations,
    )

    val capturedName = toolName
    new McpToolHandlerR[R]:
      def name: ToolName = capturedName
      def definition: ToolDefinition = toolDef

      def call(args: Option[Json.Obj]): ZIO[R, Nothing, CallToolResult] =
        val argsJson = args.getOrElse(Json.Obj()).toJson
        inDecoder.decodeJson(argsJson) match
          case Left(decodeError) =>
            ZIO.succeed(CallToolResult(
              content = Chunk(ToolContent.text(s"Invalid arguments: $decodeError")),
              isError = Some(true),
            ))
          case Right(input) =>
            f(input).fold(
              error => CallToolResult(
                content = Chunk(ToolContent.text(mcpError.message(error))),
                isError = Some(true),
              ),
              output =>
                val jsonStr = outEncoder.encodeJson(output, None).toString
                val structured = jsonStr.fromJson[Json].toOption
                CallToolResult(
                  content = Chunk(ToolContent.text(jsonStr)),
                  structuredContent = structured,
                ),
            )

  def handleDirectWithContext[In: Schema](f: (In, McpToolContext) => ZIO[Any, Any, Chunk[ToolContent]]): McpToolHandler =
    val inSchema  = summon[Schema[In]]
    val inDecoder = SchemaJsonCodec.jsonDecoder(inSchema)
    val inputJsonSchema = JsonSchemaGen.fromSchema(inSchema)

    val toolDef = ToolDefinition(
      name = toolName.value,
      description = toolDescription,
      inputSchema = inputJsonSchema,
      annotations = toolAnnotations,
    )

    val capturedName = toolName
    new McpToolHandlerR[Any]:
      def name: ToolName = capturedName
      def definition: ToolDefinition = toolDef

      def call(args: Option[Json.Obj]): ZIO[Any, Nothing, CallToolResult] =
        callWithContext(args, McpToolContext.noop)

      override def callWithContext(args: Option[Json.Obj], ctx: McpToolContext): ZIO[Any, Nothing, CallToolResult] =
        val argsJson = args.getOrElse(Json.Obj()).toJson
        inDecoder.decodeJson(argsJson) match
          case Left(decodeError) =>
            ZIO.succeed(CallToolResult(
              content = Chunk(ToolContent.text(s"Invalid arguments: $decodeError")),
              isError = Some(true),
            ))
          case Right(input) =>
            f(input, ctx).fold(
              error => CallToolResult(
                content = Chunk(ToolContent.text(error.toString)),
                isError = Some(true),
              ),
              content => CallToolResult(content = content),
            )

  def withRawInputSchema(schema: Json.Obj): McpToolRaw =
    McpToolRaw(toolName, toolDescription, toolAnnotations, schema)

  def handleDirect[In: Schema](f: In => ZIO[Any, Any, Chunk[ToolContent]]): McpToolHandler =
    val inSchema  = summon[Schema[In]]
    val inDecoder = SchemaJsonCodec.jsonDecoder(inSchema)
    val inputJsonSchema = JsonSchemaGen.fromSchema(inSchema)

    val toolDef = ToolDefinition(
      name = toolName.value,
      description = toolDescription,
      inputSchema = inputJsonSchema,
      annotations = toolAnnotations,
    )

    val capturedName = toolName
    new McpToolHandlerR[Any]:
      def name: ToolName = capturedName
      def definition: ToolDefinition = toolDef

      def call(args: Option[Json.Obj]): ZIO[Any, Nothing, CallToolResult] =
        val argsJson = args.getOrElse(Json.Obj()).toJson
        inDecoder.decodeJson(argsJson) match
          case Left(decodeError) =>
            ZIO.succeed(CallToolResult(
              content = Chunk(ToolContent.text(s"Invalid arguments: $decodeError")),
              isError = Some(true),
            ))
          case Right(input) =>
            f(input).fold(
              error => CallToolResult(
                content = Chunk(ToolContent.text(error.toString)),
                isError = Some(true),
              ),
              content => CallToolResult(content = content),
            )

// --- Raw JSON Schema tool builder (for tools with hand-crafted JSON Schema) ---

final class McpToolRaw private[mcp] (
  val toolName: ToolName,
  val toolDescription: Option[String],
  val toolAnnotations: Option[ToolAnnotations],
  val rawInputSchema: Json.Obj,
):
  def handleDirect(f: Option[Json.Obj] => ZIO[Any, Any, Chunk[ToolContent]]): McpToolHandler =
    val toolDef = ToolDefinition(
      name = toolName.value,
      description = toolDescription,
      inputSchema = rawInputSchema,
      annotations = toolAnnotations,
    )
    val capturedName = toolName
    val handler = f
    new McpToolHandlerR[Any]:
      def name: ToolName = capturedName
      def definition: ToolDefinition = toolDef
      def call(args: Option[Json.Obj]): ZIO[Any, Nothing, CallToolResult] =
        handler(args).fold(
          error => CallToolResult(
            content = Chunk(ToolContent.text(error.toString)),
            isError = Some(true),
          ),
          content => CallToolResult(content = content),
        )

object McpTool:
  def apply(name: String): McpTool =
    new McpTool(ToolName(name), None, None)
