package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.auth.OauthScope
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.schema.Schema
import zio.schema.codec.JsonCodec as SchemaJsonCodec

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
    private val valueSchema = JsonSchemaGen.fromSchema(schema)
    private val isObject: Boolean =
      valueSchema.get("type").flatMap(_.asString).contains("object")

    // The MCP spec constrains a tool's `outputSchema` to `{ "type": "object" }`
    // and its `structuredContent` to a JSON object. A value whose schema is
    // already object-typed (a record, a map) is used verbatim; any other value
    // (an array, a scalar, a boolean) is nested under a single `result`
    // property, so *every* `Schema[A]` yields a spec-compliant object schema and
    // object structured content.
    private val WrapperKey = "result"

    val outputSchema: Option[Json.Obj] =
      if isObject then Some(valueSchema)
      else Some(Json.Obj(Chunk(
        "type"       -> Json.Str("object"),
        "properties" -> Json.Obj(Chunk(WrapperKey -> (valueSchema: Json))),
        "required"   -> Json.Arr(Json.Str(WrapperKey)),
      )))

    private val encoder = SchemaJsonCodec.jsonEncoder(schema)

    def toResult(output: A): CallToolResult =
      val encoded = encoder.encodeJson(output, None).toString.fromJson[Json].getOrElse(Json.Null)
      val structured: Json.Obj = encoded match
        case obj: Json.Obj => obj
        case other         => Json.Obj(Chunk(WrapperKey -> other))
      // Text content mirrors the (object) structured content, per the spec's
      // backward-compat recommendation.
      CallToolResult(
        content = Chunk(ToolContent.text(structured.toJson)),
        structuredContent = Some(structured),
      )

  given McpOutput[ToolContent] with
    val outputSchema: Option[Json.Obj] = None
    def toResult(output: ToolContent): CallToolResult =
      CallToolResult(content = Chunk(output))

  given McpOutput[Chunk[ToolContent]] with
    val outputSchema: Option[Json.Obj] = None
    def toResult(output: Chunk[ToolContent]): CallToolResult =
      CallToolResult(content = output)

  given McpOutput[CallToolResult] with
    val outputSchema: Option[Json.Obj] = None
    def toResult(output: CallToolResult): CallToolResult = output

// --- Tool handler with environment requirement (contravariant, like ZIO/Routes) ---

trait McpToolHandlerR[-R]:
  def name: ToolName
  def definition: ToolDefinition
  /** Per-tool scope requirements. Empty for tools that don't restrict scopes. */
  def requiredScopes: Set[OauthScope] = Set.empty
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
  val toolRequiredScopes: Set[OauthScope],
):
  def description(d: String): McpTool =
    new McpTool(toolName, Some(d), toolAnnotations, toolRequiredScopes)

  def annotations(
    title: Option[String] = None,
    readOnly: OptBool = OptBool.Unset,
    destructive: OptBool = OptBool.Unset,
    idempotent: OptBool = OptBool.Unset,
    openWorld: OptBool = OptBool.Unset,
  ): McpTool =
    new McpTool(toolName, toolDescription, Some(ToolAnnotations(title, readOnly.toOption, destructive.toOption, idempotent.toOption, openWorld.toOption)), toolRequiredScopes)

  /**
   * Add OAuth scope requirements for this tool. Server-wide [[com.jamesward.ziohttp.mcp.auth.McpAuth.requiredScopes]]
   * apply on top — per-tool scopes are additive.
   *
   * If the server has no `.auth(...)` configured, scope declarations are silently ignored,
   * keeping authoring fully opt-in.
   */
  def requireScopes(scopes: OauthScope*): McpTool =
    new McpTool(toolName, toolDescription, toolAnnotations, toolRequiredScopes ++ scopes)

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
    val capturedScopes = toolRequiredScopes
    new McpToolHandlerR[R]:
      def name: ToolName = capturedName
      def definition: ToolDefinition = toolDef
      override def requiredScopes: Set[OauthScope] = capturedScopes

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
            ).catchAllDefect:
              // A modern (MRTR) input request is not a failure — let it
              // propagate so the dispatcher can turn it into an
              // InputRequiredResult and re-run the handler on the retry.
              case signal: McpToolContext.InputRequiredSignal =>
                ZIO.die(signal)
              case defect =>
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
    new McpTool(ToolName(name), None, None, Set.empty)
