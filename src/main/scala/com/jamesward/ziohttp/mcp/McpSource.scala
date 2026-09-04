package com.jamesward.ziohttp.mcp

import zio.*
import zio.json.ast.Json

/**
 * A dynamic provider of the `initialize` result's `instructions` string, consulted by
 * [[McpServer]] on every `initialize` with the request's [[McpToolContext]] in hand — so
 * the instructions can vary per caller (`ctx.principal`) and per mount (`ctx.pathParams`,
 * e.g. the `<slug>` of a path-parameterised mount).
 *
 * Pass one to the [[McpServer.instructions]] overload (the dynamic analogue of passing a
 * plain `String`). Setting one replaces any static instructions value, and vice versa —
 * the two are mutually exclusive, last one wins, because `instructions` is a single value
 * rather than a combinable collection like tools/resources.
 *
 * `instructions` runs in the `Nothing` error channel: a provider that can't reach its
 * backing data should degrade to `None` rather than failing the handshake. Contravariant
 * in `R`, so a provider needing environment `R1` widens the server to `R & R1`.
 *
 * It's a single-abstract-method trait, so a lambda works: `InstructionsSource(ctx => ...)`.
 */
trait InstructionsSource[-R]:
  def instructions(ctx: McpToolContext): URIO[R, Option[String]]

object InstructionsSource:
  /** Build an [[InstructionsSource]] from a function. */
  def apply[R](f: McpToolContext => URIO[R, Option[String]]): InstructionsSource[R] =
    (ctx: McpToolContext) => f(ctx)

  /** A provider that always supplies the same fixed instructions. */
  def const(text: String): InstructionsSource[Any] =
    (_: McpToolContext) => ZIO.succeed(Some(text))


/**
 * A dynamic provider of the handshake `serverInfo` ([[Implementation]]) — the
 * server identity (name, title, description, icons per SEP-973, websiteUrl) a client MAY
 * surface (e.g. a connector icon). Consulted by [[McpServer]] on every
 * `initialize` and `server/discover` with the request's [[McpToolContext]] in
 * hand, so the identity can vary per caller (`ctx.principal`) and — the point
 * of this seam — per mount (`ctx.pathParams`, e.g. the `<slug>` of a
 * path-parameterised mount, so one server brands itself differently for each
 * value it serves).
 *
 * Pass one to [[McpServer.serverInfo]] (the dynamic analogue of the static
 * `McpServer(name, version)` identity). Setting one replaces the static
 * identity in the handshake response. Runs in the `Nothing` error channel: a
 * provider that can't reach its backing data should degrade to a sensible
 * default (typically the static identity) rather than failing the handshake.
 * Contravariant in `R`.
 *
 * It's a single-abstract-method trait, so a lambda works: `ServerInfoSource(ctx => ...)`.
 */
trait ServerInfoSource[-R]:
  def serverInfo(ctx: McpToolContext): URIO[R, Implementation]

object ServerInfoSource:
  /** Build a [[ServerInfoSource]] from a function. */
  def apply[R](f: McpToolContext => URIO[R, Implementation]): ServerInfoSource[R] =
    (ctx: McpToolContext) => f(ctx)


/**
 * A dynamic source of tools, queried by [[McpServer]] at request time.
 *
 * Unlike a statically registered `.tool(...)`, a source is consulted on every
 * `tools/list` / `tools/call` with the request's [[McpToolContext]] in hand — so it
 * can return a different tool set per caller (`ctx.principal`) and per mount
 * (`ctx.pathParams`, e.g. the `<slug>` of a path-parameterised mount). This is what
 * lets a single server proxy a set of upstream tools that live elsewhere and change
 * over time.
 *
 * Contravariant in `R` like [[McpToolHandlerR]] / `Routes`, so a source needing
 * environment `R1` widens the server to `R & R1`.
 */
trait McpToolSource[-R]:
  /**
   * Tools to merge into `tools/list`, with their names unchanged. The server appends
   * these to the statically registered tools; the result is assumed already
   * access-scoped by the source, so the server does not filter it further.
   *
   * Returns `Nothing` in the error channel: a source that can't reach its backing data
   * degrades to an empty (or partial) list rather than failing the whole listing.
   */
  def listTools(ctx: McpToolContext): ZIO[R, Nothing, Chunk[ToolDefinition]]

  /**
   * Handle a `tools/call` for a name not matched by any static `.tool(...)`. The source
   * resolves and dispatches it. An unknown or forbidden name returns a
   * [[CallToolResult]] with `isError = true` rather than failing the channel.
   */
  def callTool(name: ToolName, args: Option[Json.Obj], ctx: McpToolContext): ZIO[R, Nothing, CallToolResult]

object McpToolSource:
  /** A source contributing no tools; an unknown name yields an `isError` result. */
  val empty: McpToolSource[Any] = new McpToolSource[Any]:
    def listTools(ctx: McpToolContext): ZIO[Any, Nothing, Chunk[ToolDefinition]] =
      ZIO.succeed(Chunk.empty)
    def callTool(name: ToolName, args: Option[Json.Obj], ctx: McpToolContext): ZIO[Any, Nothing, CallToolResult] =
      ZIO.succeed(CallToolResult(
        content = Chunk(ToolContent.text(s"Unknown tool: ${name.value}")),
        isError = Some(true),
      ))

/**
 * A dynamic source of resources, resource templates, and completions, queried by
 * [[McpServer]] at request time. The companion to [[McpToolSource]] for the
 * resources side of the protocol.
 */
trait McpResourceSource[-R]:
  /** Concrete resources to merge into `resources/list`, URIs unchanged. */
  def listResources(ctx: McpToolContext): ZIO[R, Nothing, Chunk[ResourceDefinition]]

  /** Resource templates to merge into `resources/templates/list`, URIs unchanged. */
  def listResourceTemplates(ctx: McpToolContext): ZIO[R, Nothing, Chunk[ResourceTemplateDefinition]]

  /**
   * Read a resource the server's static resources/templates didn't match. A
   * [[ToolError]] surfaces as a JSON-RPC error (e.g. resource-not-found).
   */
  def readResource(uri: String, ctx: McpToolContext): ZIO[R, ToolError, Chunk[ResourceContents]]

  /**
   * `completion/complete` for a source-provided ref (a resource-template URI or a
   * prompt). Defaults to no candidates. The server delegates here when its own static
   * completions have nothing to add.
   */
  def complete(ref: CompletionRef, argument: CompletionArgument, ctx: McpToolContext): ZIO[R, Nothing, CompletionResult] =
    ZIO.succeed(CompletionResult(CompletionValues(values = Chunk.empty)))

  /**
   * `resources/directory/read` (SEP-2640): the direct children of a directory resource.
   * Files carry ordinary resource metadata; subdirectories are marked with `mimeType`
   * `inode/directory`. A [[ToolError]] (unknown URI / not a directory) surfaces as a
   * JSON-RPC `InvalidParams` error. Defaults to unsupported; override + advertise via
   * [[capabilities]] to enable.
   */
  @deprecated("Register an McpServerExtension operation instead", "0.6.0")
  def readDirectory(uri: String, ctx: McpToolContext): ZIO[R, ToolError, Chunk[ResourceDefinition]] =
    ZIO.fail(ToolError(s"Directory read not supported: $uri"))

  /**
   * Extension capabilities this source contributes to the server's `initialize`
   * response, keyed by reverse-domain extension id. For example, a source that
   * implements [[readDirectory]] for skills declares
   * `Map("io.modelcontextprotocol/skills" -> Json.Obj("directoryRead" -> Json.Bool(true)))`.
   * Defaults to none.
   */
  @deprecated("Register an McpServerExtension capability instead", "0.6.0")
  def capabilities: Map[String, zio.json.ast.Json.Obj] = Map.empty

object McpResourceSource:
  /** A source contributing no resources, templates, or completions. */
  val empty: McpResourceSource[Any] = new McpResourceSource[Any]:
    def listResources(ctx: McpToolContext): ZIO[Any, Nothing, Chunk[ResourceDefinition]] =
      ZIO.succeed(Chunk.empty)
    def listResourceTemplates(ctx: McpToolContext): ZIO[Any, Nothing, Chunk[ResourceTemplateDefinition]] =
      ZIO.succeed(Chunk.empty)
    def readResource(uri: String, ctx: McpToolContext): ZIO[Any, ToolError, Chunk[ResourceContents]] =
      ZIO.fail(ToolError(s"Resource not found: $uri"))
