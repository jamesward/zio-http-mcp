package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.client.McpClientExtension
import zio.*
import zio.json.ast.Json

object McpApps:
  val Id: McpExtensionId = McpExtensionId.fromValid("io.modelcontextprotocol/ui")
  val HtmlMimeType: String = McpAppsMimeType.Html.value

  val serverExtension: McpServerExtension[Any] =
    McpServerExtension.capability(Id, Json.Obj())

  def clientExtension(settings: McpAppsClientSettings): McpClientExtension =
    McpClientExtension(Id, settingsJson(settings))

  def tool[R](handler: McpToolHandlerR[R], metadata: McpAppsToolMeta): McpToolHandlerR[R] =
    AppsToolHandler(handler, metadata)

  def resource(
    uri: McpUiUri,
    name: String,
    metadata: McpAppsResourceMeta,
    description: Option[String] = None,
  ): ResourceDefinition =
    ResourceDefinition(
      uri = uri.value,
      name = name,
      description = description,
      mimeType = Some(HtmlMimeType),
      meta = Some(McpMetadata.mergeNested(None, "ui", resourceMetaJson(metadata))),
    )

  def contents(uri: McpUiUri, html: String, metadata: McpAppsResourceMeta): ResourceContents =
    ResourceContents(
      uri = uri.value,
      mimeType = Some(HtmlMimeType),
      text = Some(html),
      meta = Some(McpMetadata.mergeNested(None, "ui", resourceMetaJson(metadata))),
    )

  def resourceHandler(
    uri: McpUiUri,
    name: String,
    metadata: McpAppsResourceMeta,
    description: Option[String] = None,
  )(
    read: ZIO[Any, ToolError, String]
  ): McpResourceHandler =
    AppsResourceHandler(resource(uri, name, metadata, description), uri, metadata, read)

  def result(
    fallback: McpAppsFallbackText,
    structuredContent: Option[Json] = None,
    metadata: Json.Obj = Json.Obj(),
  ): CallToolResult =
    CallToolResult(
      content = Chunk(ToolContent.text(fallback.value)),
      structuredContent = structuredContent,
      meta = Option.when(metadata.fields.nonEmpty)(metadata),
    )

  def withToolMetadata(definition: ToolDefinition, metadata: McpAppsToolMeta): ToolDefinition =
    val ui = McpMetadata.mergeNested(definition.meta, "ui", toolMetaJson(metadata))
    val withEntryPoint = metadata match
      case McpAppsToolMeta.View(_, _, true) =>
        McpMetadata.mergeNested(Some(ui), OpenAiUiKey, Json.Obj("entrypoints" -> Json.Arr(Json.Obj("type" -> Json.Str("global")))))
      case McpAppsToolMeta.View(_, _, false) | McpAppsToolMeta.AppOnly => ui
    definition.copy(meta = Some(withEntryPoint))

  /**
   * Whether the client declared that it renders MCP Apps views (`text/html;profile=mcp-app`
   * in its `io.modelcontextprotocol/ui` settings). A server can register UI tools, or
   * describe its results, only for such clients; every UI tool still needs a text fallback.
   */
  def clientSupportsHtml(ctx: McpRequestContext): Boolean =
    ctx.extensionCapabilities.get(Id).flatMap(_.asObject).flatMap(_.get("mimeTypes")).flatMap(_.asArray)
      .exists(_.exists(_.asString.contains(HtmlMimeType)))

  /**
   * The `domain` (sandbox origin) Claude gives the views of the server at `serverUrl`: the
   * first 32 hex characters of the SHA-256 of the URL, then `.claudemcpcontent.com`.
   * `serverUrl` is the URL the connector was added with, exactly. Other hosts define their
   * own formats.
   */
  def claudeDomain(serverUrl: String): String =
    val digest = java.security.MessageDigest.getInstance("SHA-256").digest(serverUrl.getBytes(java.nio.charset.StandardCharsets.UTF_8))
    digest.map(b => f"${b & 0xff}%02x").mkString.take(32) + ".claudemcpcontent.com"

  /** The OpenAI Apps SDK's `_meta` key, which Claude also reads for entry points. */
  private val OpenAiUiKey = "openai/ui"

  def withResourceMetadata(definition: ResourceDefinition, metadata: McpAppsResourceMeta): ResourceDefinition =
    definition.copy(meta = Some(McpMetadata.mergeNested(definition.meta, "ui", resourceMetaJson(metadata))))

  def withResourceMetadata(contents: ResourceContents, metadata: McpAppsResourceMeta): ResourceContents =
    contents.copy(meta = Some(McpMetadata.mergeNested(contents.meta, "ui", resourceMetaJson(metadata))))

  private final case class AppsToolHandler[R](
    delegate: McpToolHandlerR[R],
    metadata: McpAppsToolMeta,
  ) extends McpToolHandlerR[R]:
    def name: ToolName = delegate.name
    def definition: ToolDefinition = withToolMetadata(delegate.definition, metadata)
    override def requiredScopes: Set[auth.OauthScope] = delegate.requiredScopes
    override def taskPolicy: TaskPolicy = delegate.taskPolicy
    def call(args: Option[Json.Obj]): ZIO[R, Nothing, CallToolResult] = delegate.call(args)
    override def callWithContext(
      args: Option[Json.Obj],
      ctx: McpToolContext,
    ): ZIO[R, Nothing, CallToolResult] = delegate.callWithContext(args, ctx)

  private final case class AppsResourceHandler(
    definition: ResourceDefinition,
    uri: McpUiUri,
    metadata: McpAppsResourceMeta,
    readHtml: ZIO[Any, ToolError, String],
  ) extends McpResourceHandler:
    def read(requestedUri: String): ZIO[Any, ToolError, Chunk[ResourceContents]] =
      if requestedUri == uri.value then readHtml.map(html => Chunk(contents(uri, html, metadata)))
      else ZIO.fail(ToolError(s"Resource not found: $requestedUri"))

  private def settingsJson(settings: McpAppsClientSettings): Json.Obj =
    Json.Obj("mimeTypes" -> Json.Arr(settings.mimeTypes.map(mimeType => Json.Str(mimeType.value))))

  private def toolMetaJson(metadata: McpAppsToolMeta): Json.Obj = metadata match
    case McpAppsToolMeta.View(resourceUri, visibility, _) =>
      Json.Obj(
        "resourceUri" -> Json.Str(resourceUri.value),
        "visibility" -> Json.Arr(visibility.map(value => Json.Str(value.wire))),
      )
    case McpAppsToolMeta.AppOnly =>
      Json.Obj("visibility" -> Json.Arr(Json.Str(McpAppsVisibility.App.wire)))

  private def resourceMetaJson(metadata: McpAppsResourceMeta): Json.Obj =
    val csp = metadata.csp.map: value =>
      val fields = Chunk(
        Option.when(value.connectDomains.nonEmpty)("connectDomains" -> stringArray(value.connectDomains)),
        Option.when(value.resourceDomains.nonEmpty)("resourceDomains" -> stringArray(value.resourceDomains)),
        Option.when(value.frameDomains.nonEmpty)("frameDomains" -> stringArray(value.frameDomains)),
        Option.when(value.baseUriDomains.nonEmpty)("baseUriDomains" -> stringArray(value.baseUriDomains)),
      ).flatten
      "csp" -> (Json.Obj(fields): Json)
    val permissions = Option.when(metadata.permissions.nonEmpty):
      "permissions" -> (Json.Obj(metadata.permissions.toSeq.sortBy(_.wire).map(permission =>
        permission.wire -> (Json.Obj(): Json)
      )*): Json)
    Json.Obj(Chunk(
      csp,
      permissions,
      metadata.domain.map(value => "domain" -> (Json.Str(value): Json)),
      metadata.prefersBorder.map(value => "prefersBorder" -> (Json.Bool(value): Json)),
      metadata.preferredDisplayMode.map(value => "preferredDisplayMode" -> (Json.Str(value.wire): Json)),
    ).flatten)

  private def stringArray(values: Chunk[String]): Json = Json.Arr(values.map(Json.Str.apply))
