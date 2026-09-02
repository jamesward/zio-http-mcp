package com.jamesward.ziohttp.mcp

import zio.*
import zio.json.ast.Json

object McpMetadata:
  def merge(existing: Option[Json.Obj], additions: Json.Obj): Json.Obj =
    mergeObjects(existing.getOrElse(Json.Obj()), additions)

  def mergeObjects(existing: Json.Obj, additions: Json.Obj): Json.Obj =
    val replaced = additions.fields.map(_._1).toSet
    Json.Obj(existing.fields.filterNot((key, _) => replaced.contains(key)) ++ additions.fields)

  def mergeNested(existing: Option[Json.Obj], namespace: String, additions: Json.Obj): Json.Obj =
    val base = existing.getOrElse(Json.Obj())
    val nested = base.get(namespace).flatMap(_.asObject).getOrElse(Json.Obj())
    merge(Some(base), Json.Obj(namespace -> mergeObjects(nested, additions)))

  extension (definition: ToolDefinition)
    def withMetadata(additions: Json.Obj): ToolDefinition =
      definition.copy(meta = Some(merge(definition.meta, additions)))

  extension (definition: ResourceDefinition)
    def withMetadata(additions: Json.Obj): ResourceDefinition =
      definition.copy(meta = Some(merge(definition.meta, additions)))

  extension (definition: ResourceTemplateDefinition)
    def withMetadata(additions: Json.Obj): ResourceTemplateDefinition =
      definition.copy(meta = Some(merge(definition.meta, additions)))

  extension (contents: ResourceContents)
    def withMetadata(additions: Json.Obj): ResourceContents =
      contents.copy(meta = Some(merge(contents.meta, additions)))

  extension (result: CallToolResult)
    def withMetadata(additions: Json.Obj): CallToolResult =
      result.copy(meta = Some(merge(result.meta, additions)))

  extension [R](handler: McpToolHandlerR[R])
    def withMetadata(additions: Json.Obj): McpToolHandlerR[R] =
      MetadataToolHandler(handler, additions)

  private final case class MetadataToolHandler[R](
    delegate: McpToolHandlerR[R],
    additions: Json.Obj,
  ) extends McpToolHandlerR[R]:
    def name: ToolName = delegate.name
    def definition: ToolDefinition = delegate.definition.withMetadata(additions)
    override def requiredScopes: Set[auth.OauthScope] = delegate.requiredScopes
    def call(args: Option[Json.Obj]): ZIO[R, Nothing, CallToolResult] = delegate.call(args)
    override def callWithContext(
      args: Option[Json.Obj],
      ctx: McpToolContext,
    ): ZIO[R, Nothing, CallToolResult] =
      delegate.callWithContext(args, ctx)
