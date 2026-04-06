package com.jamesward.ziohttp.mcp

import zio.*
import zio.json.*
import zio.json.ast.Json

// --- Implementation Info ---

case class Implementation(name: String, version: String)

object Implementation:
  given CanEqual[Implementation, Implementation] = CanEqual.derived
  given JsonCodec[Implementation] = DeriveJsonCodec.gen[Implementation]

// --- Tool Annotations ---

case class ToolAnnotations(
  title: Option[String] = None,
  readOnlyHint: Option[Boolean] = None,
  destructiveHint: Option[Boolean] = None,
  idempotentHint: Option[Boolean] = None,
  openWorldHint: Option[Boolean] = None,
)

object ToolAnnotations:
  given CanEqual[ToolAnnotations, ToolAnnotations] = CanEqual.derived
  given JsonCodec[ToolAnnotations] = DeriveJsonCodec.gen[ToolAnnotations]

// --- Tool Definition (wire format for tools/list) ---

case class ToolDefinition(
  name: ToolName,
  description: Option[String] = None,
  inputSchema: Json.Obj,
  outputSchema: Option[Json.Obj] = None,
  annotations: Option[ToolAnnotations] = None,
)

object ToolDefinition:
  given CanEqual[ToolDefinition, ToolDefinition] = CanEqual.derived
  given JsonCodec[ToolDefinition] = DeriveJsonCodec.gen[ToolDefinition]

// --- Initialize ---

case class InitializeParams(
  protocolVersion: String,
  capabilities: Json.Obj,
  clientInfo: Implementation,
)

object InitializeParams:
  given CanEqual[InitializeParams, InitializeParams] = CanEqual.derived
  given JsonCodec[InitializeParams] = DeriveJsonCodec.gen[InitializeParams]

case class ServerCapabilities(
  tools: Option[Json.Obj] = None,
  resources: Option[Json.Obj] = None,
  prompts: Option[Json.Obj] = None,
  logging: Option[Json.Obj] = None,
  completions: Option[Json.Obj] = None,
)

object ServerCapabilities:
  given CanEqual[ServerCapabilities, ServerCapabilities] = CanEqual.derived
  given JsonCodec[ServerCapabilities] = DeriveJsonCodec.gen[ServerCapabilities]

case class InitializeResult(
  protocolVersion: String,
  capabilities: ServerCapabilities,
  serverInfo: Implementation,
  instructions: Option[String] = None,
)

object InitializeResult:
  given CanEqual[InitializeResult, InitializeResult] = CanEqual.derived
  given JsonCodec[InitializeResult] = DeriveJsonCodec.gen[InitializeResult]

// --- Tools List ---

case class ToolsListParams(
  cursor: Option[String] = None,
)

object ToolsListParams:
  given CanEqual[ToolsListParams, ToolsListParams] = CanEqual.derived
  given JsonCodec[ToolsListParams] = DeriveJsonCodec.gen[ToolsListParams]

case class ToolsListResult(
  tools: Chunk[ToolDefinition],
  nextCursor: Option[String] = None,
)

object ToolsListResult:
  given CanEqual[ToolsListResult, ToolsListResult] = CanEqual.derived
  given JsonCodec[ToolsListResult] = DeriveJsonCodec.gen[ToolsListResult]

// --- Tools Call ---

case class ToolCallParams(
  name: ToolName,
  arguments: Option[Json.Obj] = None,
)

object ToolCallParams:
  given CanEqual[ToolCallParams, ToolCallParams] = CanEqual.derived
  given JsonCodec[ToolCallParams] = DeriveJsonCodec.gen[ToolCallParams]

case class CallToolResult(
  content: Chunk[ToolContent] = Chunk.empty,
  structuredContent: Option[Json] = None,
  isError: Option[Boolean] = None,
)

object CallToolResult:
  given CanEqual[CallToolResult, CallToolResult] = CanEqual.derived
  given JsonCodec[CallToolResult] = DeriveJsonCodec.gen[CallToolResult]

// --- Resources ---

case class ResourceDefinition(
  uri: String,
  name: String,
  description: Option[String] = None,
  mimeType: Option[String] = None,
)

object ResourceDefinition:
  given CanEqual[ResourceDefinition, ResourceDefinition] = CanEqual.derived
  given JsonCodec[ResourceDefinition] = DeriveJsonCodec.gen[ResourceDefinition]

case class ResourceTemplateDefinition(
  uriTemplate: String,
  name: String,
  description: Option[String] = None,
  mimeType: Option[String] = None,
)

object ResourceTemplateDefinition:
  given CanEqual[ResourceTemplateDefinition, ResourceTemplateDefinition] = CanEqual.derived
  given JsonCodec[ResourceTemplateDefinition] = DeriveJsonCodec.gen[ResourceTemplateDefinition]

case class ResourcesListResult(
  resources: Chunk[ResourceDefinition],
  nextCursor: Option[String] = None,
)

object ResourcesListResult:
  given CanEqual[ResourcesListResult, ResourcesListResult] = CanEqual.derived
  given JsonCodec[ResourcesListResult] = DeriveJsonCodec.gen[ResourcesListResult]

case class ResourceTemplatesListResult(
  resourceTemplates: Chunk[ResourceTemplateDefinition],
  nextCursor: Option[String] = None,
)

object ResourceTemplatesListResult:
  given CanEqual[ResourceTemplatesListResult, ResourceTemplatesListResult] = CanEqual.derived
  given JsonCodec[ResourceTemplatesListResult] = DeriveJsonCodec.gen[ResourceTemplatesListResult]

case class ResourceReadParams(uri: String)

object ResourceReadParams:
  given CanEqual[ResourceReadParams, ResourceReadParams] = CanEqual.derived
  given JsonCodec[ResourceReadParams] = DeriveJsonCodec.gen[ResourceReadParams]

case class ResourceReadResult(contents: Chunk[ResourceContents])

object ResourceReadResult:
  given CanEqual[ResourceReadResult, ResourceReadResult] = CanEqual.derived
  given JsonCodec[ResourceReadResult] = DeriveJsonCodec.gen[ResourceReadResult]

case class ResourceSubscribeParams(uri: String)

object ResourceSubscribeParams:
  given CanEqual[ResourceSubscribeParams, ResourceSubscribeParams] = CanEqual.derived
  given JsonCodec[ResourceSubscribeParams] = DeriveJsonCodec.gen[ResourceSubscribeParams]

// --- Prompts ---

case class PromptArgument(
  name: String,
  description: Option[String] = None,
  required: Option[Boolean] = None,
)

object PromptArgument:
  given CanEqual[PromptArgument, PromptArgument] = CanEqual.derived
  given JsonCodec[PromptArgument] = DeriveJsonCodec.gen[PromptArgument]

case class PromptDefinition(
  name: PromptName,
  description: Option[String] = None,
  arguments: Option[Chunk[PromptArgument]] = None,
)

object PromptDefinition:
  given CanEqual[PromptDefinition, PromptDefinition] = CanEqual.derived
  given JsonCodec[PromptDefinition] = DeriveJsonCodec.gen[PromptDefinition]

case class PromptsListResult(
  prompts: Chunk[PromptDefinition],
  nextCursor: Option[String] = None,
)

object PromptsListResult:
  given CanEqual[PromptsListResult, PromptsListResult] = CanEqual.derived
  given JsonCodec[PromptsListResult] = DeriveJsonCodec.gen[PromptsListResult]

case class PromptGetParams(
  name: PromptName,
  arguments: Option[Map[String, String]] = None,
)

object PromptGetParams:
  given CanEqual[PromptGetParams, PromptGetParams] = CanEqual.derived
  given JsonCodec[PromptGetParams] = DeriveJsonCodec.gen[PromptGetParams]

case class PromptMessage(
  role: Role,
  content: ToolContent,
)

object PromptMessage:
  given CanEqual[PromptMessage, PromptMessage] = CanEqual.derived
  given JsonCodec[PromptMessage] = DeriveJsonCodec.gen[PromptMessage]

case class PromptGetResult(
  description: Option[String] = None,
  messages: Chunk[PromptMessage],
)

object PromptGetResult:
  given CanEqual[PromptGetResult, PromptGetResult] = CanEqual.derived
  given JsonCodec[PromptGetResult] = DeriveJsonCodec.gen[PromptGetResult]

// --- Logging ---

case class LoggingSetLevelParams(level: String)

object LoggingSetLevelParams:
  given CanEqual[LoggingSetLevelParams, LoggingSetLevelParams] = CanEqual.derived
  given JsonCodec[LoggingSetLevelParams] = DeriveJsonCodec.gen[LoggingSetLevelParams]

// --- Completions ---

case class CompletionCompleteParams(
  ref: CompletionRef,
  argument: CompletionArgument,
)

object CompletionCompleteParams:
  given CanEqual[CompletionCompleteParams, CompletionCompleteParams] = CanEqual.derived
  given JsonCodec[CompletionCompleteParams] = DeriveJsonCodec.gen[CompletionCompleteParams]

case class CompletionRef(
  `type`: CompletionRefType,
  name: Option[String] = None,
  uri: Option[String] = None,
)

object CompletionRef:
  given CanEqual[CompletionRef, CompletionRef] = CanEqual.derived
  given JsonCodec[CompletionRef] = DeriveJsonCodec.gen[CompletionRef]

case class CompletionArgument(
  name: String,
  value: String,
)

object CompletionArgument:
  given CanEqual[CompletionArgument, CompletionArgument] = CanEqual.derived
  given JsonCodec[CompletionArgument] = DeriveJsonCodec.gen[CompletionArgument]

case class CompletionResult(
  completion: CompletionValues,
)

object CompletionResult:
  given CanEqual[CompletionResult, CompletionResult] = CanEqual.derived
  given JsonCodec[CompletionResult] = DeriveJsonCodec.gen[CompletionResult]

case class CompletionValues(
  values: Chunk[String],
  total: Option[Int] = None,
  hasMore: Option[Boolean] = None,
)

object CompletionValues:
  given CanEqual[CompletionValues, CompletionValues] = CanEqual.derived
  given JsonCodec[CompletionValues] = DeriveJsonCodec.gen[CompletionValues]
