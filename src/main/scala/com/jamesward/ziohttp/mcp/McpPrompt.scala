package com.jamesward.ziohttp.mcp

import zio.*

// --- Prompt Handler ---

trait McpPromptHandler:
  def definition: PromptDefinition
  def get(arguments: Map[String, String]): ZIO[Any, ToolError, PromptGetResult]

// --- Prompt Builder ---

final class McpPrompt private (
  val promptName: String,
  val promptDescription: Option[String],
  val promptArguments: Chunk[PromptArgument],
):
  def description(d: String): McpPrompt =
    new McpPrompt(promptName, Some(d), promptArguments)

  def argument(name: String, description: String, required: Boolean = true): McpPrompt =
    new McpPrompt(promptName, promptDescription, promptArguments :+ PromptArgument(name, Some(description), Some(required)))

  def get(f: Map[String, String] => ZIO[Any, ToolError, PromptGetResult]): McpPromptHandler =
    val promptDef = PromptDefinition(
      name = promptName,
      description = promptDescription,
      arguments = if promptArguments.nonEmpty then Some(promptArguments) else None,
    )
    val handler = f
    new McpPromptHandler:
      def definition: PromptDefinition = promptDef
      def get(arguments: Map[String, String]): ZIO[Any, ToolError, PromptGetResult] = handler(arguments)

object McpPrompt:
  def apply(name: String): McpPrompt =
    new McpPrompt(name, None, Chunk.empty)
