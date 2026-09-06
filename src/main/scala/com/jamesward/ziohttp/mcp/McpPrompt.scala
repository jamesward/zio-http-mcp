package com.jamesward.ziohttp.mcp

import zio.*

// --- Prompt Handler ---

trait McpPromptHandler:
  def definition: PromptDefinition
  def get(arguments: Map[String, String]): ZIO[Any, ToolError, PromptGetResult]
  /**
   * Render the prompt with access to the request context. Under the modern
   * (2026-07-28) revision a prompt may ask the client for input the same way a
   * tool does — `input_required` is universal in SEP-2322 — and is replayed on
   * the retry. Prompts built with [[McpPrompt.get]] ignore the context.
   */
  def getWithContext(
    arguments: Map[String, String],
    ctx: McpToolContext,
  ): ZIO[Any, ToolError, PromptGetResult] = get(arguments)

// --- Prompt Builder ---

final class McpPrompt private (
  val promptName: PromptName,
  val promptDescription: Option[String],
  val promptArguments: Chunk[PromptArgument],
):
  def description(d: String): McpPrompt =
    new McpPrompt(promptName, Some(d), promptArguments)

  def argument(name: String, description: String, required: Boolean = true): McpPrompt =
    new McpPrompt(promptName, promptDescription, promptArguments :+ PromptArgument(name, Some(description), Some(required)))

  def get(f: Map[String, String] => ZIO[Any, ToolError, PromptGetResult]): McpPromptHandler =
    getWithContext((arguments, _) => f(arguments))

  /**
   * Render the prompt with the request context in hand — to read the caller's
   * principal, or to ask the client for input (`ctx.elicit` / `ctx.sample`),
   * which a modern connection answers by retrying the `prompts/get`.
   */
  def getWithContext(
    f: (Map[String, String], McpToolContext) => ZIO[Any, ToolError, PromptGetResult]
  ): McpPromptHandler =
    val promptDef = PromptDefinition(
      name = promptName,
      description = promptDescription,
      arguments = if promptArguments.nonEmpty then Some(promptArguments) else None,
    )
    new McpPromptHandler:
      def definition: PromptDefinition = promptDef
      def get(arguments: Map[String, String]): ZIO[Any, ToolError, PromptGetResult] =
        f(arguments, McpToolContext.noop)
      override def getWithContext(
        arguments: Map[String, String],
        ctx: McpToolContext,
      ): ZIO[Any, ToolError, PromptGetResult] = f(arguments, ctx)

object McpPrompt:
  def apply(name: String): McpPrompt =
    new McpPrompt(PromptName(name), None, Chunk.empty)
