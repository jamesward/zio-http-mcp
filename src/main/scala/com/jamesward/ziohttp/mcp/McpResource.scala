package com.jamesward.ziohttp.mcp

import zio.*

// --- Resource Handler ---

trait McpResourceHandler:
  def definition: ResourceDefinition
  def read(uri: String): ZIO[Any, ToolError, Chunk[ResourceContents]]

// --- Resource Template Handler ---

trait McpResourceTemplateHandler:
  def definition: ResourceTemplateDefinition
  def read(uri: String): ZIO[Any, ToolError, Chunk[ResourceContents]]

// --- Resource Builder ---

final class McpResource private (
  val uri: String,
  val resourceName: String,
  val resourceDescription: Option[String],
  val resourceMimeType: Option[String],
):
  def description(d: String): McpResource =
    new McpResource(uri, resourceName, Some(d), resourceMimeType)

  def mimeType(m: String): McpResource =
    new McpResource(uri, resourceName, resourceDescription, Some(m))

  def read(f: String => ZIO[Any, ToolError, Chunk[ResourceContents]]): McpResourceHandler =
    val resDef = ResourceDefinition(
      uri = uri,
      name = resourceName,
      description = resourceDescription,
      mimeType = resourceMimeType,
    )
    val handler = f
    new McpResourceHandler:
      def definition: ResourceDefinition = resDef
      def read(uri: String): ZIO[Any, ToolError, Chunk[ResourceContents]] = handler(uri)

object McpResource:
  def apply(uri: String, name: String): McpResource =
    new McpResource(uri, name, None, None)

// --- Resource Template Builder ---

final class McpResourceTemplate private (
  val uriTemplate: String,
  val templateName: String,
  val templateDescription: Option[String],
  val templateMimeType: Option[String],
):
  def description(d: String): McpResourceTemplate =
    new McpResourceTemplate(uriTemplate, templateName, Some(d), templateMimeType)

  def mimeType(m: String): McpResourceTemplate =
    new McpResourceTemplate(uriTemplate, templateName, templateDescription, Some(m))

  def read(f: String => ZIO[Any, ToolError, Chunk[ResourceContents]]): McpResourceTemplateHandler =
    val tmplDef = ResourceTemplateDefinition(
      uriTemplate = uriTemplate,
      name = templateName,
      description = templateDescription,
      mimeType = templateMimeType,
    )
    val handler = f
    new McpResourceTemplateHandler:
      def definition: ResourceTemplateDefinition = tmplDef
      def read(uri: String): ZIO[Any, ToolError, Chunk[ResourceContents]] = handler(uri)

object McpResourceTemplate:
  def apply(uriTemplate: String, name: String): McpResourceTemplate =
    new McpResourceTemplate(uriTemplate, name, None, None)
