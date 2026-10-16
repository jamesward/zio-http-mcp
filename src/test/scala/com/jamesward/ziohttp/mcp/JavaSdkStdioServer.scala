package com.jamesward.ziohttp.mcp

import io.modelcontextprotocol.json.McpJsonDefaults
import io.modelcontextprotocol.json.schema.JsonSchemaValidator
import io.modelcontextprotocol.server.McpServer as JavaMcpServer
import io.modelcontextprotocol.server.transport.StdioServerTransportProvider
import io.modelcontextprotocol.spec.McpSchema

/**
 * An MCP server built with the official Java SDK on its stdio transport,
 * launched as a subprocess by `McpClientStdioSpec` for interop. The SDK speaks
 * only the legacy (`initialize`) revisions. It doesn't exit when its stdin
 * closes, so it also exercises the client's forced shutdown.
 */
object JavaSdkStdioServer:
  def main(args: Array[String]): Unit =
    val mapper = McpJsonDefaults.getMapper()
    val schema = """{"type":"object","properties":{"text":{"type":"string"}},"required":["text"]}"""
    val transport = StdioServerTransportProvider(mapper)
    JavaMcpServer
      .sync(transport)
      .serverInfo("java-sdk-stdio", "1.0.0")
      .capabilities(McpSchema.ServerCapabilities.builder().tools(true).build())
      // the test classpath resolves a newer networknt json-schema-validator than the SDK's default validator links
      // against, so validation is off; it is beside the point of a transport interop test
      .jsonSchemaValidator((_, _) => JsonSchemaValidator.ValidationResponse.asValid(null))
      .toolCall(
        McpSchema.Tool.builder("echo", mapper, schema).description("Echoes its text").build(),
        (_, request) => McpSchema.CallToolResult.builder().addTextContent(s"java echo: ${request.arguments().get("text")}").build(),
      )
      .build()
    // the transport reads stdin on its own threads; keep the JVM up until it reaches end of file
    Thread.currentThread().join()
