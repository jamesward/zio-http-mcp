package com.jamesward.ziohttp.mcp

import zio.*
import zio.http.*
import zio.json.ast.Json
import zio.schema.*
import zio.test.*
import zio.test.TestAspect.*

import org.testcontainers.Testcontainers as TC
import org.testcontainers.containers.GenericContainer
import org.testcontainers.containers.output.ToStringConsumer
import org.testcontainers.containers.startupcheck.OneShotStartupCheckStrategy
import org.testcontainers.images.builder.ImageFromDockerfile

import java.time.Duration as JDuration
import java.util.Base64

object ConformanceSpec extends ZIOSpecDefault:

  // --- Conformance tools ---

  // Minimal 1x1 red pixel PNG (base64)
  private val minimalPng: String =
    Base64.getEncoder.encodeToString(Array[Byte](
      0x89.toByte, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a, // PNG signature
      0x00, 0x00, 0x00, 0x0d, 0x49, 0x48, 0x44, 0x52, // IHDR chunk
      0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01, // 1x1
      0x08, 0x02, 0x00, 0x00, 0x00, 0x90.toByte, 0x77, 0x53, // 8-bit RGB
      0xde.toByte, 0x00, 0x00, 0x00, 0x0c, 0x49, 0x44, 0x41, // IDAT chunk
      0x54, 0x08, 0xd7.toByte, 0x63, 0xf8.toByte, 0xcf.toByte,
      0xc0.toByte, 0x00, 0x00, 0x00, 0x02, 0x00, 0x01, 0xe2.toByte,
      0x21, 0xbc.toByte, 0x33, 0x00, 0x00, 0x00, 0x00, 0x49, // IEND chunk
      0x45, 0x4e, 0x44, 0xae.toByte, 0x42, 0x60, 0x82.toByte,
    ))

  // Minimal WAV header (silence, 1 sample)
  private val minimalWav: String =
    val header = Array[Byte](
      0x52, 0x49, 0x46, 0x46, // "RIFF"
      0x24, 0x00, 0x00, 0x00, // chunk size (36 bytes)
      0x57, 0x41, 0x56, 0x45, // "WAVE"
      0x66, 0x6d, 0x74, 0x20, // "fmt "
      0x10, 0x00, 0x00, 0x00, // subchunk size (16)
      0x01, 0x00,             // PCM
      0x01, 0x00,             // mono
      0x44, 0xac.toByte, 0x00, 0x00, // 44100 Hz
      0x44, 0xac.toByte, 0x00, 0x00, // byte rate
      0x01, 0x00,             // block align
      0x08, 0x00,             // 8 bits per sample
      0x64, 0x61, 0x74, 0x61, // "data"
      0x00, 0x00, 0x00, 0x00, // data size (0)
    )
    Base64.getEncoder.encodeToString(header)

  case class EmptyInput() derives Schema

  val testSimpleText: McpToolHandler = McpTool("test_simple_text")
    .description("Returns simple text for testing")
    .handleDirect[EmptyInput]: _ =>
      ZIO.succeed(Chunk(ToolContent.text("This is a simple text response for testing.")))

  val testImageContent: McpToolHandler = McpTool("test_image_content")
    .description("Returns image content for testing")
    .handleDirect[EmptyInput]: _ =>
      ZIO.succeed(Chunk(ToolContent.image(minimalPng, "image/png")))

  val testAudioContent: McpToolHandler = McpTool("test_audio_content")
    .description("Returns audio content for testing")
    .handleDirect[EmptyInput]: _ =>
      ZIO.succeed(Chunk(ToolContent.audio(minimalWav, "audio/wav")))

  val testEmbeddedResource: McpToolHandler = McpTool("test_embedded_resource")
    .description("Returns embedded resource content for testing")
    .handleDirect[EmptyInput]: _ =>
      ZIO.succeed(Chunk(ToolContent.embeddedResource(
        ResourceContents(
          uri = "test://embedded-resource",
          mimeType = Some("text/plain"),
          text = Some("This is an embedded resource content."),
        )
      )))

  val testMultipleContentTypes: McpToolHandler = McpTool("test_multiple_content_types")
    .description("Returns multiple content types for testing")
    .handleDirect[EmptyInput]: _ =>
      ZIO.succeed(Chunk(
        ToolContent.text("Multiple content types test:"),
        ToolContent.image(minimalPng, "image/png"),
        ToolContent.embeddedResource(ResourceContents(
          uri = "test://mixed-content-resource",
          mimeType = Some("application/json"),
          text = Some("""{"test":"data","value":123}"""),
        )),
      ))

  val testErrorHandling: McpToolHandler = McpTool("test_error_handling")
    .description("Always returns an error for testing")
    .handleDirect[EmptyInput]: _ =>
      ZIO.fail(ToolError("This tool intentionally returns an error for testing"))

  val testToolWithLogging: McpToolHandler = McpTool("test_tool_with_logging")
    .description("Tool that emits log notifications during execution")
    .handleDirectWithContext[EmptyInput]: (_, ctx) =>
      for
        _ <- ctx.log(com.jamesward.ziohttp.mcp.LogLevel.Info, "Tool execution started")
        _ <- ZIO.sleep(50.millis)
        _ <- ctx.log(com.jamesward.ziohttp.mcp.LogLevel.Info, "Tool processing data")
        _ <- ZIO.sleep(50.millis)
        _ <- ctx.log(com.jamesward.ziohttp.mcp.LogLevel.Info, "Tool execution completed")
      yield Chunk(ToolContent.text("Logging test completed successfully."))

  val testToolWithProgress: McpToolHandler = McpTool("test_tool_with_progress")
    .description("Tool that emits progress notifications during execution")
    .handleDirectWithContext[EmptyInput]: (_, ctx) =>
      for
        _ <- ctx.progress(0, 100)
        _ <- ZIO.sleep(50.millis)
        _ <- ctx.progress(50, 100)
        _ <- ZIO.sleep(50.millis)
        _ <- ctx.progress(100, 100)
      yield Chunk(ToolContent.text("Progress test completed successfully."))

  case class PromptInput(prompt: String) derives Schema
  case class MessageInput(message: String) derives Schema

  val testSampling: McpToolHandler = McpTool("test_sampling")
    .description("Tool that tests sampling capability")
    .handleDirectWithContext[PromptInput]: (input, ctx) =>
      ctx.sample(input.prompt, 100).map: result =>
        val responseText = result.content match
          case ToolContent.Text(text, _) => text
          case _ => ""
        Chunk(ToolContent.text(s"LLM response: $responseText"))

  val testElicitation: McpToolHandler = McpTool("test_elicitation")
    .description("Tool that tests elicitation capability")
    .handleDirectWithContext[MessageInput]: (input, ctx) =>
      val schema = Json.Obj(Chunk(
        "type" -> Json.Str("object"),
        "properties" -> Json.Obj(Chunk(
          "username" -> Json.Obj(Chunk("type" -> Json.Str("string"))),
          "email" -> Json.Obj(Chunk("type" -> Json.Str("string"))),
        )),
        "required" -> Json.Arr(Chunk(Json.Str("username"), Json.Str("email"))),
      ))
      ctx.elicit(input.message, schema).map: result =>
        Chunk(ToolContent.text(s"User response: action=${result.action}, content=${result.content.getOrElse(Map.empty)}"))

  val testElicitationSep1034Defaults: McpToolHandler = McpTool("test_elicitation_sep1034_defaults")
    .description("Tool that tests elicitation with default values")
    .handleDirectWithContext[EmptyInput]: (_, ctx) =>
      val schema = Json.Obj(Chunk(
        "type" -> Json.Str("object"),
        "properties" -> Json.Obj(Chunk(
          "name" -> Json.Obj(Chunk("type" -> Json.Str("string"), "default" -> Json.Str("John Doe"))),
          "age" -> Json.Obj(Chunk("type" -> Json.Str("integer"), "default" -> Json.Num(30))),
          "score" -> Json.Obj(Chunk("type" -> Json.Str("number"), "default" -> Json.Num(95.5))),
          "status" -> Json.Obj(Chunk(
            "type" -> Json.Str("string"),
            "enum" -> Json.Arr(Chunk(Json.Str("active"), Json.Str("inactive"), Json.Str("pending"))),
            "default" -> Json.Str("active"),
          )),
          "verified" -> Json.Obj(Chunk("type" -> Json.Str("boolean"), "default" -> Json.Bool(true))),
        )),
      ))
      ctx.elicit("Please provide your information", schema).map: result =>
        Chunk(ToolContent.text(s"Elicitation completed: action=${result.action}, content=${result.content.getOrElse(Map.empty)}"))

  val testElicitationSep1330Enums: McpToolHandler = McpTool("test_elicitation_sep1330_enums")
    .description("Tool that tests elicitation with enum schemas")
    .handleDirectWithContext[EmptyInput]: (_, ctx) =>
      val schema = Json.Obj(Chunk(
        "type" -> Json.Str("object"),
        "properties" -> Json.Obj(Chunk(
          "untitledSingle" -> Json.Obj(Chunk(
            "type" -> Json.Str("string"),
            "enum" -> Json.Arr(Chunk(Json.Str("option1"), Json.Str("option2"), Json.Str("option3"))),
          )),
          "titledSingle" -> Json.Obj(Chunk(
            "type" -> Json.Str("string"),
            "oneOf" -> Json.Arr(Chunk(
              Json.Obj(Chunk("const" -> Json.Str("value1"), "title" -> Json.Str("First Option"))),
              Json.Obj(Chunk("const" -> Json.Str("value2"), "title" -> Json.Str("Second Option"))),
              Json.Obj(Chunk("const" -> Json.Str("value3"), "title" -> Json.Str("Third Option"))),
            )),
          )),
          "legacyEnum" -> Json.Obj(Chunk(
            "type" -> Json.Str("string"),
            "enum" -> Json.Arr(Chunk(Json.Str("opt1"), Json.Str("opt2"), Json.Str("opt3"))),
            "enumNames" -> Json.Arr(Chunk(Json.Str("Option One"), Json.Str("Option Two"), Json.Str("Option Three"))),
          )),
          "untitledMulti" -> Json.Obj(Chunk(
            "type" -> Json.Str("array"),
            "items" -> Json.Obj(Chunk(
              "type" -> Json.Str("string"),
              "enum" -> Json.Arr(Chunk(Json.Str("option1"), Json.Str("option2"), Json.Str("option3"))),
            )),
          )),
          "titledMulti" -> Json.Obj(Chunk(
            "type" -> Json.Str("array"),
            "items" -> Json.Obj(Chunk(
              "anyOf" -> Json.Arr(Chunk(
                Json.Obj(Chunk("const" -> Json.Str("value1"), "title" -> Json.Str("First Choice"))),
                Json.Obj(Chunk("const" -> Json.Str("value2"), "title" -> Json.Str("Second Choice"))),
                Json.Obj(Chunk("const" -> Json.Str("value3"), "title" -> Json.Str("Third Choice"))),
              )),
            )),
          )),
        )),
      ))
      ctx.elicit("Please select options", schema).map: result =>
        Chunk(ToolContent.text(s"Elicitation completed: action=${result.action}, content=${result.content.getOrElse(Map.empty)}"))

  // JSON Schema 2020-12 tool — raw schema preserving $schema, $defs, $ref, additionalProperties
  val jsonSchema202012Tool: McpToolHandler = McpTool("json_schema_2020_12_tool")
    .description("Tool with JSON Schema 2020-12 features")
    .withRawInputSchema(Json.Obj(Chunk(
      "$schema" -> Json.Str("https://json-schema.org/draft/2020-12/schema"),
      "type" -> Json.Str("object"),
      "$defs" -> Json.Obj(Chunk(
        "address" -> Json.Obj(Chunk(
          "type" -> Json.Str("object"),
          "properties" -> Json.Obj(Chunk(
            "street" -> Json.Obj(Chunk("type" -> Json.Str("string"))),
            "city" -> Json.Obj(Chunk("type" -> Json.Str("string"))),
          )),
        )),
      )),
      "properties" -> Json.Obj(Chunk(
        "name" -> Json.Obj(Chunk("type" -> Json.Str("string"))),
        "address" -> Json.Obj(Chunk("$ref" -> Json.Str("#/$defs/address"))),
      )),
      "additionalProperties" -> Json.Bool(false),
    )))
    .handleDirect: args =>
      ZIO.succeed(Chunk(ToolContent.text("JSON Schema 2020-12 tool called successfully.")))

  // --- Conformance resources ---

  val staticTextResource: McpResourceHandler = McpResource("test://static-text", "Static Text")
    .description("A static text resource")
    .mimeType("text/plain")
    .read: uri =>
      ZIO.succeed(Chunk(ResourceContents(
        uri = uri,
        mimeType = Some("text/plain"),
        text = Some("This is the content of the static text resource."),
      )))

  val staticBinaryResource: McpResourceHandler = McpResource("test://static-binary", "Static Binary")
    .description("A static binary resource")
    .mimeType("image/png")
    .read: uri =>
      ZIO.succeed(Chunk(ResourceContents(
        uri = uri,
        mimeType = Some("image/png"),
        blob = Some(minimalPng),
      )))

  val watchedResource: McpResourceHandler = McpResource("test://watched-resource", "Watched Resource")
    .description("A watched resource for subscription testing")
    .mimeType("text/plain")
    .read: uri =>
      ZIO.succeed(Chunk(ResourceContents(
        uri = uri,
        mimeType = Some("text/plain"),
        text = Some("Watched resource content."),
      )))

  val templateResource: McpResourceTemplateHandler = McpResourceTemplate("test://template/{id}/data", "Template Data")
    .description("A template resource")
    .mimeType("application/json")
    .read: uri =>
      val id = uri.stripPrefix("test://template/").stripSuffix("/data")
      ZIO.succeed(Chunk(ResourceContents(
        uri = uri,
        mimeType = Some("application/json"),
        text = Some(s"""{"id":"$id","templateTest":true,"data":"Data for ID: $id"}"""),
      )))

  // --- Conformance prompts ---

  val testSimplePrompt: McpPromptHandler = McpPrompt("test_simple_prompt")
    .description("Simple prompt for testing")
    .get: _ =>
      ZIO.succeed(PromptGetResult(
        messages = Chunk(PromptMessage(
          role = "user",
          content = ToolContent.text("This is a simple prompt for testing."),
        )),
      ))

  val testPromptWithArguments: McpPromptHandler = McpPrompt("test_prompt_with_arguments")
    .description("Parameterized prompt for testing")
    .argument("arg1", "First test argument")
    .argument("arg2", "Second test argument")
    .get: args =>
      val arg1 = args.getOrElse("arg1", "")
      val arg2 = args.getOrElse("arg2", "")
      ZIO.succeed(PromptGetResult(
        messages = Chunk(PromptMessage(
          role = "user",
          content = ToolContent.text(s"Arguments received: arg1=$arg1, arg2=$arg2"),
        )),
      ))

  val testPromptWithEmbeddedResource: McpPromptHandler = McpPrompt("test_prompt_with_embedded_resource")
    .description("Prompt with embedded resource for testing")
    .argument("resourceUri", "URI of resource to embed")
    .get: args =>
      val resourceUri = args.getOrElse("resourceUri", "test://static-text")
      ZIO.succeed(PromptGetResult(
        messages = Chunk(
          PromptMessage(
            role = "user",
            content = ToolContent.embeddedResource(ResourceContents(
              uri = resourceUri,
              mimeType = Some("text/plain"),
              text = Some("This is embedded resource content."),
            )),
          ),
          PromptMessage(
            role = "user",
            content = ToolContent.text("Please process the embedded resource above."),
          ),
        ),
      ))

  val testPromptWithImage: McpPromptHandler = McpPrompt("test_prompt_with_image")
    .description("Prompt with image for testing")
    .get: _ =>
      ZIO.succeed(PromptGetResult(
        messages = Chunk(
          PromptMessage(
            role = "user",
            content = ToolContent.image(minimalPng, "image/png"),
          ),
          PromptMessage(
            role = "user",
            content = ToolContent.text("Please analyze the image above."),
          ),
        ),
      ))

  val testServer = McpServer("test-server", "0.1.0")
    .tool(testSimpleText)
    .tool(testImageContent)
    .tool(testAudioContent)
    .tool(testEmbeddedResource)
    .tool(testMultipleContentTypes)
    .tool(testErrorHandling)
    .tool(testToolWithLogging)
    .tool(testToolWithProgress)
    .tool(testSampling)
    .tool(testElicitation)
    .tool(testElicitationSep1034Defaults)
    .tool(testElicitationSep1330Enums)
    .tool(jsonSchema202012Tool)
    .resource(staticTextResource)
    .resource(staticBinaryResource)
    .resource(watchedResource)
    .resourceTemplate(templateResource)
    .prompt(testSimplePrompt)
    .prompt(testPromptWithArguments)
    .prompt(testPromptWithEmbeddedResource)
    .prompt(testPromptWithImage)

  val conformanceImage: ImageFromDockerfile =
    ImageFromDockerfile("mcp-conformance", false)
      .withDockerfileFromBuilder: builder =>
        builder
          .from("node:22-slim")
          .run("npm install -g @modelcontextprotocol/conformance")
          .entryPoint("npx", "@modelcontextprotocol/conformance")
          .build()

  def runConformance(port: Int): Task[(Long, String)] =
    ZIO.attemptBlocking:
      TC.exposeHostPorts(port)

      val stdout = ToStringConsumer()
      val container = GenericContainer(conformanceImage)
      container.withAccessToHost(true)
      container.withExtraHost("localhost", "host-gateway")
      container.withCommand(
        "server",
        "--url", s"http://localhost:$port/mcp",
      )
      container.withStartupCheckStrategy(
        OneShotStartupCheckStrategy().withTimeout(JDuration.ofSeconds(60))
      )
      container.withLogConsumer(stdout)

      try
        container.start()
        val exitCode: Long = container.getContainerInfo.getState.getExitCodeLong
        (exitCode, stdout.toUtf8String)
      catch
        case _: org.testcontainers.containers.ContainerLaunchException =>
          val exitCode: Long =
            try container.getContainerInfo.getState.getExitCodeLong
            catch case _: Exception => -1L
          (exitCode, stdout.toUtf8String)
      finally
        try container.stop()
        catch case _: Exception => ()

  override def spec =
    suite("MCP Conformance")(
      test("conformance test suite passes"):
        for
          port              <- Server.install(testServer.routes)
          _                 <- ZIO.logInfo(s"MCP server started on port $port")
          (exitCode, output) <- runConformance(port)
          _                 <- ZIO.logInfo(s"Conformance exit code: $exitCode")
          _                 <- ZIO.logInfo(s"Conformance output:\n$output")
        yield assertTrue(
          exitCode == 0L,
        )
    ).provide(Server.defaultWith(_.onAnyOpenPort)) @@
      withLiveClock @@
      timeout(2.minutes) @@
      sequential
