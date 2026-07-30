package com.jamesward.ziohttp.mcp

import zio.*
import zio.http.*
import zio.json.ast.Json
import zio.schema.*
import zio.test.*
import zio.test.TestAspect.*

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

  val testSimpleText: McpToolHandler = McpTool("test_simple_text")
    .description("Returns simple text for testing")
    .handle:
      ZIO.succeed("This is a simple text response for testing.")

  val testImageContent: McpToolHandler = McpTool("test_image_content")
    .description("Returns image content for testing")
    .handle:
      ZIO.succeed(ToolContent.image(minimalPng, "image/png"))

  val testAudioContent: McpToolHandler = McpTool("test_audio_content")
    .description("Returns audio content for testing")
    .handle:
      ZIO.succeed(ToolContent.audio(minimalWav, "audio/wav"))

  val testEmbeddedResource: McpToolHandler = McpTool("test_embedded_resource")
    .description("Returns embedded resource content for testing")
    .handle:
      ZIO.succeed(ToolContent.embeddedResource(
        ResourceContents(
          uri = "test://embedded-resource",
          mimeType = Some("text/plain"),
          text = Some("This is an embedded resource content."),
        )
      ))

  val testMultipleContentTypes: McpToolHandler = McpTool("test_multiple_content_types")
    .description("Returns multiple content types for testing")
    .handle:
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
    .handle[Any, ToolError, String]:
      ZIO.fail(ToolError("This tool intentionally returns an error for testing"))

  val testToolWithLogging: McpToolHandler = McpTool("test_tool_with_logging")
    .description("Tool that emits log notifications during execution")
    .handleWithContext: ctx =>
      for
        _ <- ctx.log(com.jamesward.ziohttp.mcp.LogLevel.Info, "Tool execution started")
        _ <- ZIO.sleep(50.millis)
        _ <- ctx.log(com.jamesward.ziohttp.mcp.LogLevel.Info, "Tool processing data")
        _ <- ZIO.sleep(50.millis)
        _ <- ctx.log(com.jamesward.ziohttp.mcp.LogLevel.Info, "Tool execution completed")
      yield Chunk(ToolContent.text("Logging test completed successfully."))

  val testToolWithProgress: McpToolHandler = McpTool("test_tool_with_progress")
    .description("Tool that emits progress notifications during execution")
    .handleWithContext: ctx =>
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
    .handleWithContext[Any, ToolError, PromptInput, Chunk[ToolContent]]: (input, ctx) =>
      ctx.sample(input.prompt, 100).map: result =>
        val responseText = result.content match
          case ToolContent.Text(text, _) => text
          case _ => ""
        Chunk(ToolContent.text(s"LLM response: $responseText"))

  val testElicitation: McpToolHandler = McpTool("test_elicitation")
    .description("Tool that tests elicitation capability")
    .handleWithContext[Any, ToolError, MessageInput, Chunk[ToolContent]]: (input, ctx) =>
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
    .handleWithContext[Any, ToolError, Chunk[ToolContent]]: ctx =>
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
    .handleWithContext[Any, ToolError, Chunk[ToolContent]]: ctx =>
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
  private given McpInput[Option[Json.Obj]] = McpInput.raw(Json.Obj(Chunk(
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

  val jsonSchema202012Tool: McpToolHandler = McpTool("json_schema_2020_12_tool")
    .description("Tool with JSON Schema 2020-12 features")
    .handle: (_: Option[Json.Obj]) =>
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
          role = Role.User,
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
          role = Role.User,
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
            role = Role.User,
            content = ToolContent.embeddedResource(ResourceContents(
              uri = resourceUri,
              mimeType = Some("text/plain"),
              text = Some("This is embedded resource content."),
            )),
          ),
          PromptMessage(
            role = Role.User,
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
            role = Role.User,
            content = ToolContent.image(minimalPng, "image/png"),
          ),
          PromptMessage(
            role = Role.User,
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

  // No baselined failures: the kit runs on the host against a real `localhost`
  // URL, so `dns-rebinding-protection` — which previously had to be baselined
  // because rootless Docker could not reach the host's localhost — is exercised
  // for real. Modern `tools/call` likewise streams request-scoped
  // `notifications/progress` / `notifications/message` over the SSE response
  // stream when the request opts in, so `tools-call-with-progress` passes too.
  private val expectedFailuresYaml: String = "{}\n"

  // The official MCP conformance kit (npm). The `latest` line (0.1.x) drives the
  // 2025-11-25 protocol; the `0.2.0` line drives the modern 2026-07-28 protocol
  // (`0.2.0-alpha.10` is the release aligned with the finalized 2026-07-28 spec;
  // move to the stable `0.2.0` once it ships). Our server is dual-era, so each
  // version exercises a different negotiated path against it. The name and
  // version are kept separate and joined with `@` only when building the
  // `npx` invocation.
  private val ConformancePackage = "@modelcontextprotocol/conformance"
  private val LegacyKitVersion   = "0.1.16"
  private val ModernKitVersion   = "0.2.0-alpha.10"

  /**
   * Run the kit against the server bound on `port`, as a host process via `npx`.
   *
   * The kit used to run in a testcontainer, which meant reaching the server over
   * `host.testcontainers.internal` and baselining `dns-rebinding-protection` as an
   * expected failure, since rootless Docker cannot reach the host's `localhost`.
   * Running it on the host removes both the Docker dependency and that caveat, and
   * matches how [[ClientConformanceSpec]] drives the kit's client mode.
   */
  def runConformance(kitVersion: String, port: Int, specVersion: String, expectedFailures: String, scenario: Option[String] = None): Task[(Int, String)] =
    ZIO.attemptBlocking:
      val expectedFailuresFile = java.io.File.createTempFile("expected-failures", ".yaml")
      expectedFailuresFile.deleteOnExit()
      java.nio.file.Files.writeString(expectedFailuresFile.toPath, expectedFailures)

      // The kit writes a `results/` directory into its working directory; keep
      // that out of the repo.
      val workDir = java.nio.file.Files.createTempDirectory("mcp-conformance").toFile
      workDir.deleteOnExit()

      val baseArgs = Seq(
        "npx", "-y", s"$ConformancePackage@$kitVersion",
        "server",
        "--url", s"http://localhost:$port/mcp",
        // `--spec-version` picks which protocol era the kit drives: without it
        // the kit defaults to the legacy `initialize` handshake even on the
        // 2026-07-28 line, so it must be set explicitly to actually exercise the
        // modern (server/discover + per-request `_meta`) wire protocol.
        "--spec-version", specVersion,
        "--expected-failures", expectedFailuresFile.getAbsolutePath,
      )
      val args = scenario.fold(baseArgs)(s => baseArgs ++ Seq("--scenario", s))

      val builder = ProcessBuilder(args*)
      builder.directory(workDir)
      builder.redirectErrorStream(true)
      val process = builder.start()
      val output  = String(process.getInputStream.readAllBytes())
      // The kit runs ~30 scenarios in one shot; allow generous headroom so a
      // slow npm fetch does not time the whole run out mid-suite.
      val exited  = process.waitFor(300, java.util.concurrent.TimeUnit.SECONDS)
      if !exited then process.destroyForcibly()
      ((if exited then process.exitValue() else -1), output)

  override def spec =
    suite("MCP Conformance")(
      test("2025-11-25 (legacy) conformance suite passes"):
        for
          port              <- Server.install(testServer.routes)
          _                 <- ZIO.logInfo(s"MCP server started on port $port")
          (exitCode, output) <- runConformance(LegacyKitVersion, port, "2025-11-25", expectedFailuresYaml)
          _                 <- ZIO.logInfo(s"Legacy conformance exit code: $exitCode")
          _                 <- ZIO.logInfo(s"Legacy conformance output:\n$output").when(exitCode != 0)
        yield assertTrue(exitCode == 0)
      ,
      test("2026-07-28 (modern) conformance suite passes"):
        for
          port              <- Server.install(testServer.routes)
          (exitCode, output) <- runConformance(ModernKitVersion, port, "2026-07-28", expectedFailuresYaml)
          _                 <- ZIO.logInfo(s"Modern conformance exit code: $exitCode")
          _                 <- ZIO.logInfo(s"Modern conformance output:\n$output").when(exitCode != 0)
        yield assertTrue(exitCode == 0)
    ).provide(Server.defaultWith(_.onAnyOpenPort), McpServer.State.default) @@
      withLiveClock @@
      timeout(5.minutes) @@
      sequential
