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

import java.nio.file.{Files, Path, Paths}
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

  // --- MRTR fixtures (SEP-2322) ---
  //
  // The kit's `input-required-result-*` scenarios drive named tools through the
  // `input_required` flow, so the names, the input-request keys, and (for the
  // request-state scenarios) the "state-ok" marker are fixed by the suite —
  // see the scenario descriptions in `conformance list --verbose`.

  private def objectSchema(property: String, propertyType: String): Json.Obj =
    Json.Obj(Chunk(
      "type" -> Json.Str("object"),
      "properties" -> Json.Obj(Chunk(property -> Json.Obj(Chunk("type" -> Json.Str(propertyType))))),
      "required" -> Json.Arr(Chunk(Json.Str(property))),
    ))

  private val nameSchema    = objectSchema("name", "string")
  private val colorSchema   = objectSchema("color", "string")
  private val okSchema      = objectSchema("ok", "boolean")
  private val contextSchema = objectSchema("context", "string")

  private def elicited(result: ElicitationResult, field: String, fallback: String): String =
    result.content.flatMap(_.get(field)).flatMap(_.asString).getOrElse(fallback)

  private def sampledText(result: SamplingResult): String =
    result.content match
      case ToolContent.Text(text, _) => text
      case _                         => ""

  val testInputRequiredElicitation: McpToolHandler = McpTool("test_input_required_result_elicitation")
    .description("Asks for a name via elicitation, then greets it")
    .handleWithContext[Any, ToolError, Chunk[ToolContent]]: ctx =>
      ctx.elicit("user_name", "What is your name?", nameSchema).map: result =>
        Chunk(ToolContent.text(s"Hello, ${elicited(result, "name", "stranger")}!"))

  val testInputRequiredSampling: McpToolHandler = McpTool("test_input_required_result_sampling")
    .description("Asks the client's model a question via sampling")
    .handleWithContext[Any, ToolError, Chunk[ToolContent]]: ctx =>
      ctx.sample("capital_question", "What is the capital of France?", 100).map: result =>
        Chunk(ToolContent.text(s"The model said: ${sampledText(result)}"))

  val testInputRequiredListRoots: McpToolHandler = McpTool("test_input_required_result_list_roots")
    .description("Asks the client for its roots")
    .handleWithContext[Any, ToolError, Chunk[ToolContent]]: ctx =>
      ctx.listRoots("client_roots").map: roots =>
        Chunk(ToolContent.text(s"Client roots: ${roots.map(_.uri).mkString(", ")}"))

  val testInputRequiredRequestState: McpToolHandler = McpTool("test_input_required_result_request_state")
    .description("Round-trips opaque requestState alongside an elicitation")
    .handleWithContext[Any, ToolError, Chunk[ToolContent]]: ctx =>
      ctx.setRequestState("awaiting-confirmation") *>
        ctx.elicit("confirm", "Please confirm", okSchema).map: result =>
          // Reached only on the retry, which carries the state back; saying so
          // is what the scenario checks.
          val marker = if ctx.requestState.contains("awaiting-confirmation") then "state-ok" else "state-missing"
          Chunk(ToolContent.text(s"$marker (${result.action})"))

  val testInputRequiredMultipleInputs: McpToolHandler = McpTool("test_input_required_result_multiple_inputs")
    .description("Asks for an elicitation, a sampling and the client's roots in one round trip")
    .handleWithContext[Any, ToolError, Chunk[ToolContent]]: ctx =>
      ctx.setRequestState("collecting-inputs") *>
        ctx.inputs(
          InputSpec.elicit("user_name", "What is your name?", nameSchema),
          InputSpec.sample("greeting", "Generate a greeting", 50),
          InputSpec.listRoots("client_roots"),
        ).map: results =>
          val name  = elicited(results.elicitation("user_name"), "name", "stranger")
          val roots = results.roots("client_roots").map(_.uri).mkString(", ")
          Chunk(ToolContent.text(s"${sampledText(results.sampling("greeting"))} $name (roots: $roots)"))

  val testInputRequiredMultiRound: McpToolHandler = McpTool("test_input_required_result_multi_round")
    .description("Collects a name, then a colour, over two rounds of evolving requestState")
    .handleWithContext[Any, ToolError, Chunk[ToolContent]]: ctx =>
      val askColour = ctx.elicit("step2", "Step 2: What is your favorite color?", colorSchema)
      ctx.requestState match
        case Some("round-2") =>
          askColour.map(result => Chunk(ToolContent.text(s"Favourite colour: ${elicited(result, "color", "none")}")))
        case Some("round-1") =>
          ctx.setRequestState("round-2") *> askColour.map(_ => Chunk(ToolContent.text("colour requested")))
        case _ =>
          ctx.setRequestState("round-1") *>
            ctx.elicit("step1", "Step 1: What is your name?", nameSchema)
              .map(_ => Chunk(ToolContent.text("name requested")))

  val testInputRequiredTamperedState: McpToolHandler = McpTool("test_input_required_result_tampered_state")
    .description("Issues integrity-protected requestState; the server rejects it if edited")
    .handleWithContext[Any, ToolError, Chunk[ToolContent]]: ctx =>
      ctx.setRequestState("integrity-protected") *>
        ctx.elicit("confirm", "Please confirm", okSchema).map: result =>
          Chunk(ToolContent.text(s"confirmed: ${result.action}"))

  val testInputRequiredCapabilities: McpToolHandler = McpTool("test_input_required_result_capabilities")
    .description("Only asks for input the client declared it can answer")
    .handleWithContext[Any, ToolError, Chunk[ToolContent]]: ctx =>
      val specs = Chunk(
        Option.when(ctx.clientSupports("elicitation"))(InputSpec.elicit("user_name", "What is your name?", nameSchema)),
        Option.when(ctx.clientSupports("sampling"))(InputSpec.sample("greeting", "Generate a greeting", 50)),
      ).flatten
      ctx.inputs(specs*).map: results =>
        val name = results.json("user_name").fold("not asked")(_ => elicited(results.elicitation("user_name"), "name", "stranger"))
        val greeting = results.json("greeting").fold("not asked")(_ => sampledText(results.sampling("greeting")))
        Chunk(ToolContent.text(s"greeting=$greeting, name=$name"))

  val testInputRequiredPrompt: McpPromptHandler = McpPrompt("test_input_required_result_prompt")
    .description("Prompt that elicits its context before it renders")
    .getWithContext: (_, ctx) =>
      ctx.elicit("user_context", "What context should the prompt use?", contextSchema).map: result =>
        PromptGetResult(
          messages = Chunk(PromptMessage(
            role = Role.User,
            content = ToolContent.text(s"Context: ${elicited(result, "context", "none")}"),
          )),
        )

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
    .tool(testInputRequiredElicitation)
    .tool(testInputRequiredSampling)
    .tool(testInputRequiredListRoots)
    .tool(testInputRequiredRequestState)
    .tool(testInputRequiredMultipleInputs)
    .tool(testInputRequiredMultiRound)
    .tool(testInputRequiredTamperedState)
    .tool(testInputRequiredCapabilities)
    .prompt(testInputRequiredPrompt)

  /**
   * A CA bundle to trust inside the image build, taken from the standard
   * environment variables. On an ordinary machine none of these are set and the
   * image is built unchanged; behind a TLS-intercepting proxy (corporate network,
   * sandboxed CI) they point at the proxy's bundle, without which `npm install`
   * fails the build with `SELF_SIGNED_CERT_IN_CHAIN`.
   */
  private val hostCaBundle: Option[Path] =
    List("NODE_EXTRA_CA_CERTS", "SSL_CERT_FILE", "CURL_CA_BUNDLE", "REQUESTS_CA_BUNDLE")
      .flatMap(sys.env.get)
      .map(Paths.get(_))
      .find(Files.isRegularFile(_))

  /**
   * Run the kit's container in the host network namespace, so it reaches the
   * server under test at a genuine `localhost` URL. That matters beyond
   * convenience: `dns-rebinding-protection` asserts the server's `Origin`
   * handling for localhost callers and cannot run over
   * `host.testcontainers.internal`.
   *
   * Host networking is a Linux capability (and Docker Desktop 4.34+ with the
   * feature enabled). Elsewhere we fall back to bridged networking, where that
   * one scenario is baselined as an expected failure. Force either mode with
   * `CONFORMANCE_HOST_NETWORK=true|false`.
   */
  private val useHostNetwork: Boolean =
    sys.env.get("CONFORMANCE_HOST_NETWORK")
      .map(_.equalsIgnoreCase("true"))
      .getOrElse(java.lang.System.getProperty("os.name", "").toLowerCase.contains("linux"))

  /**
   * Bridged networking cannot give the kit a localhost URL, so
   * `dns-rebinding-protection` is baselined in that mode only. Under host
   * networking every scenario is expected to pass.
   *
   * Modern `tools/call` streams request-scoped `notifications/progress` /
   * `notifications/message` over the SSE response stream when the request opts
   * in, so `tools-call-with-progress` passes in both modes.
   */
  private val expectedFailuresYaml: String =
    if useHostNetwork then "{}\n"
    else
      """server:
        |  - dns-rebinding-protection
        |""".stripMargin

  // The official MCP conformance kit (npm). The `latest` line (0.1.x) drives the
  // 2025-11-25 protocol; the `0.2.0` line drives the modern 2026-07-28 protocol
  // (move to the stable `0.2.0` once it ships). Our server is dual-era, so each
  // version exercises a different negotiated path against it. The name and
  // version are kept separate and joined with `@` only when building the
  // `npx` invocation.
  private val ConformancePackage = "@modelcontextprotocol/conformance"
  private val LegacyKitVersion   = "0.1.16"
  private val ModernKitVersion   = "0.2.0-alpha.11"

  /**
   * Build an image with the kit preinstalled, so the test needs Docker and nothing
   * else on the host — no Node, no npm, no global installs.
   */
  def conformanceImage(version: String, tag: String): ImageFromDockerfile =
    val base = ImageFromDockerfile(s"mcp-conformance-$tag", false)
    // The CA has to be baked in rather than mounted: `npm install` runs during the
    // image build, where volumes are not available.
    val withContext = hostCaBundle.fold(base)(ca => base.withFileFromPath("ca-bundle.crt", ca))
    withContext.withDockerfileFromBuilder: builder =>
      builder.from("node:22-slim")
      hostCaBundle.foreach: _ =>
        builder.copy("ca-bundle.crt", "/usr/local/share/ca-certificates/proxy-ca.crt")
        builder.env("NODE_EXTRA_CA_CERTS", "/usr/local/share/ca-certificates/proxy-ca.crt")
      builder
        .run(s"npm install -g $ConformancePackage@$version")
        .entryPoint("npx", ConformancePackage)
        .build()

  val legacyImage: ImageFromDockerfile = conformanceImage(LegacyKitVersion, "legacy")
  val modernImage: ImageFromDockerfile = conformanceImage(ModernKitVersion, "modern")

  /** The URL the kit uses to reach the server, which depends on the networking mode. */
  private def serverUrlFor(port: Int): String =
    if useHostNetwork then s"http://localhost:$port/mcp"
    else s"http://host.testcontainers.internal:$port/mcp"

  /** Run the kit against the server bound on `port` of the host. */
  def runConformance(
    image: ImageFromDockerfile,
    port: Int,
    specVersion: String,
    expectedFailures: String,
    scenario: Option[String] = None,
    requirementsMode: Boolean = false,
  ): Task[(Long, String)] =
    ZIO.logInfo(
      s"conformance $specVersion: ${if useHostNetwork then "host" else "bridge"} networking, " +
        s"url=${serverUrlFor(port)}, ca=${hostCaBundle.fold("none")(_.toString)}"
    ) *>
    ZIO.attemptBlocking:
      val expectedFailuresFile = java.io.File.createTempFile("expected-failures", ".yaml")
      expectedFailuresFile.deleteOnExit()
      Files.writeString(expectedFailuresFile.toPath, expectedFailures)

      val stdout    = ToStringConsumer()
      val container = GenericContainer(image)

      // Host networking reaches the server as `localhost`; bridged networking has
      // to go through the gateway alias, which `dns-rebinding-protection` cannot use.
      // These `with…` builders declare testcontainers' self type, which Scala infers
      // as `Nothing`; keep them in statement position so no cast to `Nothing` is
      // emitted (in value position that throws `ClassCastException` at runtime).
      if useHostNetwork then
        container.withNetworkMode("host")
        ()
      else
        TC.exposeHostPorts(port)
        container.withAccessToHost(true)
        ()

      container.withFileSystemBind(expectedFailuresFile.getAbsolutePath, "/tmp/expected-failures.yaml",
        org.testcontainers.containers.BindMode.READ_ONLY)
      val selection =
        // `--requirements` (kit 0.2.0-alpha.11+) runs exactly the scenarios a
        // revision requires, frozen at its release, and is the only mode that
        // reaches the `input-required-result-*` (MRTR) scenarios — they were
        // pending in the kit's own suite when 2026-07-28 shipped, so the
        // default `active` suite still skips them.
        //
        // `--spec-version` is the older selector: it picks which protocol era
        // the kit drives, and without either flag the kit defaults to the
        // legacy `initialize` handshake even on the 2026-07-28 line.
        if requirementsMode then Seq("--requirements", specVersion)
        else Seq("--spec-version", specVersion)
      val baseArgs = Seq(
        "server",
        "--url", serverUrlFor(port),
      ) ++ selection ++ Seq(
        "--expected-failures", "/tmp/expected-failures.yaml",
      )
      val args = scenario.fold(baseArgs)(s => baseArgs ++ Seq("--scenario", s))
      container.withCommand(args*)
      container.withStartupCheckStrategy(
        // The kit runs ~30 scenarios in one shot; allow generous headroom so a
        // slower Docker host does not time the whole run out mid-suite.
        OneShotStartupCheckStrategy().withTimeout(JDuration.ofSeconds(180))
      )
      container.withLogConsumer(stdout)

      try
        container.start()
        (container.getContainerInfo.getState.getExitCodeLong, stdout.toUtf8String)
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
      test("2025-11-25 (legacy) conformance suite passes"):
        for
          port              <- Server.install(testServer.routes)
          _                 <- ZIO.logInfo(s"MCP server started on port $port")
          (exitCode, output) <- runConformance(legacyImage, port, "2025-11-25", expectedFailuresYaml)
          _                 <- ZIO.logInfo(s"Legacy conformance exit code: $exitCode")
          _                 <- ZIO.logInfo(s"Legacy conformance output:\n$output").when(exitCode != 0)
        yield assertTrue(exitCode == 0L)
      ,
      test("2026-07-28 (modern) conformance suite passes"):
        for
          port              <- Server.install(testServer.routes)
          (exitCode, output) <- runConformance(modernImage, port, "2026-07-28", expectedFailuresYaml, requirementsMode = true)
          _                 <- ZIO.logInfo(s"Modern conformance exit code: $exitCode")
          _                 <- ZIO.logInfo(s"Modern conformance output:\n$output").when(exitCode != 0)
        yield assertTrue(exitCode == 0L)
    ).provide(Server.defaultWith(_.onAnyOpenPort), McpServer.State.default) @@
      withLiveClock @@
      timeout(5.minutes) @@
      sequential
