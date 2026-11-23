package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.client.*
import zio.*
import zio.http.Client
import zio.json.*
import zio.json.ast.Json

/**
 * The client-under-test for the official MCP conformance kit's *client* mode
 * (`npx @modelcontextprotocol/conformance client --command …`), driven by
 * [[ClientConformanceSpec]].
 *
 * The kit appends the test server URL as the final argument and communicates the
 * scenario through environment variables:
 *
 *   - `MCP_CONFORMANCE_SCENARIO` — scenario name (e.g. `auth/basic-cimd`)
 *   - `MCP_CONFORMANCE_CONTEXT` — optional JSON with scenario data (pre-registered
 *     `client_id` / `client_secret`)
 *   - `MCP_CONFORMANCE_PROTOCOL_VERSION` — protocol version to run
 *
 * For `auth` scenarios the client configures the matching [[McpClientOAuth]]:
 * `client_credentials` scenarios use [[OAuthClientCredentials]]; everything else
 * uses [[OAuthAuthorizationCode]] with the kit's fixed CIMD document URL, falling
 * back per the spec's registration priority (pre-registration → CIMD → DCR). The
 * kit's mock AS auto-approves, so [[AuthorizationHandler.autoRedirect]] completes
 * the redirect without a browser.
 */
object ConformanceClientMain extends ZIOAppDefault:

  /** The CIMD URL convention used by the conformance kit's `auth/basic-cimd` scenario. */
  private val CimdClientMetadataUrl = "https://conformance-test.local/client-metadata.json"

  override def run =
    for
      args      <- getArgs
      serverUrl <- ZIO.fromOption(args.lastOption).orElseFail(new IllegalArgumentException("usage: ConformanceClientMain <server-url>"))
      scenario   = java.lang.System.getenv().getOrDefault("MCP_CONFORMANCE_SCENARIO", "")
      context    = Option(java.lang.System.getenv("MCP_CONFORMANCE_CONTEXT"))
                     .flatMap(_.fromJson[Json.Obj].toOption)
      version    = Option(java.lang.System.getenv("MCP_CONFORMANCE_PROTOCOL_VERSION"))
      // The client's scope closes inside the one Client.default lives in, so the
      // client's DELETE on close still has an HTTP client to send it with.
      _         <- ZIO.scoped(runScenario(serverUrl, scenario, context, version))
                     .provide(Client.default)
    yield ()

  private def contextString(context: Option[Json.Obj], field: String): Option[String] =
    context.flatMap(_.get(field)).flatMap(_.asString)

  private def oauthFor(scenario: String, context: Option[Json.Obj]): Option[McpClientOAuth] =
    if !scenario.startsWith("auth/") then None
    else if scenario.startsWith("auth/client-credentials") then
      for
        id     <- contextString(context, "client_id")
        secret <- contextString(context, "client_secret")
      yield OAuthClientCredentials(clientId = id, clientSecret = Config.Secret(secret))
    else
      Some(OAuthAuthorizationCode(
        clientId = contextString(context, "client_id"),
        clientSecret = contextString(context, "client_secret").map(Config.Secret(_)),
        clientMetadataUrl = Some(CimdClientMetadataUrl),
        redirectUri = "http://127.0.0.1:3000/callback",
      ))

  private def runScenario(
    serverUrl: String,
    scenario: String,
    context: Option[Json.Obj],
    version: Option[String],
  ): ZIO[Client & Scope, Throwable, Unit] =
    val preferred = version match
      case Some(v) => ProtocolVersion.all.find(_.wire == v).getOrElse(ProtocolVersion.V2025_11_25)
      case None    => ProtocolVersion.V2025_11_25
    for
      client <- McpClient.connect(McpClientConfig(
                  serverUrl = serverUrl,
                  oauth = oauthFor(scenario, context),
                  preferredVersion = preferred,
                  onInputRequest = Some(answerInput),
                ))
      tools  <- client.listTools
      _      <- Console.printLine(s"tools: ${tools.map(_.name.value).mkString(", ")}")
      _      <- exercise(client, scenario, context, tools)
    yield ()

  /**
   * What each scenario asks the client to do once it has listed the tools.
   * A scenario whose context lists `toolCalls` gets exactly those; the others
   * get the calls their descriptions ask for, and by default the first tool is
   * called without arguments.
   */
  private def exercise(
    client: McpClient,
    scenario: String,
    context: Option[Json.Obj],
    tools: Chunk[ToolDefinition],
  ): IO[Throwable, Unit] =
    def call(name: String, arguments: Json.Obj): IO[Throwable, Unit] =
      client.callTool(name, arguments)
        .flatMap(r => Console.printLine(s"called $name: isError=${r.isError}"))
    val toolCalls = context.flatMap(_.get("toolCalls")).flatMap(_.asArray).getOrElse(Chunk.empty).flatMap(_.asObject)
    if toolCalls.nonEmpty then
      ZIO.foreachDiscard(toolCalls): tc =>
        call(tc.get("name").flatMap(_.asString).getOrElse(""), tc.get("arguments").flatMap(_.asObject).getOrElse(Json.Obj()))
    else scenario match
      case "tools_call" =>
        call("add_numbers", Json.Obj("a" -> Json.Num(5), "b" -> Json.Num(3)))
      case "json-schema-2020-12-preservation" =>
        // Echo the focal tool's inputSchema verbatim, to show nothing was lost in parsing.
        ZIO.foreachDiscard(tools.find(_.name.value == "json_schema_2020_12_tool")): focal =>
          call("json_schema_echo", Json.Obj("schema" -> focal.inputSchema))
      case "json-schema-ref-no-deref" =>
        ZIO.unit // listing the tools is the whole scenario
      case "sep-2322-client-request-state" =>
        // Every MRTR tool, concurrently, so each exchange keeps its own state.
        ZIO.foreachParDiscard(tools)(tool => call(tool.name.value, Json.Obj()))
      case _ =>
        ZIO.foreachDiscard(tools.headOption)(tool => call(tool.name.value, Json.Obj()))

  /**
   * The answers a user would give: an elicitation accepted with the form's
   * defaults (SEP-1034: a client pre-populates them), a sampled greeting, and no roots.
   */
  private def answerInput(request: InputRequest): IO[McpClientError, Json] =
    ZIO.succeed:
      request.method match
        case "elicitation/create" =>
          val properties = request.params.get("requestedSchema").flatMap(_.asObject)
            .flatMap(_.get("properties")).flatMap(_.asObject).getOrElse(Json.Obj())
          val defaults = properties.fields.flatMap: (name, schema) =>
            schema.asObject.flatMap(_.get("default")).map(name -> _)
          Json.Obj("action" -> Json.Str("accept"), "content" -> Json.Obj(defaults))
        case "sampling/createMessage" =>
          Json.Obj(
            "role" -> Json.Str("assistant"),
            "content" -> Json.Obj("type" -> Json.Str("text"), "text" -> Json.Str("Hello from the conformance client")),
            "model" -> Json.Str("conformance-model"),
            "stopReason" -> Json.Str("endTurn"),
          )
        case "roots/list" => Json.Obj("roots" -> Json.Arr())
        case _            => Json.Obj()
