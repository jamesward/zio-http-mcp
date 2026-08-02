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
      _         <- runScenario(serverUrl, scenario, context, version)
                     .provideSome[Scope](Client.default)
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
                ))
      tools  <- client.listTools
      _      <- Console.printLine(s"tools: ${tools.map(_.name.value).mkString(", ")}")
      _      <- ZIO.foreachDiscard(tools.headOption): tool =>
                  client.callTool(tool.name.value, Json.Obj())
                    .flatMap(r => Console.printLine(s"called ${tool.name.value}: isError=${r.isError}"))
    yield ()
