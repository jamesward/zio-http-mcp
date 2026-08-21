package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.auth.*
import zio.*
import zio.http.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*
import zio.test.TestAspect.*

import java.nio.file.Files
import java.time.Instant
import java.util.concurrent.TimeUnit

/**
 * Drives the REAL TypeScript MCP SDK (`@modelcontextprotocol/sdk`) — the one MCP
 * Inspector uses — against our server, to validate the User-Agent-keyed
 * AS-metadata discovery (see [[McpServer.asMetadataRedirectRoutes]] /
 * [[McpServer.isPrmCapableClient]]).
 *
 * We use the SDK's own `buildDiscoveryUrls` to generate the exact resource-origin
 * AS-metadata URLs Inspector probes, then fetch each with the SDK's transport
 * (Node's global `fetch`, user-agent `node`) and with a legacy `kiro-cli`
 * user-agent, asserting:
 *   - the SDK/Inspector (node) origin probe → 404 (so it falls back to the PRM,
 *     which advertises the real cross-origin AS — no RFC 8414 §3.3 issuer mismatch)
 *   - a legacy kiro-cli / rmcp probe → still a redirect (compat preserved)
 *   - the PRM correctly advertises the configured cross-origin authorization server
 *
 * Needs Node + npm + network (to install the SDK). It auto-detects whether Node
 * and npm are available and runs only then (otherwise it's a no-op) — no env vars.
 */
object TypeScriptSdkDiscoverySpec extends ZIOSpecDefault:

  // A cross-origin AS (distinct origin from the resource → the redirect logic
  // engages; the port is intentionally unreachable since we only assert on the
  // origin-probe status, never follow the redirect).
  private val issuer = AuthorizationServer("http://localhost:65535")

  private val verifier: TokenVerifier[Any] =
    TokenVerifier.fromFunction(raw => ZIO.succeed(Principal(
      subject = Some("u"), clientId = Some("c"), scopes = Set.empty, audience = Set.empty,
      issuer = Some("http://localhost:65535"), expiresAt = Some(Instant.now().plusSeconds(3600)),
      raw = raw, claims = Json.Obj())))

  private val pingTool: McpToolHandler = McpTool("ping").description("ping").handle(ZIO.succeed("pong"))

  private val server: McpServer[Any] =
    McpServer("param-mount", "0.1.0")
      .mountedAtParam("tenant")
      .auth(McpAuth(authorizationServers = NonEmptyChunk(issuer), verifier = verifier))
      .tool(pingTool)

  private val probeScript =
    """import { discoverOAuthProtectedResourceMetadata, buildDiscoveryUrls } from '@modelcontextprotocol/sdk/client/auth.js';
      |const resource = process.argv[2];
      |const out = {};
      |async function hit(u, ua) {
      |  const r = await fetch(u, { redirect: 'manual', headers: { 'user-agent': ua } });
      |  return { status: r.status, type: r.type };
      |}
      |try {
      |  const prm = await discoverOAuthProtectedResourceMetadata(resource);
      |  out.authServers = prm.authorization_servers;
      |  const urls = buildDiscoveryUrls(new URL(resource)).map(u => (u.url ?? u).toString());
      |  out.probeUrls = urls;
      |  out.node = []; out.kiro = [];
      |  for (const u of urls) {
      |    out.node.push(await hit(u, 'node'));
      |    out.kiro.push(await hit(u, 'kiro-cli/0.3 rmcp/0.1'));
      |  }
      |  out.ok = true;
      |} catch (e) { out.ok = false; out.error = String(e && e.message); }
      |console.log('RESULT_JSON=' + JSON.stringify(out));
      |""".stripMargin

  private final case class Probe(status: Int, @jsonField("type") tpe: String) derives JsonDecoder
  private final case class ProbeResult(
      authServers: List[String] = Nil,
      probeUrls:   List[String] = Nil,
      node:        List[Probe]  = Nil,
      kiro:        List[Probe]  = Nil,
      ok:          Boolean      = false,
      error:       Option[String] = None,
  ) derives JsonDecoder

  private def runProbe(resource: String): Task[String] =
    ZIO.attemptBlocking {
      // Self-contained: npm install the SDK into a temp dir.
      val workDir = Files.createTempDirectory("ts-sdk-probe").toFile
      Files.writeString(workDir.toPath.resolve("package.json"), """{"name":"probe","private":true}""")
      val install = ProcessBuilder("npm", "install", "--no-audit", "--no-fund", "@modelcontextprotocol/sdk")
        .directory(workDir).redirectErrorStream(true).start()
      val installOut = String(install.getInputStream.readAllBytes())
      val installed  = install.waitFor(300, TimeUnit.SECONDS)
      require(installed && install.exitValue() == 0, s"npm install failed:\n$installOut")
      Files.writeString(workDir.toPath.resolve("probe.mjs"), probeScript)
      val proc = ProcessBuilder("node", "probe.mjs", resource)
        .directory(workDir).redirectErrorStream(true).start()
      val out = String(proc.getInputStream.readAllBytes())
      if !proc.waitFor(60, TimeUnit.SECONDS) then proc.destroyForcibly()
      out
    }

  private def realSuite =
    suite("TypeScriptSdkDiscoverySpec")(
      test("the TypeScript SDK (MCP Inspector) origin probe gets 404; a legacy kiro-cli probe still redirects"):
        for
          port    <- Server.install(server.statelessRoutes)
          resource = s"http://localhost:$port/tenant-a"
          raw     <- runProbe(resource)
          line     = raw.linesIterator.find(_.startsWith("RESULT_JSON=")).map(_.stripPrefix("RESULT_JSON=")).getOrElse("{}")
          res     <- ZIO.fromEither(line.fromJson[ProbeResult]).mapError(e => RuntimeException(s"probe decode failed: $e\nraw:\n$raw"))
          _       <- ZIO.logInfo(s"TS SDK probe: $line").when(!res.ok || res.node.exists(_.status != 404))
        yield assertTrue(
          res.ok,
          // PRM advertises the configured cross-origin AS.
          res.authServers == List("http://localhost:65535"),
          // The SDK generates resource-origin AS-metadata probe URLs (this is what
          // Inspector hits — and where our old 302 caused the issuer mismatch).
          res.probeUrls.nonEmpty,
          res.probeUrls.forall(_.contains(s"localhost:$port")),
          // TS SDK / Node fetch (user-agent "node") → 404 on every origin-probe
          // URL, so Inspector falls back to the PRM instead of failing on the
          // cross-origin redirect.
          res.node.nonEmpty,
          res.node.forall(_.status == 404),
          // Same origin-probe URL, different behavior by client: Inspector (node)
          // gets 404 while legacy kiro-cli / rmcp gets the 302 compat redirect —
          // the User-Agent-keyed branch.
          res.kiro.nonEmpty,
          res.node.zip(res.kiro).exists((n, k) => n.status == 404 && k.status == 302),
        )
    ).provide(
      Server.defaultWith(_.onAnyOpenPort),
      Scope.default,
    ) @@ withLiveClock @@ timeout(6.minutes) @@ sequential

  // Detect whether the environment has what this test needs (Node + npm). If so
  // it runs; otherwise it's a no-op. No env vars — capability is probed directly.
  private def toolAvailable(cmd: String*): Boolean =
    try
      val p = ProcessBuilder(cmd*).redirectErrorStream(true).start()
      p.getInputStream.readAllBytes()
      p.waitFor(15, TimeUnit.SECONDS) && p.exitValue() == 0
    catch case _: Throwable => false

  private lazy val nodeToolsAvailable: Boolean =
    toolAvailable("node", "--version") && toolAvailable("npm", "--version")

  override def spec =
    if nodeToolsAvailable then realSuite
    else
      suite("TypeScriptSdkDiscoverySpec")(
        test("skipped — Node + npm not available in this environment")(assertCompletes)
      )
