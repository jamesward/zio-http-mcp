package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.auth.*
import zio.*
import zio.http.*
import zio.test.*
import zio.test.TestAspect.*

import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit
import scala.jdk.CollectionConverters.*

/**
 * Whether the installed kiro-cli finds a cross-origin authorization server through
 * the protected resource metadata alone.
 *
 * The server used to answer origin probes for authorization-server metadata with a
 * `User-Agent`-keyed 302, because an older kiro-cli (Rust `rmcp`) ignored the
 * protected resource metadata's `authorization_servers` and looked on the MCP
 * server's own origin. kiro-cli 3.5.0 passing this spec on both engines is why that
 * redirect was removed ([[AsMetadataOriginSpec]]); a failure here means a kiro-cli
 * that needs it again. This spec runs the real kiro-cli, on both of its engines,
 * against an auth-protected, parameter-mounted server whose authorization server
 * ([[TestIdp]]) is on another origin, and records every discovery request it makes.
 *
 * Passing means the client found the authorization server through the protected
 * resource metadata and never asked the MCP origin for authorization-server
 * metadata, which the server doesn't serve. It doesn't finish the
 * sign-in: kiro-cli opens no browser in `--no-interactive` mode, and discovery is
 * all this checks.
 *
 * Needs `kiro-cli` on the `PATH`, signed in (`kiro-cli whoami`), and sends it one
 * trivial prompt per engine. Without it the suite is a no-op. kiro-cli runs with
 * its own temporary `HOME`, so the user's MCP configuration is never touched.
 * Tagged `kiro-cli`.
 */
object KiroCliDiscoverySpec extends ZIOSpecDefault:

  private final case class Seen(origin: String, method: String, path: String, userAgent: String)

  /** The engines kiro-cli runs, and the flag that selects each. */
  private val engines = List("v3" -> List("--v3"), "v2" -> List("--v2"))

  private val pingTool: McpToolHandler = McpTool("ping").description("ping").handle(ZIO.succeed("pong"))

  private def recorded(tag: String, seen: Ref[Chunk[Seen]], routes: Routes[Any, Response]): Routes[Any, Response] =
    routes @@ HandlerAspect.interceptIncomingHandler(Handler.fromFunctionZIO[Request] { req =>
      seen.update(_ :+ Seen(tag, req.method.toString, req.url.path.encode, req.rawHeader("user-agent").getOrElse("-")))
        .as((req, ()))
    })

  private def run(cmd: String*): Option[(Int, String)] =
    try
      val proc = ProcessBuilder(cmd*).redirectErrorStream(true).start()
      val out = String(proc.getInputStream.readAllBytes())
      if proc.waitFor(30, TimeUnit.SECONDS) then Some((proc.exitValue(), out)) else { proc.destroyForcibly(); None }
    catch case _: java.io.IOException => None

  private lazy val kiroVersion: Option[String] =
    run("kiro-cli", "--version").collect { case (0, out) => out.trim }

  private lazy val signedIn: Boolean =
    run("kiro-cli", "whoami").exists(_._1 == 0)

  /**
   * Run kiro-cli once against `mcpUrl`, in a temporary `HOME` holding only an
   * `mcp.json` that names it. The environment of the kiro-cli session running
   * these tests (its session id, TUI handshake) is left out, so the child starts
   * a session of its own.
   */
  private def runKiro(mcpUrl: String, engineFlags: List[String]): Task[String] =
    ZIO.attemptBlocking:
      val home: Path = Files.createTempDirectory("kiro-cli-eval")
      val settings = Files.createDirectories(home.resolve(".kiro/settings"))
      Files.writeString(settings.resolve("mcp.json"),
        s"""{"mcpServers":{"probe":{"url":"$mcpUrl"}}}""")
      val pb = ProcessBuilder((List("kiro-cli", "chat") ++ engineFlags ++
        List("--no-interactive", "--trust-tools=", "Reply with the single word OK."))*)
        .directory(home.toFile)
        .redirectErrorStream(true)
      val env = pb.environment()
      env.keySet.asScala.toList
        .filter(k => k == "KIRO_SESSION_ID" || k.startsWith("KIRO_TUI_") || k == "KIRO_TURN_MARKER_DIR")
        .foreach(env.remove)
      env.put("HOME", home.toString)
      // Nothing may open a real browser.
      env.put("BROWSER", "/bin/true")
      try
        val proc = pb.start()
        val out = String(proc.getInputStream.readAllBytes())
        if !proc.waitFor(120, TimeUnit.SECONDS) then proc.destroyForcibly()
        out
      finally
        Files.walk(home).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p): Unit)

  private def isAsMetadata(path: String): Boolean =
    path.startsWith("/.well-known/oauth-authorization-server") || path.startsWith("/.well-known/openid-configuration")

  private def engineTest(engine: String, flags: List[String]) =
    test(s"${kiroVersion.getOrElse("kiro-cli")} ($engine) finds a cross-origin authorization server through the PRM alone"):
      ZIO.scoped:
        for
          seen     <- Ref.make(Chunk.empty[Seen])
          idpPort  <- AuthTestHelpers.findFreePort
          idp      <- TestIdp.make(s"http://localhost:$idpPort", TestIdp.Config(dcrEnabled = true, cimdSupported = false))
          _        <- Server.serve(recorded("idp", seen, idp.routes)).provide(Server.defaultWithPort(idpPort)).forkScoped
          _        <- AuthTestHelpers.waitForBind(idpPort)
          verifier <- TokenVerifier.discoverJwks(issuer = idp.issuer)
          server    = McpServer("kiro-eval", "0.1.0")
                        .mountedAtParam("tenant")
                        .auth(McpAuth(authorizationServers = NonEmptyChunk(AuthorizationServer(idp.issuer)), verifier = verifier))
                        .tool(pingTool)
          mcpPort  <- AuthTestHelpers.findFreePort
          _        <- Server.serve(recorded("mcp", seen, server.statelessRoutes)).provide(Server.defaultWithPort(mcpPort)).forkScoped
          _        <- AuthTestHelpers.waitForBind(mcpPort)
          // The sign-in can't finish, so each run ends on its own; record what it asked for.
          out      <- runKiro(s"http://localhost:$mcpPort/tenant-a", flags)
          // Leave out the server's own JWKS fetches from the authorization server.
          requests <- seen.get.map(_.filterNot(_.userAgent.startsWith("Zio-Http-Client")))
          _        <- ZIO.logInfo(
                        s"kiro-cli $engine requests:\n${requests.map(s => s"  [${s.origin}] ${s.method} ${s.path} UA=${s.userAgent}").mkString("\n")}" +
                          s"\nkiro-cli output:\n$out")
        yield
          val mcp = requests.filter(_.origin == "mcp")
          val idpSeen = requests.filter(_.origin == "idp")
          assertTrue(
            // It reached the server and read its protected resource metadata...
            mcp.exists(_.path == "/tenant-a"),
            mcp.exists(_.path.startsWith("/.well-known/oauth-protected-resource")),
            // ...then went to the authorization server the metadata names...
            idpSeen.exists(s => isAsMetadata(s.path)),
            // ...and never asked the MCP origin for authorization-server metadata,
            // which the server doesn't serve.
            !mcp.exists(s => isAsMetadata(s.path)),
          )

  override def spec =
    val available = kiroVersion.isDefined && signedIn
    suite("KiroCliDiscoverySpec")(
      (if available then engines.map(engineTest) else List(test("kiro-cli is not installed and signed in")(assertCompletes) @@ ignore))*
    ).provide(Client.default) @@ tag("kiro-cli") @@ withLiveClock @@ timeout(10.minutes) @@ sequential
