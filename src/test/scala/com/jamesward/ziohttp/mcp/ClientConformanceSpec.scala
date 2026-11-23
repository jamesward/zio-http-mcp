package com.jamesward.ziohttp.mcp

import zio.*
import zio.test.*
import zio.test.TestAspect.*

import scala.jdk.CollectionConverters.*

/**
 * Runs the official MCP conformance kit's *client* scenarios against our
 * [[com.jamesward.ziohttp.mcp.client.McpClient]] (as [[ConformanceClientMain]]).
 *
 * For each scenario the kit starts a mock MCP server (and, for the `auth/` scenarios, a mock
 * authorization server), spawns our client with the server URL, and grades its
 * wire-level behavior. Most scenarios are OAuth: CIMD client ids, PKCE S256,
 * RFC 8707 `resource`, RFC 9207 `iss` handling, metadata discovery forms, scope
 * selection, token endpoint client authentication and `offline_access`. The rest
 * are the ones [[ConformanceClientMain]]'s generic flow (connect, list tools,
 * call the first) satisfies: the handshake, request metadata, invalid tool
 * headers, and SEP-2640 skill verification.
 *
 * Needs `npx` (Node) and network access to the npm registry; tagged
 * `conformance-client` for filtering. The kit version is pinned to the same
 * `2026-07-28`-line release [[ConformanceSpec]] uses.
 */
object ClientConformanceSpec extends ZIOSpecDefault:

  private val ConformancePackage = "@modelcontextprotocol/conformance"
  private val KitVersion         = "0.2.0-alpha.12"

  private val scenarios = List(
    // client registration mechanisms
    "auth/basic-cimd",
    "auth/pre-registration",
    // AS metadata discovery well-known forms (root, OIDC, path-based)
    "auth/metadata-default",
    "auth/metadata-var1",
    "auth/metadata-var2",
    "auth/metadata-var3",
    "auth/metadata-issuer-mismatch",
    // RFC 9207 issuer validation
    "auth/iss-supported",
    "auth/iss-not-advertised",
    "auth/iss-supported-missing",
    "auth/iss-wrong-issuer",
    "auth/iss-normalized",
    "auth/iss-unexpected",
    // RFC 8707 resource validation
    "auth/resource-mismatch",
    // scope selection strategy
    "auth/scope-from-www-authenticate",
    "auth/scope-from-scopes-supported",
    "auth/scope-omitted-when-undefined",
    "auth/scope-retry-limit",
    // step-up authorization on 403 insufficient_scope (scope union)
    "auth/scope-step-up",
    // re-registration when the PRM names a new authorization server (SEP-2352)
    "auth/authorization-server-migration",
    // 2025-03-26 servers: no PRM, the server origin is the authorization server
    "auth/2025-03-26-oauth-metadata-backcompat",
    "auth/2025-03-26-oauth-endpoint-fallback",
    // refresh tokens via offline_access
    "auth/offline-access-scope",
    "auth/offline-access-not-supported",
    // token endpoint client authentication
    "auth/token-endpoint-auth-basic",
    "auth/token-endpoint-auth-post",
    "auth/token-endpoint-auth-none",
    // client_credentials extension
    "auth/client-credentials-basic",
    // handshake and request metadata
    "initialize",
    "request-metadata",
    // SSE resumption: GET with Last-Event-ID after the server closes the stream
    "sse-retry",
    // tool calls, and input the server asks for while one runs
    "tools_call",
    "elicitation-sep1034-client-defaults",
    "sep-2322-client-request-state",
    // JSON Schema handling of tool inputSchemas
    "json-schema-ref-no-deref",
    "json-schema-2020-12-preservation",
    // SEP-2243 standard and custom (x-mcp-header) request headers
    "http-standard-headers",
    "http-custom-headers",
    "http-invalid-tool-headers",
    // SEP-2640 skills: no prefetch, and verifying what a skill's files declare
    "sep-2640-client-no-prefetch",
    "sep-2640-client-verify-digest",
    "sep-2640-client-verify-size",
    "sep-2640-client-verify-frontmatter",
  )

  private def clientCommand: Task[String] =
    ZIO.attempt {
      val javaBin = s"${java.lang.System.getProperty("java.home")}/bin/java"
      // sbt forks tests into a worker JVM whose `java.class.path` holds only the
      // worker jars; the real test classpath lives in a URLClassLoader. Collect it
      // from the classloader hierarchy so the spawned client sees the same jars.
      val fromLoaders = LazyList
        .iterate(getClass.getClassLoader)(_.getParent)
        .takeWhile(_ != null)
        .collect { case u: java.net.URLClassLoader => u.getURLs.toSeq }
        .flatten
        .map(u => java.nio.file.Paths.get(u.toURI).toAbsolutePath.toString)
      val entries =
        if fromLoaders.nonEmpty then fromLoaders.distinct
        else java.lang.System.getProperty("java.class.path").split(java.io.File.pathSeparator).toSeq
      val classpath = entries.filter(_.nonEmpty).mkString(java.io.File.pathSeparator)
      require(!classpath.contains(" "), "classpath contains spaces; the conformance kit splits the command on spaces")
      s"$javaBin -cp $classpath com.jamesward.ziohttp.mcp.ConformanceClientMain"
    }

  private def runScenario(scenario: String, command: String, workDir: java.io.File): Task[(Int, String)] =
    ZIO.attemptBlocking {
      val pb = new ProcessBuilder(
        "npx", "-y", s"$ConformancePackage@$KitVersion",
        "client",
        "--command", command,
        "--scenario", scenario,
      )
      pb.directory(workDir)
      pb.redirectErrorStream(true)
      val proc   = pb.start()
      val output = new String(proc.getInputStream.readAllBytes())
      val exited = proc.waitFor(180, java.util.concurrent.TimeUnit.SECONDS)
      if !exited then proc.destroyForcibly()
      (if exited then proc.exitValue() else -1, output)
    }

  override def spec =
    suite("MCP client conformance")(
      scenarios.map { scenario =>
        test(scenario):
          for
            command            <- clientCommand
            workDir            <- ZIO.attempt(java.nio.file.Files.createTempDirectory("mcp-client-conformance").toFile)
            (exitCode, output) <- runScenario(scenario, command, workDir)
            _                  <- ZIO.logInfo(s"$scenario exit=$exitCode\n$output").when(exitCode != 0)
          yield assertTrue(exitCode == 0)
      }*
    ) @@ tag("conformance-client") @@ sequential @@ withLiveClock @@ timeout(20.minutes)
