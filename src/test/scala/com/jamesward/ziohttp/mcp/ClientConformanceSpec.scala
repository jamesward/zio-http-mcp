package com.jamesward.ziohttp.mcp

import zio.*
import zio.test.*
import zio.test.TestAspect.*

import scala.jdk.CollectionConverters.*

/**
 * Runs the official MCP conformance kit's *client* auth scenarios against our
 * [[com.jamesward.ziohttp.mcp.client.McpClient]] (as [[ConformanceClientMain]]).
 *
 * For each scenario the kit starts a mock MCP resource server + mock authorization
 * server, spawns our client with the server URL, and grades the wire-level OAuth
 * behavior: CIMD client ids, PKCE S256, RFC 8707 `resource`, RFC 9207 `iss`
 * handling, metadata discovery forms, and scope selection.
 *
 * Needs `npx` (Node) and network access to the npm registry; tagged
 * `conformance-client` for filtering. The kit version is pinned to the same
 * `2026-07-28`-line release [[ConformanceSpec]] uses.
 */
object ClientConformanceSpec extends ZIOSpecDefault:

  private val ConformancePackage = "@modelcontextprotocol/conformance"
  private val KitVersion         = "0.2.0-alpha.10"

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
    // client_credentials extension
    "auth/client-credentials-basic",
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
    suite("MCP client conformance (auth)")(
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
