organization := "com.jamesward"

name := "zio-http-mcp"

scalaVersion := "3.8.4"

scalacOptions ++= Seq(
  // "-Yexplicit-nulls", // not sure where it went
  "-language:strictEquality",
  // "-Xfatal-warnings", // not sure where it went
  // Pin the emitted bytecode to Java 17 regardless of the JDK running the
  // build, so CI can run on a newer JDK (e.g. Java 25, needed to load the
  // Java 21 tachyon interop dependency at test time) while the published
  // artifacts stay JDK 17 compatible.
  "-release", "17",
)

val zioVersion = "2.1.26"

libraryDependencies ++= Seq(
  "dev.zio" %% "zio"                   % zioVersion,
  "dev.zio" %% "zio-http"              % "3.11.4",
  "dev.zio" %% "zio-schema-derivation" % "1.8.6",
  "dev.zio" %% "zio-schema-json"       % "1.8.6",

  "com.guizmaii" %% "scala-nimbus-jose-jwt-zio" % "4.1.5",

  "org.slf4j" % "slf4j-simple" % "2.0.18" % Test,

  "dev.zio" %% "zio-test"           % zioVersion % Test,
  "dev.zio" %% "zio-test-sbt"       % zioVersion % Test,
  "dev.zio" %% "zio-test-magnolia"  % zioVersion % Test,

  "org.testcontainers" % "testcontainers" % "2.0.5" % Test,

  "io.modelcontextprotocol.sdk" % "mcp-core"           % "2.0.0" % Test,
  "io.modelcontextprotocol.sdk" % "mcp-json-jackson2"  % "2.0.1" % Test,

  // kpavlov/tachyon — a standalone pure-Java MCP server runtime, used as a
  // third-party interop target for cross-version negotiation tests.
  "dev.tachyonmcp" % "tachyon-core" % "1.0.0-beta.20" % Test,
)

fork := true

javaOptions += "-Djava.net.preferIPv4Stack=true"

// Disable Testcontainers' Ryuk reaper for the (forked) test JVM.
//
// Ryuk is a helper container that publishes a host port so it can be signalled
// to reap leaked containers at JVM exit. Under rootless Docker (RootlessKit)
// that published port is allocated from the bottom of the OS ephemeral range
// (32768–60999) — the very same range the ConformanceSpec test server draws
// from via `Server.onAnyOpenPort`. When the two collide, Ryuk fails to start
// with `bind: address already in use` on 0.0.0.0:32768, and because
// `DockerClientFactory` caches that failure on its JVM-wide singleton, every
// subsequent conformance run in the same sbt session then fails fast — turning
// one transient port clash into a persistent failure.
//
// Testcontainers reads this flag only from the process environment (see
// `ResourceReaper.instance()`), so it must be an env var, not a system
// property. With Ryuk disabled it falls back to `JVMHookResourceReaper`, which
// starts no container and publishes no port (it prunes by label from a JVM
// shutdown hook). ConformanceSpec already stops each container in a `finally`
// and uses a one-shot startup check, so cleanup is unaffected.
Test / envVars += "TESTCONTAINERS_RYUK_DISABLED" -> "true"

licenses := Seq("MIT License" -> uri("https://opensource.org/licenses/MIT"))

homepage := Some(uri("https://github.com/jamesward/zio-http-mcp"))

developers := List(
  Developer(
    "jamesward",
    "James Ward",
    "james@jamesward.com",
    uri("https://jamesward.com")
  )
)

Compile / doc / scalacOptions ++= Seq("-doc-root-content", (baseDirectory.value / "README.md").getAbsolutePath)

ThisBuild / versionScheme := Some("semver-spec")
