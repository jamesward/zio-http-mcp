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
  "dev.zio" %% "zio-http"              % "3.11.3",
  "dev.zio" %% "zio-schema-derivation" % "1.8.5",
  "dev.zio" %% "zio-schema-json"       % "1.8.5",

  "com.guizmaii" %% "scala-nimbus-jose-jwt-zio" % "4.1.5",

  "org.slf4j" % "slf4j-simple" % "2.0.18" % Test,

  "dev.zio" %% "zio-test"           % zioVersion % Test,
  "dev.zio" %% "zio-test-sbt"       % zioVersion % Test,
  "dev.zio" %% "zio-test-magnolia"  % zioVersion % Test,

  "org.testcontainers" % "testcontainers" % "2.0.5" % Test,

  "io.modelcontextprotocol.sdk" % "mcp-core"           % "2.0.0" % Test,
  "io.modelcontextprotocol.sdk" % "mcp-json-jackson2"  % "2.0.0" % Test,

  // kpavlov/tachyon — a standalone pure-Java MCP server runtime, used as a
  // third-party interop target for cross-version negotiation tests.
  "dev.tachyonmcp" % "tachyon-core" % "1.0.0-beta.15" % Test,
)

fork := true

javaOptions += "-Djava.net.preferIPv4Stack=true"

licenses := Seq("MIT License" -> url("https://opensource.org/licenses/MIT"))

homepage := Some(url("https://github.com/jamesward/zio-http-mcp"))

developers := List(
  Developer(
    "jamesward",
    "James Ward",
    "james@jamesward.com",
    url("https://jamesward.com")
  )
)

Compile / doc / scalacOptions ++= Seq("-doc-root-content", (baseDirectory.value / "README.md").getAbsolutePath)

ThisBuild / versionScheme := Some("semver-spec")
