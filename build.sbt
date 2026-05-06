organization := "com.jamesward"

name := "zio-http-mcp"

scalaVersion := "3.8.3"

scalacOptions ++= Seq(
  // "-Yexplicit-nulls", // not sure where it went
  "-language:strictEquality",
  // "-Xfatal-warnings", // not sure where it went
)

val zioVersion = "2.1.25"

libraryDependencies ++= Seq(
  "dev.zio" %% "zio"                 % zioVersion,
  "dev.zio" %% "zio-http"            % "3.11.1",
  "dev.zio" %% "zio-schema-derivation" % "1.8.3",
  "dev.zio" %% "zio-schema-json"       % "1.8.3",

  "org.slf4j" % "slf4j-simple" % "2.0.17" % Test,

  "dev.zio" %% "zio-test"           % zioVersion % Test,
  "dev.zio" %% "zio-test-sbt"       % zioVersion % Test,
  "dev.zio" %% "zio-test-magnolia"  % zioVersion % Test,

  "org.testcontainers" % "testcontainers" % "2.0.5" % Test,

  "io.modelcontextprotocol.sdk" % "mcp-core"           % "1.1.2" % Test,
  "io.modelcontextprotocol.sdk" % "mcp-json-jackson2"  % "1.1.2" % Test,
)

fork := true

javaOptions += "-Djava.net.preferIPv4Stack=true"

licenses := Seq("MIT License" -> url("https://opensource.org/licenses/MIT"))

homepage := Some(url("https://github.com/jamesward/zio-mavencentral"))

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
