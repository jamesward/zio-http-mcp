package com.jamesward.ziohttp.mcp

import java.nio.file.{Files, Path, Paths}

/** What the container-based specs (`ConformanceSpec`, `PythonSdkSpec`) share: the host's proxy CA and how a container reaches the server. */
object DockerSupport:
  /**
   * A CA bundle to trust inside the image build, taken from the standard
   * environment variables. On an ordinary machine none of these are set and the
   * image is built unchanged; behind a TLS-intercepting proxy (corporate network,
   * sandboxed CI) they point at the proxy's bundle, without which `npm install`
   * fails the build with `SELF_SIGNED_CERT_IN_CHAIN`.
   */
  val hostCaBundle: Option[Path] =
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
  val useHostNetwork: Boolean =
    sys.env.get("CONFORMANCE_HOST_NETWORK")
      .map(_.equalsIgnoreCase("true"))
      .getOrElse(java.lang.System.getProperty("os.name", "").toLowerCase.contains("linux"))
