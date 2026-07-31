package com.jamesward.ziohttp.mcp

import zio.*
import zio.http.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*
import zio.test.TestAspect.*

/**
 * Live interop check of Client ID Metadata Documents (CIMD,
 * [[https://datatracker.ietf.org/doc/html/draft-ietf-oauth-client-id-metadata-document-00 draft-ietf-oauth-client-id-metadata-document-00]])
 * against the real authorization server at `https://login.jamesward.dev`.
 *
 * The MCP `2026-07-28` spec makes CIMD the preferred client-registration mechanism.
 * Our own CIMD behavior is covered offline by [[CimdAuthSpec]] (against [[TestIdp]])
 * and by the official conformance kit ([[ClientConformanceSpec]], which only checks
 * that a URL-shaped `client_id` was used — it never dereferences it); this spec
 * exists to catch drift between those and a third-party implementation that really
 * does fetch and validate the document.
 *
 * A CIMD document must declare a `client_id` equal to its own URL, which means it
 * has to be served from a public HTTPS host the authorization server can reach.
 * [[CimdTestServer]] provides that: `https://www.cimd.now/<port>/<path>` returns a
 * document whose `client_id` is that URL and whose `redirect_uris` is
 * `http://localhost:<port>/<path>`, so a test can mint a document matching whatever
 * loopback port it happens to bind.
 *
 * The full authorization-code flow can't be automated here — `/oauth2/authorize`
 * requires an interactive user session — but client *identification* is observable
 * at the token endpoint: posting an `authorization_code` grant with a deliberately
 * bogus code returns
 *
 *   - `invalid_grant` when the AS resolved and accepted the client, and
 *   - `invalid_client` (or a bare `401`) when it could not.
 */
object LiveCimdAuthSpec extends ZIOSpecDefault:

  private val issuer = AuthTestHelpers.asIssuer

  /** Base URL of the CIMD test server; override to point at another deployment. */
  private val cimdServer: String =
    sys.env.getOrElse("CIMD_TEST_SERVER", "https://www.cimd.now").stripSuffix("/")

  private def documentUrl(port: Int, path: String): String = s"$cimdServer/$port/$path"
  private def redirectUri(port: Int, path: String): String = s"http://localhost:$port/$path"

  private def fetchJson(url: String): ZIO[Client, Throwable, (Status, Option[Json.Obj])] =
    for
      client <- ZIO.service[Client]
      u      <- ZIO.fromEither(URL.decode(url))
      resp   <- ZIO.scoped(client.batched(Request.get(u)))
      body   <- resp.body.asString
    yield (resp.status, body.fromJson[Json.Obj].toOption)

  /**
   * Post an `authorization_code` token request with a bogus code, identifying the
   * client only by `client_id`. Returns the OAuth `error` code, or `None` when the
   * AS answered without an error body (how it rejects unresolvable CIMD clients).
   */
  private def probeClientIdentification(clientId: String, redirect: String): ZIO[Client, Throwable, Option[String]] =
    val form =
      s"grant_type=authorization_code" +
        s"&code=bogus-code-for-client-identification-probe" +
        s"&redirect_uri=${enc(redirect)}" +
        s"&code_verifier=dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk" +
        s"&client_id=${enc(clientId)}"
    for
      client <- ZIO.service[Client]
      url    <- ZIO.fromEither(URL.decode(s"$issuer/oauth2/token"))
      resp   <- ZIO.scoped(client.batched(
                  Request.post(url, Body.fromString(form))
                    .addHeader(Header.ContentType(MediaType.application.`x-www-form-urlencoded`))
                    .addHeader("accept", "application/json")
                ))
      body   <- resp.body.asString
    yield body.fromJson[Json.Obj].toOption.flatMap(_.get("error")).flatMap(_.asString)

  private def enc(s: String): String = java.net.URLEncoder.encode(s, "UTF-8")

  /** A fresh port number per run, so each test mints a distinct client identity. */
  private val randomPort: UIO[Int] = Random.nextIntBetween(20000, 60000)

  override def spec =
    suite("LiveCimdAuthSpec")(

      test("authorization server advertises client_id_metadata_document_supported"):
        for
          (status, json) <- fetchJson(s"$issuer/.well-known/oauth-authorization-server")
        yield assertTrue(
          status.isSuccess,
          json.flatMap(_.get("client_id_metadata_document_supported")).flatMap(_.asBoolean).contains(true),
        )
      ,
      test("CIMD test server serves a self-consistent document for an arbitrary port"):
        for
          port           <- randomPort
          url             = documentUrl(port, "callback")
          (status, json) <- fetchJson(url)
        yield
          val doc = json.getOrElse(Json.Obj())
          val redirects = doc.get("redirect_uris").flatMap(_.asArray).map(_.flatMap(_.asString)).getOrElse(Chunk.empty)
          assertTrue(
            status.isSuccess,
            // The document must name its own URL, or the AS is right to reject it.
            doc.get("client_id").flatMap(_.asString).contains(url),
            // and it must authorize the loopback listener the test would bind.
            redirects.contains(redirectUri(port, "callback")),
          )
      ,
      test("authorization server identifies a client by its Client ID Metadata Document"):
        for
          port  <- randomPort
          url    = documentUrl(port, "callback")
          _     <- ZIO.logInfo(s"probing CIMD identification with $url")
          error <- probeClientIdentification(url, redirectUri(port, "callback"))
        yield assertTrue(
          // `invalid_grant` means the bogus code failed *after* the AS resolved the
          // client from its metadata document — identification succeeded.
          error.contains("invalid_grant")
        )
      ,
      test("authorization server does not identify a client from an unresolvable URL"):
        for
          port  <- randomPort
          error <- probeClientIdentification(s"$cimdServer/", redirectUri(port, "callback"))
        yield assertTrue(!error.contains("invalid_grant"))
      ,
    ).provide(Client.default) @@ tag("live-auth") @@ sequential @@ withLiveClock @@ timeout(90.seconds)
