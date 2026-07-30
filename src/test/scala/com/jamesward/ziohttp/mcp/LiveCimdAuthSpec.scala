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
 * The MCP `2026-07-28` spec makes CIMD the preferred client-registration mechanism,
 * and this AS now advertises `client_id_metadata_document_supported`. Our own CIMD
 * behavior is covered offline by [[CimdAuthSpec]] (against [[TestIdp]]) and by the
 * official conformance kit ([[ClientConformanceSpec]]); this spec exists to catch
 * drift between those and a third-party implementation.
 *
 * The full authorization-code flow can't be automated here — `/oauth2/authorize`
 * requires an interactive user session — but client *identification* is observable
 * at the token endpoint: posting an `authorization_code` grant with a deliberately
 * bogus code returns
 *
 *   - `invalid_grant` when the AS resolved and accepted the client, and
 *   - `invalid_client` (or a bare `401`) when it could not.
 *
 * The metadata documents live in `src/test/resources/cimd` and are served publicly
 * over jsDelivr, whose `application/json` content type this AS requires — the
 * `text/plain` that `raw.githubusercontent.com` returns is rejected. Because a
 * document's `client_id` must equal its own URL, the committed fixtures name their
 * `@main` URLs; set `-Dcimd.base.url=…` (or `CIMD_BASE_URL`) to point at a branch
 * while iterating. When the documents are not reachable at the configured base the
 * AS-side assertions are skipped rather than failed, so a branch that has not
 * reached `main` yet does not break CI.
 */
object LiveCimdAuthSpec extends ZIOSpecDefault:

  private val issuer = AuthTestHelpers.asIssuer

  private val defaultBaseUrl =
    "https://cdn.jsdelivr.net/gh/jamesward/zio-http-mcp@main/src/test/resources/cimd"

  private def baseUrl: String =
    sys.props.get("cimd.base.url")
      .orElse(sys.env.get("CIMD_BASE_URL"))
      .getOrElse(defaultBaseUrl)
      .stripSuffix("/")

  private def documentUrl(name: String): String = s"$baseUrl/$name"

  /**
   * The state of a metadata document at the configured base URL. Only a `404`
   * counts as "not published yet" and skips; every other problem fails the test, so
   * a genuinely broken document can never be mistaken for an unmerged fixture.
   */
  private enum DocumentState:
    case NotPublished
    case Unreadable(reason: String)
    case Published(json: Json.Obj)

  private object DocumentState:
    given CanEqual[DocumentState, DocumentState] = CanEqual.derived

  private def fetchDocument(url: String): ZIO[Client, Nothing, DocumentState] =
    (for
      client <- ZIO.service[Client]
      u      <- ZIO.fromEither(URL.decode(url))
      resp   <- ZIO.scoped(client.batched(Request.get(u)))
      body   <- resp.body.asString
    yield
      if resp.status.code == 404 then DocumentState.NotPublished
      else if !resp.status.isSuccess then DocumentState.Unreadable(s"HTTP ${resp.status.code}")
      else
        body.fromJson[Json.Obj] match
          case Right(json) => DocumentState.Published(json)
          case Left(err)   => DocumentState.Unreadable(s"not a JSON object: $err")
    ).catchAll(t => ZIO.succeed(DocumentState.Unreadable(Option(t.getMessage).getOrElse(t.toString))))

  /** Run `assertion` against a published document; skip only when it is a 404. */
  private def withDocument(url: String)(
    assertion: Json.Obj => ZIO[Client, Throwable, TestResult]
  ): ZIO[Client, Throwable, TestResult] =
    fetchDocument(url).flatMap:
      case DocumentState.Published(json) =>
        ZIO.logInfo(s"probing CIMD identification with $url") *> assertion(json)
      case DocumentState.NotPublished =>
        ZIO.logWarning(
          s"CIMD document not published at $url — skipping the AS-side assertion. " +
            "It runs once the fixture reaches the configured ref (default: main)."
        ).as(assertCompletes)
      case DocumentState.Unreadable(reason) =>
        ZIO.fail(RuntimeException(s"CIMD document at $url could not be read: $reason"))

  /**
   * Post an `authorization_code` token request with a bogus code, identifying the
   * client only by `client_id`. Returns the OAuth `error` code, or `None` when the
   * AS answered without an error body (how it rejects unresolvable CIMD clients).
   */
  private def probeClientIdentification(clientId: String): ZIO[Client, Throwable, Option[String]] =
    val form =
      s"grant_type=authorization_code" +
        s"&code=bogus-code-for-client-identification-probe" +
        s"&redirect_uri=${enc("http://127.0.0.1:3000/callback")}" +
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

  override def spec =
    suite("LiveCimdAuthSpec")(

      test("authorization server advertises client_id_metadata_document_supported"):
        for
          client <- ZIO.service[Client]
          url    <- ZIO.fromEither(URL.decode(s"$issuer/.well-known/oauth-authorization-server"))
          resp   <- ZIO.scoped(client.batched(Request.get(url)))
          body   <- resp.body.asString
          json   <- ZIO.fromEither(body.fromJson[Json.Obj]).mapError(RuntimeException(_))
        yield assertTrue(
          json.get("client_id_metadata_document_supported").flatMap(_.asBoolean).contains(true)
        )
      ,
      test("authorization server identifies a client by its Client ID Metadata Document"):
        val url = documentUrl("client-metadata.json")
        withDocument(url): doc =>
          probeClientIdentification(url).map: error =>
            assertTrue(
              // The document must name its own URL, or the AS is right to reject it.
              doc.get("client_id").flatMap(_.asString).contains(url),
              // `invalid_grant` means the bogus code failed *after* the AS resolved
              // the client from its metadata document — identification succeeded.
              error.contains("invalid_grant"),
            )
      ,
      test("authorization server rejects a metadata document whose client_id does not match its URL"):
        val url = documentUrl("client-metadata-mismatch.json")
        withDocument(url): doc =>
          probeClientIdentification(url).map: error =>
            assertTrue(
              !doc.get("client_id").flatMap(_.asString).contains(url),
              // Anything but `invalid_grant`: the client must not be identified.
              !error.contains("invalid_grant"),
            )
      ,
      test("authorization server does not identify a client from an unresolvable URL"):
        for
          error <- probeClientIdentification(documentUrl("no-such-client-document.json"))
        yield assertTrue(!error.contains("invalid_grant"))
      ,
    ).provide(Client.default) @@ tag("live-auth") @@ sequential @@ withLiveClock @@ timeout(90.seconds)
