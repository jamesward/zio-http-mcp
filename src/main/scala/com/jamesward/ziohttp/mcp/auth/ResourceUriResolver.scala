package com.jamesward.ziohttp.mcp.auth

import zio.http.*

/**
 * Derives a [[ResourceUri]] from an incoming HTTP request, used when
 * [[McpAuth.resourceUri]] is not explicitly set.
 *
 * Priority order:
 *
 *   1. RFC 7239 `Forwarded` header (`Forwarded: proto=https;host=example.com`).
 *   2. `X-Forwarded-Proto` + `X-Forwarded-Host` (de facto convention used by Heroku, AWS ELB,
 *      Cloudflare, ngrok, etc.).
 *   3. `Host` header with scheme inferred from the listening server (defaults to `http`).
 *   4. `localhost` fallback if no host can be determined.
 *
 * Security caveat: this trusts the client-supplied headers. For a server with direct internet
 * exposure (no proxy in front), an attacker could spoof the `Forwarded` / `X-Forwarded-*` /
 * `Host` headers to make the server advertise an attacker-controlled resource URI. The
 * primary defense is to set [[McpAuth.resourceUri]] explicitly in production, which makes the
 * server pin to a single canonical URL regardless of incoming headers.
 */
private[mcp] object ResourceUriResolver:

  /**
   * Build the resource URI for the given request, using the configured static value if set,
   * otherwise deriving the origin from forwarded headers and appending [[resourcePath]].
   */
  def resolve(
    configured: Option[ResourceUri],
    resourcePath: String,
    request: Request,
  ): ResourceUri = configured match
    case Some(uri) => uri
    case None      =>
      val origin = deriveOrigin(request).getOrElse("http://localhost")
      val path   = if resourcePath.startsWith("/") then resourcePath else s"/$resourcePath"
      val full   = s"$origin${path.stripSuffix("/")}"
      ResourceUri.unsafe(full)

  /** Public for testing. */
  private[auth] def deriveOrigin(request: Request): Option[String] =
    parseForwardedHeader(request)
      .orElse(parseXForwarded(request))
      .orElse(parseHostHeader(request))

  /**
   * Parse the [[https://www.rfc-editor.org/rfc/rfc7239 RFC 7239]] `Forwarded` header.
   * Format: `Forwarded: by=...;for=...;host=...;proto=https` (semicolon-separated kv pairs,
   * comma-separated entries from multiple proxies — first entry wins).
   */
  private def parseForwardedHeader(request: Request): Option[String] =
    request.rawHeader("forwarded").flatMap { value =>
      val firstHop = value.split(',').headOption.getOrElse(value).trim
      val pairs = firstHop.split(';').iterator.flatMap { kv =>
        kv.trim.split('=') match
          case Array(k, v) => Some(k.trim.toLowerCase -> stripQuotes(v.trim))
          case _           => None
      }.toMap
      val proto = pairs.get("proto")
      val host  = pairs.get("host")
      (proto, host) match
        case (Some(p), Some(h)) => Some(s"$p://$h")
        case _                  => None
    }

  /**
   * Parse `X-Forwarded-Proto` + `X-Forwarded-Host` headers (most common convention).
   *
   * `X-Forwarded-Host` is optional: many edge proxies (Heroku, NLB) send only
   * `X-Forwarded-Proto` and leave the original `Host` header untouched, since
   * they don't rewrite the host. When that's the case, fall back to `Host` for
   * the host part — `X-Forwarded-Proto` alone tells us the scheme, and `Host`
   * carries the canonical hostname the client used.
   */
  private def parseXForwarded(request: Request): Option[String] =
    val proto = request.rawHeader("x-forwarded-proto").map(_.trim).filter(_.nonEmpty)
    val host  = request.rawHeader("x-forwarded-host").map(_.trim).filter(_.nonEmpty)
      .orElse(request.rawHeader("host").map(_.trim).filter(_.nonEmpty))
    (proto, host) match
      case (Some(p), Some(h)) => Some(s"${firstOf(p)}://${firstOf(h)}")
      case _                  => None

  /** Fall back to the `Host` header, assuming `http` (no scheme info available). */
  private def parseHostHeader(request: Request): Option[String] =
    request.rawHeader("host")
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(h => s"http://$h")

  /** Some proxy chains pack multiple comma-separated values into one header; take the first. */
  private def firstOf(value: String): String =
    value.split(',').headOption.getOrElse(value).trim

  /** RFC 7239 quoted-string values are surrounded by double quotes. */
  private def stripQuotes(s: String): String =
    if s.length >= 2 && s.startsWith("\"") && s.endsWith("\"") then s.substring(1, s.length - 1)
    else s
