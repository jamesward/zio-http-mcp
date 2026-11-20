package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.auth.Principal
import zio.*
import zio.json.*
import zio.json.ast.Json

import java.net.URI
import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import java.time.Instant
import java.util.{Base64, HexFormat}
import scala.util.Try

// --- MCP Events extension, webhook delivery (SEP-3415): the domain ---

/** A webhook callback URL: absolute `http` or `https`, with a host and no credentials or fragment.
  * Whether `http` is accepted is the server's [[McpWebhookTargets]] policy. */
opaque type McpWebhookUrl = URI

object McpWebhookUrl:
  def parse(value: String): Either[String, McpWebhookUrl] =
    Try(URI(value)).toOption.toRight(s"Invalid 'delivery.url': $value").flatMap: uri =>
      val scheme = Option(uri.getScheme).map(_.toLowerCase)
      if !uri.isAbsolute || !scheme.exists(s => s == "https" || s == "http") then
        Left("Invalid 'delivery.url': expected an https URL")
      else if Option(uri.getHost).forall(_.isEmpty) then Left("Invalid 'delivery.url': no host")
      else if Option(uri.getRawUserInfo).isDefined then Left("Invalid 'delivery.url': credentials aren't allowed")
      else if Option(uri.getRawFragment).isDefined then Left("Invalid 'delivery.url': a fragment isn't allowed")
      else Right(uri)

  extension (url: McpWebhookUrl)
    def value: String     = url.toString
    def isHttps: Boolean  = url.getScheme.equalsIgnoreCase("https")
    def host: String      = url.getHost

  given CanEqual[McpWebhookUrl, McpWebhookUrl] = CanEqual.derived

/**
 * A Standard Webhooks signing secret: `whsec_` and the base64 of 24 to 64
 * random bytes. The client supplies it; the server never makes one up.
 */
final case class McpWebhookSecret private (value: String, key: Chunk[Byte]):
  override def toString: String = "McpWebhookSecret(<redacted>)"

object McpWebhookSecret:
  val Prefix: String = "whsec_"

  def parse(value: String): Either[String, McpWebhookSecret] =
    val invalid = "Invalid 'delivery.secret': expected 'whsec_' and the base64 of 24 to 64 bytes"
    Option.when(value.startsWith(Prefix))(value.drop(Prefix.length))
      .flatMap(encoded => Try(Base64.getDecoder.decode(encoded)).toOption)
      .filter(bytes => bytes.length >= 24 && bytes.length <= 64)
      .map(bytes => McpWebhookSecret(value, Chunk.fromArray(bytes)))
      .toRight(invalid)

  given CanEqual[McpWebhookSecret, McpWebhookSecret] = CanEqual.derived

/** Who owns a webhook subscription: the issuer and subject (or client id) of the caller's [[Principal]]. */
opaque type McpWebhookPrincipalId = String

object McpWebhookPrincipalId:
  /** The id of an authenticated caller; `None` when the principal names no subject or client. */
  def of(principal: Principal): Option[McpWebhookPrincipalId] =
    principal.subject.map(s => s"sub:$s").orElse(principal.clientId.map(c => s"client:$c"))
      .map(who => s"${principal.issuer.getOrElse("")}|$who")

  def apply(value: String): McpWebhookPrincipalId = value
  extension (id: McpWebhookPrincipalId) def value: String = id
  given CanEqual[McpWebhookPrincipalId, McpWebhookPrincipalId] = CanEqual.derived

/** The handle of a webhook subscription, derived from its [[McpWebhookKey]]: `X-MCP-Subscription-Id` on every delivery. */
opaque type McpWebhookSubscriptionId = String

object McpWebhookSubscriptionId:
  def apply(value: String): McpWebhookSubscriptionId = value
  extension (id: McpWebhookSubscriptionId) def value: String = id
  given CanEqual[McpWebhookSubscriptionId, McpWebhookSubscriptionId] = CanEqual.derived

/**
 * What identifies a webhook subscription: who asked, where it delivers, and
 * what it listens for. Subscribing again with the same key refreshes it.
 */
final case class McpWebhookKey(
  principal: McpWebhookPrincipalId,
  url: McpWebhookUrl,
  name: McpEventName,
  arguments: Json.Obj,
):
  /** `sub_` and 16 bytes of the SHA-256 of the key, with `arguments` in canonical JSON: the same on every server. */
  lazy val id: McpWebhookSubscriptionId =
    val canonical = Json.Arr(Json.Str(principal.value), Json.Str(url.value), Json.Str(name.value),
      McpCanonicalJson(arguments)).toJson
    val digest = MessageDigest.getInstance("SHA-256").digest(canonical.getBytes(UTF_8))
    McpWebhookSubscriptionId("sub_" + HexFormat.of.formatHex(digest.take(16)))

object McpWebhookKey:
  given CanEqual[McpWebhookKey, McpWebhookKey] = CanEqual.derived

/** Object members sorted by name, and numbers without trailing zeros, so that equal JSON values serialize equally. */
private[mcp] object McpCanonicalJson:
  def apply(json: Json): Json = json match
    case Json.Obj(fields) => Json.Obj(fields.sortBy(_._1).map((k, v) => k -> apply(v)))
    case Json.Arr(values) => Json.Arr(values.map(apply))
    case Json.Num(n)      => Json.Num(n.stripTrailingZeros)
    case other            => other

/** A secret replaced on a refresh: deliveries are signed with it too until `until`, so in-flight ones still verify. */
final case class McpWebhookPreviousSecret(secret: McpWebhookSecret, until: Instant)

object McpWebhookPreviousSecret:
  given CanEqual[McpWebhookPreviousSecret, McpWebhookPreviousSecret] = CanEqual.derived

/**
 * A webhook subscription, as the [[McpWebhookStore]] holds it.
 *
 * @param principal     the caller, for the event type's check function; its `raw` token is not kept
 * @param refreshBefore when it expires unless refreshed; `None` for no expiry
 * @param cursor        the delivery watermark: what it was subscribed from, and,
 *                      in a durable store, kept up to date, so delivery resumes
 *                      from it after a restart
 */
final case class McpWebhookSubscription(
  key: McpWebhookKey,
  secret: McpWebhookSecret,
  previousSecret: Option[McpWebhookPreviousSecret],
  principal: Principal,
  refreshBefore: Option[Instant],
  cursor: Option[McpEventCursor],
):
  def id: McpWebhookSubscriptionId = key.id
  def isExpired(now: Instant): Boolean = refreshBefore.exists(!_.isAfter(now))

  /** The secrets to sign with at `now`: the current one, and a replaced one during its grace window. */
  def signingSecrets(now: Instant): NonEmptyChunk[McpWebhookSecret] =
    NonEmptyChunk(secret, previousSecret.filter(_.until.isAfter(now)).map(_.secret).toList*)

object McpWebhookSubscription:
  given CanEqual[McpWebhookSubscription, McpWebhookSubscription] = CanEqual.derived

/** A `(principal, url)` whose endpoint proved it wants deliveries, until `until` (`None`: as long as its subscriptions last). */
final case class McpWebhookVerification(
  principal: McpWebhookPrincipalId,
  url: McpWebhookUrl,
  until: Option[Instant],
):
  def isValid(now: Instant): Boolean = until.forall(_.isAfter(now))

object McpWebhookVerification:
  given CanEqual[McpWebhookVerification, McpWebhookVerification] = CanEqual.derived

final case class McpWebhookStoreError(message: String, cause: Option[Throwable] = None)

object McpWebhookStoreError:
  given CanEqual[McpWebhookStoreError, McpWebhookStoreError] = CanEqual.derived

/**
 * Where a server keeps its webhook subscriptions and endpoint verifications.
 * Provide one as a layer to [[McpEvents.withWebhooks]].
 *
 * Returning an expired subscription or verification is fine: the server checks
 * `refreshBefore` and `until`, and deletes expired subscriptions.
 */
trait McpWebhookStore:
  /**
   * Whether what is stored survives a restart. Only a durable store lets the
   * server grant subscriptions with no expiry, and only for a durable store does
   * the server keep each subscription's `cursor` up to date.
   */
  def durable: Boolean

  def get(id: McpWebhookSubscriptionId): IO[McpWebhookStoreError, Option[McpWebhookSubscription]]

  /** Every subscription: the server resumes their delivery when it starts. */
  def all: IO[McpWebhookStoreError, Chunk[McpWebhookSubscription]]

  /** Insert the subscription, or replace the one with its id. */
  def put(subscription: McpWebhookSubscription): IO[McpWebhookStoreError, Unit]

  /**
   * Atomically replace the subscription with `f` applied to it, and return what
   * was stored; `None` if there is none. `f` is pure, so a store with optimistic
   * concurrency may apply it more than once.
   */
  def update(id: McpWebhookSubscriptionId)(f: McpWebhookSubscription => McpWebhookSubscription)
    : IO[McpWebhookStoreError, Option[McpWebhookSubscription]]

  def delete(id: McpWebhookSubscriptionId): IO[McpWebhookStoreError, Unit]

  def verification(principal: McpWebhookPrincipalId, url: McpWebhookUrl): IO[McpWebhookStoreError, Option[McpWebhookVerification]]

  /** Insert the verification, or replace the one for its `(principal, url)`. */
  def putVerification(verification: McpWebhookVerification): IO[McpWebhookStoreError, Unit]

/** Why the server couldn't deliver to a callback endpoint: the categories SEP-3415 allows, never the endpoint's own response. */
enum McpWebhookFailure(val wire: String):
  case ConnectionRefused extends McpWebhookFailure("connection_refused")
  case Timeout           extends McpWebhookFailure("timeout")
  case TlsError          extends McpWebhookFailure("tls_error")
  case Http4xx           extends McpWebhookFailure("http_4xx")
  case Http5xx           extends McpWebhookFailure("http_5xx")
  case ChallengeFailed   extends McpWebhookFailure("challenge_failed")

object McpWebhookFailure:
  given CanEqual[McpWebhookFailure, McpWebhookFailure] = CanEqual.derived
  def fromWire(value: String): Option[McpWebhookFailure] = values.find(_.wire == value)

/**
 * How deliveries to a subscription are going, on a refresh.
 *
 * @param active      whether the server is delivering
 * @param failedSince the first failure since the last successful delivery
 */
final case class McpWebhookDeliveryStatus(
  active: Boolean,
  lastDeliveryAt: Option[Instant],
  lastError: Option[McpWebhookFailure],
  failedSince: Option[Instant],
)

object McpWebhookDeliveryStatus:
  given CanEqual[McpWebhookDeliveryStatus, McpWebhookDeliveryStatus] = CanEqual.derived
  val initial: McpWebhookDeliveryStatus = McpWebhookDeliveryStatus(active = true, None, None, None)

/** The lifetime a client asks for on `events/subscribe`. */
enum McpWebhookTtl:
  /** `ttlMs` absent: the server's default. */
  case ServerDefault
  /** `ttlMs: null`: no expiry, which the server grants only from a durable store. */
  case NoExpiry
  case Suggested(ttl: Duration)

object McpWebhookTtl:
  given CanEqual[McpWebhookTtl, McpWebhookTtl] = CanEqual.derived

/** `events/subscribe`: create or refresh the webhook subscription keyed by the caller, `url`, `name` and `arguments`. */
final case class McpEventsSubscribeParams(
  name: McpEventName,
  arguments: Json.Obj,
  url: McpWebhookUrl,
  secret: McpWebhookSecret,
  cursor: Option[McpEventCursor] = None,
  maxAgeMs: Option[Long] = None,
  ttl: McpWebhookTtl = McpWebhookTtl.ServerDefault,
)

object McpEventsSubscribeParams:
  given CanEqual[McpEventsSubscribeParams, McpEventsSubscribeParams] = CanEqual.derived

/**
 * @param refreshBefore the granted expiry: subscribe again before it; `None` for no expiry
 * @param cursor        the watermark, safe to persist: everything before it was delivered or given up on
 * @param truncated     delivery starts later than the cursor given
 */
final case class McpEventsSubscribeResult(
  id: McpWebhookSubscriptionId,
  refreshBefore: Option[Instant],
  cursor: Option[McpEventCursor],
  truncated: Boolean,
  deliveryStatus: Option[McpWebhookDeliveryStatus] = None,
)

object McpEventsSubscribeResult:
  given CanEqual[McpEventsSubscribeResult, McpEventsSubscribeResult] = CanEqual.derived

/** `events/unsubscribe`: end the caller's webhook subscription to `url` for `name` and `arguments`. */
final case class McpEventsUnsubscribeParams(name: McpEventName, arguments: Json.Obj, url: McpWebhookUrl)

object McpEventsUnsubscribeParams:
  given CanEqual[McpEventsUnsubscribeParams, McpEventsUnsubscribeParams] = CanEqual.derived
