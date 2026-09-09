package com.jamesward.ziohttp.mcp

import zio.*

import java.nio.charset.StandardCharsets
import java.security.SecureRandom
import java.util.Base64
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

/**
 * Integrity protection for the opaque `requestState` a modern server hands the
 * client on an `input_required` result (MRTR, SEP-2322).
 *
 * The state travels through the client, which can read and change it, so the
 * server signs it: what comes back is only accepted if the signature still
 * matches. A stateless server can therefore park a handler's progress in the
 * client's hands without trusting the client with it.
 *
 * The wire form is `<base64url(state)>.<base64url(HMAC-SHA256(state))>`, so any
 * edit — including appending to it — fails verification.
 */
private[mcp] object RequestState:

  private val Algorithm = "HmacSHA256"

  private val encoder = Base64.getUrlEncoder.withoutPadding
  private val decoder = Base64.getUrlDecoder

  /** A fresh signing key, one per server instance. */
  def randomKey: Array[Byte] =
    val bytes = new Array[Byte](32)
    SecureRandom().nextBytes(bytes)
    bytes

  def sign(key: Array[Byte], state: String): String =
    val payload = state.getBytes(StandardCharsets.UTF_8)
    s"${encoder.encodeToString(payload)}.${encoder.encodeToString(mac(key, payload))}"

  /**
   * The state carried by `signed`, or `None` when it was not produced by this
   * server: a malformed value, or one whose signature no longer matches.
   */
  def verify(key: Array[Byte], signed: String): Option[String] =
    signed.split('.') match
      case Array(payloadPart, signaturePart) =>
        for
          payload   <- decode(payloadPart)
          signature <- decode(signaturePart)
          if java.security.MessageDigest.isEqual(mac(key, payload), signature)
        yield String(payload, StandardCharsets.UTF_8)
      case _ => None

  private def decode(part: String): Option[Array[Byte]] =
    try Some(decoder.decode(part))
    catch case _: IllegalArgumentException => None

  private def mac(key: Array[Byte], payload: Array[Byte]): Array[Byte] =
    val hmac = Mac.getInstance(Algorithm)
    hmac.init(SecretKeySpec(key, Algorithm))
    hmac.doFinal(payload)

/**
 * How a modern (2026-07-28) server turns the opaque state a handler sets into
 * the token the client carries on its retry, and back again.
 *
 * MRTR is stateless by design: the state travels in the client, so nothing has
 * to be stored server-side and a retry can land on any replica. What the
 * replicas must agree on is how to validate what comes back — hence
 * [[McpRequestStateStore.signed]], where sharing one secret is all that horizontal
 * scaling needs.
 *
 * Implement this trait to keep the state server-side instead — in Redis, a
 * database, whatever the deployment already runs — and hand the client only a
 * handle. That is the option to reach for when the state is large, or must not
 * be visible to the client at all. [[McpRequestStateStore.inMemory]] is the
 * reference for that shape; it is not itself shareable across replicas.
 */
trait McpRequestStateStore:
  /** The token to put on an `input_required` result for `state`. */
  def issue(state: String): UIO[String]

  /** The state `token` stands for, or `None` if it is not one this server
    * issued — forged, tampered with, or long since expired. */
  def resolve(token: String): UIO[Option[String]]

object McpRequestStateStore:

  /**
   * Sign the state and let the client carry it (the default). Nothing is stored,
   * so a retry can be served by any instance that holds the same key.
   *
   * The key is random per server instance, which is right for a single server
   * and wrong for a replicated one: a retry that lands on another replica is
   * rejected as tampered. Replicated deployments want [[signed]] with a shared
   * secret.
   */
  def ephemeral: McpRequestStateStore = Signed(RequestState.randomKey)

  /**
   * Sign the state with `secret`, so every instance configured with the same
   * secret accepts state issued by the others. Still stores nothing.
   *
   * The secret is the only thing standing between a client and forged state:
   * load it from the deployment's secret store, keep it out of source, and give
   * it the entropy of a key rather than of a password. Rotating it invalidates
   * exchanges already in flight, which clients see as one rejected retry.
   */
  def signed(secret: String): McpRequestStateStore =
    Signed(secret.getBytes(java.nio.charset.StandardCharsets.UTF_8))

  /**
   * Keep the state server-side and hand the client an opaque handle.
   *
   * In-memory, so it belongs to one instance — the reference implementation for
   * this shape rather than something to point a replicated deployment at. It
   * holds at most `capacity` exchanges, dropping the oldest first, so a flood of
   * abandoned ones cannot grow without bound; a dropped handle is rejected on
   * retry exactly like an expired one.
   */
  def inMemory(capacity: Int = 10000): UIO[McpRequestStateStore] =
    for
      entries <- Ref.make(Chunk.empty[(String, String)])
      counter <- Ref.make(0L)
    yield new McpRequestStateStore:
      def issue(state: String): UIO[String] =
        for
          next  <- counter.updateAndGet(_ + 1)
          token  = s"$next-${java.util.UUID.randomUUID()}"
          _     <- entries.update(kept => (kept :+ (token -> state)).takeRight(capacity))
        yield token

      def resolve(token: String): UIO[Option[String]] =
        entries.get.map(_.collectFirst { case (t, state) if t == token => state })

  private final case class Signed(key: Array[Byte]) extends McpRequestStateStore:
    def issue(state: String): UIO[String] = ZIO.succeed(RequestState.sign(key, state))
    def resolve(token: String): UIO[Option[String]] = ZIO.succeed(RequestState.verify(key, token))
