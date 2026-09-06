package com.jamesward.ziohttp.mcp

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
