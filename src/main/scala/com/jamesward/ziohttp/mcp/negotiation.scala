package com.jamesward.ziohttp.mcp

import zio.json.ast.Json

import java.nio.charset.StandardCharsets
import java.util.Base64

/**
 * Which protocol era a Streamable HTTP request belongs to, and — for modern
 * requests — the negotiated version.
 *
 * A dual-era server chooses its behaviour from how the client opens the
 * request (per the versioning spec):
 *
 *   - a request carrying modern per-request `_meta`
 *     (`io.modelcontextprotocol/protocolVersion`), or naming a modern-only
 *     method, is [[Modern]] and served statelessly;
 *   - an `initialize` request, or any request with neither signal, is
 *     [[Legacy]] and served with the handshake/session machinery.
 */
enum ProtocolEra:
  case Modern(version: ProtocolVersion)
  case Legacy

object ProtocolEra:
  given CanEqual[ProtocolEra, ProtocolEra] = CanEqual.derived

/** Why a modern request was rejected during negotiation. Each maps to a
  * `400 Bad Request` with a specific JSON-RPC error code. */
enum NegotiationError:
  /** Requested a protocol version the server does not implement (`-32022`). */
  case UnsupportedVersion(requested: String)
  /** An HTTP routing header is missing or disagrees with the body (`-32020`). */
  case HeaderMismatch(message: String)

object NegotiationError:
  given CanEqual[NegotiationError, NegotiationError] = CanEqual.derived

/**
 * Pure request-classification and header-validation logic for the Streamable
 * HTTP transport. Kept free of ZIO/`Response` so it is trivially unit-testable;
 * the server maps [[NegotiationError]] onto HTTP responses.
 */
object Negotiation:

  /** HTTP header names mirrored from the JSON-RPC body (case-insensitive). */
  val ProtocolVersionHeader = "mcp-protocol-version"
  val MethodHeader          = "mcp-method"
  val NameHeader            = "mcp-name"

  /** The methods whose `Mcp-Name` header mirrors a body field, and which field. */
  private val nameSourceField: Map[String, String] = Map(
    "tools/call"     -> "name",
    "resources/read" -> "uri",
    "prompts/get"    -> "name",
  )

  /** The protocol version declared in the request body's `_meta`, if any. */
  def bodyProtocolVersion(params: Option[Json.Obj]): Option[String] =
    McpMeta.string(McpMeta.of(params), McpMeta.ProtocolVersion)

  /**
   * Classify a request. Modern iff its body declares a modern protocol version
   * in `_meta`, or the header declares one, or the method only exists in the
   * modern era. Everything else — including `initialize` and legacy follow-up
   * requests carrying `MCP-Protocol-Version: 2025-11-25` — is legacy.
   */
  def isModernRequest(
    method: String,
    params: Option[Json.Obj],
    protocolHeader: Option[String],
  ): Boolean =
    val modernMethod = McpDispatchMethod.parse(method).exists {
      case McpDispatchMethod.ServerDiscover | McpDispatchMethod.SubscriptionsListen |
           McpDispatchMethod.TasksGet | McpDispatchMethod.TasksUpdate | McpDispatchMethod.TasksCancel => true
      case _ => false
    }
    // The authoritative modern signal is the *presence* of the per-request
    // protocol-version key in `_meta`, regardless of whether its value is a
    // version we support — an unsupported value must still be answered with a
    // modern `UnsupportedProtocolVersionError`, not fall through to legacy.
    val bodyModern   = bodyProtocolVersion(params).isDefined
    // A header-only signal is only trusted when it names a modern version; a
    // legacy follow-up request also carries `MCP-Protocol-Version: 2025-11-25`.
    val headerModern = protocolHeader.flatMap(ProtocolVersion.parse).exists(_.isStateless)
    modernMethod || bodyModern || headerModern

  /**
   * Validate a modern request's routing headers against its body and resolve
   * the negotiated protocol version.
   *
   *   - `MCP-Protocol-Version` MUST be present and equal the body's
   *     `_meta` protocol version (when the body declares one).
   *   - `Mcp-Method` MUST be present and equal the JSON-RPC `method`.
   *   - `Mcp-Name` MUST be present and equal `params.name` / `params.uri` for
   *     the data methods, decoding the Base64 sentinel form first.
   *   - the resolved version MUST be one the server supports.
   */
  def resolveModern(
    method: String,
    params: Option[Json.Obj],
    protocolHeader: Option[String],
    methodHeader: Option[String],
    nameHeader: Option[String],
  ): Either[NegotiationError, ProtocolVersion] =
    val bodyVersion = bodyProtocolVersion(params)

    // Mcp-Method must be present and match the JSON-RPC method.
    val methodCheck: Either[NegotiationError, Unit] = methodHeader match
      case None => Left(NegotiationError.HeaderMismatch("Missing required Mcp-Method header"))
      case Some(h) if h != method =>
        Left(NegotiationError.HeaderMismatch(s"Mcp-Method header '$h' does not match body method '$method'"))
      case Some(_) => Right(())

    // MCP-Protocol-Version must be present and, when the body declares one, agree.
    val versionHeaderCheck: Either[NegotiationError, Unit] = protocolHeader match
      case None => Left(NegotiationError.HeaderMismatch("Missing required MCP-Protocol-Version header"))
      case Some(h) =>
        bodyVersion match
          case Some(b) if b != h =>
            Left(NegotiationError.HeaderMismatch(
              s"MCP-Protocol-Version header '$h' does not match body _meta protocolVersion '$b'"))
          case _ => Right(())

    // Mcp-Name must be present and match the body field for the data methods.
    val nameCheck: Either[NegotiationError, Unit] = nameSourceField.get(method) match
      case None => Right(())
      case Some(field) =>
        val bodyValue = params.flatMap(_.get(field)).flatMap(_.asString)
        (nameHeader, bodyValue) match
          case (None, _) =>
            Left(NegotiationError.HeaderMismatch(s"Missing required Mcp-Name header for $method"))
          case (Some(h), Some(b)) if decodeHeaderValue(h) != b =>
            Left(NegotiationError.HeaderMismatch(
              s"Mcp-Name header '${decodeHeaderValue(h)}' does not match body '$field' value '$b'"))
          case (Some(_), _) => Right(())

    for
      _       <- methodCheck
      _       <- versionHeaderCheck
      _       <- nameCheck
      // Resolve: prefer body-declared version, then header, then latest.
      wire     = bodyVersion.orElse(protocolHeader).getOrElse(ProtocolVersion.latest.wire)
      version <- ProtocolVersion.parse(wire).toRight(NegotiationError.UnsupportedVersion(wire))
    yield version

  /**
   * Decode a header value that may use the Base64 sentinel form
   * `=?base64?<data>?=` (used for `Mcp-Name` / `Mcp-Param-*` values that are
   * not header-safe). A plain value is returned unchanged.
   */
  def decodeHeaderValue(value: String): String =
    val prefix = "=?base64?"
    val suffix = "?="
    if value.startsWith(prefix) && value.endsWith(suffix) && value.length >= prefix.length + suffix.length then
      val encoded = value.substring(prefix.length, value.length - suffix.length)
      try new String(Base64.getDecoder.decode(encoded), StandardCharsets.UTF_8)
      catch case _: IllegalArgumentException => value
    else value

  /** The `data` payload of an `UnsupportedProtocolVersionError`. */
  def unsupportedVersionData(requested: String): Json.Obj =
    Json.Obj(
      "supported" -> Json.Arr(ProtocolVersion.supportedWire.map(Json.Str(_))),
      "requested" -> Json.Str(requested),
    )
