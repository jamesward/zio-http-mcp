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
    // Tasks extension: routes follow-up requests to the instance holding the task.
    "tasks/get"      -> "taskId",
    "tasks/update"   -> "taskId",
    "tasks/cancel"   -> "taskId",
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
   * The methods whose `Mcp-Name` header is REQUIRED by the 2026-07-28 Streamable
   * HTTP binding ("Standard Request Headers"). The Tasks extension's methods also
   * mirror a body field into `Mcp-Name` (see [[nameSourceField]]), but the core
   * spec does not require it there, so for those a missing header stays accepted.
   */
  private val nameRequired: Set[String] = Set("tools/call", "resources/read", "prompts/get")

  /**
   * Validate a modern request's routing headers against its body and resolve
   * the negotiated protocol version.
   *
   * The 2026-07-28 Streamable HTTP binding makes the standard request headers
   * REQUIRED and lists a missing one as a validation failure (400 +
   * `HeaderMismatch`), so that intermediaries routing on the header and the
   * server executing the body can never disagree:
   *
   *   - `Mcp-Method` must be present and equal the JSON-RPC `method`.
   *   - `Mcp-Name` must be present for `tools/call` / `resources/read` /
   *     `prompts/get`, and, when present, equal `params.name` / `params.uri`
   *     (`params.taskId` for the `tasks/...` methods), decoding the Base64
   *     sentinel form first.
   *   - `MCP-Protocol-Version`, when present, must equal the body's `_meta`
   *     protocol version (when the body declares one). Its *absence* stays
   *     accepted: the spec lets a server that still supports pre-2025-06-18
   *     clients (this one serves 2025-03-26) tolerate a missing header, and the
   *     body remains the source of truth.
   *   - the resolved version must be one the server supports.
   *
   * `requireHeaders = false` checks only header↔body agreement, for a binding
   * with no headers to mirror into (the stdio/stream transport).
   */
  def resolveModern(
    method: String,
    params: Option[Json.Obj],
    protocolHeader: Option[String],
    methodHeader: Option[String],
    nameHeader: Option[String],
    requireHeaders: Boolean = true,
  ): Either[NegotiationError, ProtocolVersion] =
    val bodyVersion = bodyProtocolVersion(params)

    // Mcp-Method, when present, must match the JSON-RPC method.
    val methodCheck: Either[NegotiationError, Unit] = methodHeader match
      case None if requireHeaders =>
        Left(NegotiationError.HeaderMismatch(s"Missing required Mcp-Method header (body method '$method')"))
      case Some(h) if h != method =>
        Left(NegotiationError.HeaderMismatch(s"Mcp-Method header '$h' does not match body method '$method'"))
      case _ => Right(())

    // MCP-Protocol-Version, when present and the body declares one, must agree.
    val versionHeaderCheck: Either[NegotiationError, Unit] = (protocolHeader, bodyVersion) match
      case (Some(h), Some(b)) if b != h =>
        Left(NegotiationError.HeaderMismatch(
          s"MCP-Protocol-Version header '$h' does not match body _meta protocolVersion '$b'"))
      case _ => Right(())

    // Mcp-Name, when present, must match the body field for the data methods.
    val nameCheck: Either[NegotiationError, Unit] = nameSourceField.get(method) match
      case None => Right(())
      case Some(field) =>
        val bodyValue = params.flatMap(_.get(field)).flatMap(_.asString)
        (nameHeader, bodyValue) match
          case (None, _) if requireHeaders && nameRequired.contains(method) =>
            Left(NegotiationError.HeaderMismatch(s"Missing required Mcp-Name header for '$method'"))
          case (Some(h), Some(b)) if decodeHeaderValue(h) != b =>
            Left(NegotiationError.HeaderMismatch(
              s"Mcp-Name header '${decodeHeaderValue(h)}' does not match body '$field' value '$b'"))
          case _ => Right(())

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
