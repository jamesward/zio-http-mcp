package com.jamesward.ziohttp.mcp

import zio.Chunk
import zio.json.*
import zio.json.ast.Json

// --- Protocol Version ---

/**
 * A negotiable MCP protocol revision.
 *
 * MCP identifies revisions by their release date. Two revisions matter to this
 * library:
 *
 *   - [[V2025_11_25]] — the last revision built on the `initialize` /
 *     `notifications/initialized` handshake and protocol-level sessions
 *     (`Mcp-Session-Id`). Server-initiated requests (`sampling/createMessage`,
 *     `elicitation/create`) flow over an SSE response stream.
 *   - [[V2026_07_28]] — a stateless rewrite. There is no handshake and no
 *     session: every request carries its protocol version and client
 *     capabilities in `_meta`, `server/discover` advertises capabilities on
 *     demand, and server-initiated interactions use the Multi Round-Trip
 *     Request (MRTR) pattern instead of an SSE back-channel.
 *
 * Wire identifiers are ISO dates, so lexicographic ordering of [[wire]] is the
 * same as chronological ordering of the revisions — [[isAtLeast]] relies on that.
 */
enum ProtocolVersion(val wire: String):
  case V2026_07_28 extends ProtocolVersion("2026-07-28")
  case V2025_11_25 extends ProtocolVersion("2025-11-25")
  case V2025_06_18 extends ProtocolVersion("2025-06-18")
  case V2025_03_26 extends ProtocolVersion("2025-03-26")

object ProtocolVersion:
  given CanEqual[ProtocolVersion, ProtocolVersion] = CanEqual.derived

  /**
   * Every version this library supports over the Streamable HTTP transport,
   * newest first. All but [[V2026_07_28]] are legacy (handshake + session)
   * revisions; `2025-03-26` is the oldest that used Streamable HTTP (the earlier
   * `2024-11-05` used the now-deprecated HTTP+SSE transport and is not modelled).
   * The head is what we advertise as preferred during negotiation.
   */
  val all: Chunk[ProtocolVersion] = Chunk(V2026_07_28, V2025_11_25, V2025_06_18, V2025_03_26)

  /** The newest supported revision — the version a fresh client prefers and the
    * version a server falls back to when a client asks for something newer. */
  val latest: ProtocolVersion = V2026_07_28

  /** The newest supported *legacy* (handshake-based) revision. An `initialize`
    * negotiation never resolves above this — the handshake implies legacy. */
  val latestLegacy: ProtocolVersion = V2025_11_25

  /**
   * The revision assumed when a Streamable HTTP request carries no version
   * signal at all (no `_meta` protocol version, no `MCP-Protocol-Version`
   * header, no `initialize` handshake). The MCP spec pins this to `2025-11-25`,
   * the last revision that predates the mandatory per-request version marker,
   * so pre-2026 clients keep working unchanged.
   */
  val default: ProtocolVersion = V2025_11_25

  /** Wire identifiers for every supported version, newest first. */
  val supportedWire: Chunk[String] = all.map(_.wire)

  def parse(s: String): Option[ProtocolVersion] = all.find(_.wire == s)

  /**
   * Resolve the version a server returns from an `initialize` handshake for a
   * client's requested `protocolVersion`. Per the MCP lifecycle, echo the
   * requested version when it is a supported legacy revision; otherwise (an
   * unknown/older version, or a modern version mistakenly sent via `initialize`)
   * respond with the newest legacy revision the server supports.
   */
  def negotiateLegacy(requestedWire: String): ProtocolVersion =
    parse(requestedWire) match
      case Some(v) if !v.isStateless => v
      case _                         => latestLegacy

  given JsonEncoder[ProtocolVersion] = JsonEncoder.string.contramap(_.wire)

  given JsonDecoder[ProtocolVersion] = JsonDecoder.string.mapOrFail: s =>
    parse(s).toRight(s"Unsupported protocol version: $s")

  extension (v: ProtocolVersion)
    /** Chronological comparison via the ISO-date wire identifiers. */
    def isAtLeast(other: ProtocolVersion): Boolean = v.wire >= other.wire

    /**
     * True for revisions with no `initialize` handshake and no protocol-level
     * session: the protocol version travels in `_meta` on every request and any
     * server instance can serve any request.
     */
    def isStateless: Boolean = v.isAtLeast(V2026_07_28)

// --- Well-known `_meta` keys (SEP-2575) ---

/**
 * Reverse-domain-namespaced `_meta` keys defined by the MCP specification.
 *
 * From `2026-07-28` the connection-time handshake is gone: the protocol version,
 * client identity, client capabilities, and per-request log level ride in the
 * `_meta` object of every request, and the server echoes its identity in each
 * result's `_meta`. These constants name those keys so encoders/decoders and
 * the negotiation layer agree on the exact strings.
 */
object McpMeta:
  val ProtocolVersion: String    = "io.modelcontextprotocol/protocolVersion"
  val ClientCapabilities: String = "io.modelcontextprotocol/clientCapabilities"
  val ClientInfo: String         = "io.modelcontextprotocol/clientInfo"
  val ServerInfo: String         = "io.modelcontextprotocol/serverInfo"
  val LogLevel: String           = "io.modelcontextprotocol/logLevel"
  val SubscriptionId: String     = "io.modelcontextprotocol/subscriptionId"
  val Tasks: String              = "io.modelcontextprotocol/tasks"
  /** Progress token key — unchanged across revisions, lives at the top of `_meta`. */
  val ProgressToken: String      = "progressToken"

  /** Read a string-valued key out of an optional `_meta` object. */
  def string(meta: Option[Json.Obj], key: String): Option[String] =
    meta.flatMap(_.get(key)).flatMap(_.asString)

  /** Read the raw JSON value of a key out of an optional `_meta` object. */
  def raw(meta: Option[Json.Obj], key: String): Option[Json] =
    meta.flatMap(_.get(key))

  /** Pull the `_meta` object out of a params object, if present. */
  def of(params: Option[Json.Obj]): Option[Json.Obj] =
    params.flatMap(_.get("_meta")).flatMap(_.asObject)
