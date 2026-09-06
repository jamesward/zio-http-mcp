package com.jamesward.ziohttp.mcp

import zio.*
import zio.json.*
import zio.json.ast.Json

/**
 * Wire helpers for the modern (2026-07-28) protocol envelope.
 *
 * The modern revision layers three things onto every result that the legacy
 * (2025-11-25) revision does not have:
 *
 *   - a required `resultType` discriminator (`"complete"` for an ordinary
 *     result, `"input_required"` for a Multi Round-Trip Request interim
 *     result),
 *   - the server's identity echoed in `_meta`
 *     (`io.modelcontextprotocol/serverInfo`), which servers SHOULD include, and
 *   - freshness hints (`ttlMs`, `cacheScope`) on cacheable results
 *     (`tools/list`, `prompts/list`, `resources/list`,
 *     `resources/templates/list`, `resources/read`, `server/discover`).
 *
 * Rather than give every result case class a custom codec, the handlers encode
 * their ordinary result value and this object augments the resulting JSON. For
 * a legacy response the augmentation is skipped, so the legacy wire format is
 * byte-for-byte unchanged.
 */
object ModernEnvelope:

  val ResultTypeComplete: String      = "complete"
  val ResultTypeInputRequired: String = "input_required"

  /** Default freshness hint for cacheable list/read results: one hour, public. */
  val DefaultTtlMs: Long        = 3600000L
  val DefaultCacheScope: String = "public"

  /**
   * Augment an ordinary result object for a modern response: stamp
   * `resultType: "complete"`, echo `serverInfo` in `_meta`, and (when the
   * method is cacheable) add `ttlMs` / `cacheScope`. Existing fields are never
   * overwritten, so a handler that already set any of these wins.
   */
  def complete(result: Json.Obj, serverInfo: Implementation, cacheable: Boolean): Json.Obj =
    complete(result, serverInfo, if cacheable then McpCachePolicy.Default else McpCachePolicy.NotCacheable)

  def complete(result: Json.Obj, serverInfo: Implementation, cachePolicy: McpCachePolicy): Json.Obj =
    val withType = putIfAbsent(result, "resultType", Json.Str(ResultTypeComplete))
    val withMeta = withServerInfo(withType, serverInfo)
    cachePolicy match
      case McpCachePolicy.NotCacheable => withMeta
      case McpCachePolicy.Cacheable(ttl, scope) =>
        val withTtl = putIfAbsent(withMeta, "ttlMs", Json.Num(ttl.milliseconds))
        putIfAbsent(withTtl, "cacheScope", Json.Str(scope.wire))

  /** Build an extension result envelope whose protocol-owned fields cannot be
    * supplied or overridden by the extension handler. */
  private[mcp] def completeServerOwned(
    result: Json.Obj,
    serverInfo: Implementation,
    cachePolicy: McpCachePolicy,
  ): Json.Obj =
    val withoutEnvelope = Json.Obj(result.fields.filterNot: (key, _) =>
      key == "resultType" || key == "ttlMs" || key == "cacheScope"
    )
    val withoutServerInfo = withoutEnvelope.get("_meta").flatMap(_.asObject) match
      case Some(meta) =>
        put(withoutEnvelope, "_meta", Json.Obj(meta.fields.filterNot(_._1 == McpMeta.ServerInfo)))
      case None => withoutEnvelope
    complete(withoutServerInfo, serverInfo, cachePolicy)

  /** Merge `io.modelcontextprotocol/serverInfo` into the result's `_meta`,
    * preserving any `_meta` the handler already produced. */
  def withServerInfo(result: Json.Obj, serverInfo: Implementation): Json.Obj =
    // Serialize via Implementation's own encoder so SEP-973 `title`/`icons`/
    // `websiteUrl` ride along (a client reads server identity from here); the
    // encoder omits them when unset, so the base case stays `{name, version}`.
    val infoJson = serverInfo.toJsonAST.toOption.flatMap(_.asObject).getOrElse(
      Json.Obj(Chunk("name" -> Json.Str(serverInfo.name), "version" -> Json.Str(serverInfo.version)))
    )
    val existingMeta = result.get("_meta").flatMap(_.asObject).getOrElse(Json.Obj())
    val mergedMeta = putIfAbsent(existingMeta, McpMeta.ServerInfo, infoJson)
    put(result, "_meta", mergedMeta)

  private def put(obj: Json.Obj, key: String, value: Json): Json.Obj =
    Json.Obj(obj.fields.filterNot(_._1 == key) :+ (key -> value))

  private def putIfAbsent(obj: Json.Obj, key: String, value: Json): Json.Obj =
    if obj.get(key).isDefined then obj else Json.Obj(obj.fields :+ (key -> value))

// --- Discover ---

/**
 * The result of `server/discover`. Encoded directly to the modern envelope: the
 * server's identity rides in `_meta.io.modelcontextprotocol/serverInfo` and the
 * result is cacheable (`ttlMs` / `cacheScope`).
 */
final case class DiscoverResult(
  supportedVersions: Chunk[String],
  capabilities: ServerCapabilities,
  serverInfo: Implementation,
  instructions: Option[String] = None,
):
  def toResultJson: Json.Obj =
    val serverInfoJson = serverInfo.toJsonAST.toOption.flatMap(_.asObject).getOrElse(
      Json.Obj(Chunk("name" -> Json.Str(serverInfo.name), "version" -> Json.Str(serverInfo.version)))
    )
    val base = Chunk[(String, Json)](
      "supportedVersions" -> Json.Arr(supportedVersions.map(Json.Str(_))),
      "capabilities"      -> capabilities.toJsonAST.getOrElse(Json.Obj()),
      // Emit `serverInfo` at the TOP LEVEL as well as in `_meta` (below). The
      // current spec (and MCP SDK v2 beta.5+) reads server identity only from
      // `_meta.io.modelcontextprotocol/serverInfo`, but MCP SDK v2 beta.1–beta.4
      // clients require a top-level `serverInfo` object on the DiscoverResult and
      // reject the response without it (silently falling back to legacy, which in
      // a pinned client surfaces as "server did not offer <version>"). Sending
      // both satisfies every client version.
      "serverInfo"        -> serverInfoJson,
    ) ++ instructions.fold(Chunk.empty[(String, Json)])(i => Chunk("instructions" -> Json.Str(i)))
    ModernEnvelope.complete(Json.Obj(base), serverInfo, cacheable = true)

object DiscoverResult:
  given CanEqual[DiscoverResult, DiscoverResult] = CanEqual.derived

// --- Multi Round-Trip Requests (MRTR, SEP-2322) ---

/**
 * One server-to-client input request embedded in an [[InputRequiredResult]].
 * Replaces the server-initiated `sampling/createMessage`, `elicitation/create`,
 * and `roots/list` requests of earlier revisions: instead of the server pushing
 * a JSON-RPC request, it returns the request(s) it needs answered and the
 * client retries the original call with matching input responses.
 *
 * On the wire the requests are a JSON object keyed by correlation id — the `id`
 * here is that key, not a field of the value — and the client echoes the same
 * keys in `inputResponses`:
 *
 * {{{
 * "inputRequests": { "user_name": { "method": "elicitation/create", "params": {...} } }
 * }}}
 *
 * @param id     correlation id, unique within this result; the object key
 * @param method the input method: `sampling/createMessage`, `elicitation/create`, or `roots/list`
 * @param params the parameters the client needs to fulfil the request
 */
final case class InputRequest(id: String, method: String, params: Json.Obj):
  /** This request as its `inputRequests` entry: the id keys a `{method, params}` value. */
  def toEntry: (String, Json) =
    id -> Json.Obj(Chunk("method" -> Json.Str(method), "params" -> (params: Json)))

object InputRequest:
  given CanEqual[InputRequest, InputRequest] = CanEqual.derived

  /** Read an `inputRequests` object back into requests, keeping wire order. */
  def parseAll(inputRequests: Json): Chunk[InputRequest] =
    inputRequests.asObject.fold(Chunk.empty): obj =>
      obj.fields.flatMap: (id, value) =>
        for
          fields <- value.asObject
          method <- fields.get("method").flatMap(_.asString)
        yield InputRequest(id, method, fields.get("params").flatMap(_.asObject).getOrElse(Json.Obj()))

/** A client's answer to one [[InputRequest]], sent back in the retry's
  * `inputResponses` keyed by the request's id. `result` is the payload the
  * corresponding server request would have returned (e.g. the sampled message
  * or the elicitation result). */
final case class InputResponse(id: String, result: Json)

object InputResponse:
  given CanEqual[InputResponse, InputResponse] = CanEqual.derived

  /** Render answers as the `inputResponses` object: `{ "<id>": <result> }`. */
  def toJson(responses: Chunk[InputResponse]): Json.Obj =
    Json.Obj(responses.map(r => r.id -> r.result))

/**
 * Interim result signalling that the server needs input before it can complete
 * the original request. Carries `resultType: "input_required"` and the
 * `inputRequests` the client must fulfil.
 *
 * `requestState` is opaque server state echoed back verbatim on the retry so a
 * stateless server can resume where it left off. It is a string on the wire:
 * the server alone decides what it encodes, and — since it travels through the
 * client — signs it so tampering is detectable (see `McpServer`).
 */
final case class InputRequiredResult(
  inputRequests: Chunk[InputRequest],
  requestState: Option[String] = None,
):
  def toResultJson(serverInfo: Implementation): Json.Obj =
    val base = Chunk[(String, Json)](
      "resultType"    -> Json.Str(ModernEnvelope.ResultTypeInputRequired),
      "inputRequests" -> Json.Obj(inputRequests.map(_.toEntry)),
    ) ++ requestState.fold(Chunk.empty[(String, Json)])(s => Chunk("requestState" -> Json.Str(s)))
    ModernEnvelope.withServerInfo(Json.Obj(base), serverInfo)

object InputRequiredResult:
  given CanEqual[InputRequiredResult, InputRequiredResult] = CanEqual.derived
