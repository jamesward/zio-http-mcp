package com.jamesward.ziohttp.mcp

import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.stream.{ZPipeline, ZStream}

/**
 * A scripted MCP server on stdin and stdout, launched as a subprocess by
 * `McpClientStdioSpec`. `FIXTURE_MODE` picks its era and quirks:
 *
 *   - `legacy`: answers `server/discover` with method-not-found and speaks the
 *     `initialize` handshake. Before answering `echo` it sends a log
 *     notification and a `ping` request, and waits for the client's reply.
 *   - `silent`: as `legacy`, but never answers `server/discover`.
 *   - `modern`: answers `server/discover` (2026-07-28 only) and rejects
 *     `initialize`; `echo` reports the protocol version from the request's `_meta`.
 *
 * Every mode has `echo` (`{"text": …}`), `slow` (never answers; a
 * `notifications/cancelled` for it is reported on stderr as `cancelled <id>`)
 * and `crash` (the process dies). With `FIXTURE_IGNORE_EOF=true` the server
 * keeps running after its stdin closes, to exercise forced shutdown, and with
 * `FIXTURE_START_DELAY_MS` it reads nothing for that long, like a slow-starting JVM.
 */
object StdioFixtureServer extends ZIOAppDefault:

  private val tools = ToolsListResult(Chunk(
    ToolDefinition(ToolName("echo"), Some("Echoes its text"), Json.Obj(
      "type" -> Json.Str("object"),
      "properties" -> Json.Obj("text" -> Json.Obj("type" -> Json.Str("string"))),
    )),
    ToolDefinition(ToolName("slow"), Some("Never answers"), Json.Obj("type" -> Json.Str("object"))),
    ToolDefinition(ToolName("crash"), Some("Exits the server"), Json.Obj("type" -> Json.Str("object"))),
  ))

  private val serverInfo = Implementation("stdio-fixture", "1.0.0")

  /** The server's side of one connection: serialized writes, and its own requests awaiting the client. */
  private final class Connection(mode: String, writeLock: Semaphore, waiting: Ref[Map[String, Promise[Nothing, Json.Obj]]]):

    private def send(message: Json.Obj): UIO[Unit] = writeLock.withPermit(Console.printLine(message.toJson).orDie)

    private def reply(id: Json, result: Json): UIO[Unit] =
      send(Json.Obj("jsonrpc" -> Json.Str("2.0"), "id" -> id, "result" -> result))

    private def error(id: Json, code: Int, message: String): UIO[Unit] =
      send(Json.Obj("jsonrpc" -> Json.Str("2.0"), "id" -> id, "error" -> Json.Obj("code" -> Json.Num(code), "message" -> Json.Str(message))))

    private def text(s: String, structured: Json.Obj): Json =
      CallToolResult(Chunk(ToolContent.Text(s)), Some(structured)).toJsonAST.toOption.get

    /** Sends the client a request and waits for its answer. */
    private def ask(id: String, method: String): UIO[Option[Json.Obj]] =
      for
        answer <- Promise.make[Nothing, Json.Obj]
        _      <- waiting.update(_.updated(id, answer))
        _      <- send(Json.Obj("jsonrpc" -> Json.Str("2.0"), "id" -> Json.Str(id), "method" -> Json.Str(method)))
        got    <- answer.await.timeout(5.seconds)
      yield got

    def handle(message: Json.Obj): UIO[Unit] =
      val method = message.get("method").flatMap(_.asString)
      val params = message.get("params").flatMap(_.asObject).getOrElse(Json.Obj())
      (method, message.get("id")) match
        case (Some(m), Some(id)) => request(id, m, params).forkDaemon.unit
        case (Some("notifications/cancelled"), None) =>
          Console.printLineError(s"cancelled ${params.get("requestId").fold("?")(_.toJson)}").orDie
        case (Some(_), None) => ZIO.unit
        case (None, Some(Json.Str(id))) =>
          waiting.modify(w => w.get(id) -> (w - id)).flatMap(_.fold(ZIO.unit)(_.succeed(message).unit))
        case _ => ZIO.unit

    private def request(id: Json, method: String, params: Json.Obj): UIO[Unit] =
      method match
        case "server/discover" =>
          mode match
            case "modern" =>
              reply(id, Json.Obj(
                "supportedVersions" -> Json.Arr(Json.Str("2026-07-28")),
                "capabilities" -> Json.Obj("tools" -> Json.Obj()),
                "_meta" -> Json.Obj(McpMeta.ServerInfo -> serverInfo.toJsonAST.toOption.get),
              ))
            case "silent" => ZIO.unit
            case _        => error(id, ErrorCode.MethodNotFound.code, "Method not found: server/discover")
        case "initialize" if mode == "modern" => error(id, ErrorCode.MethodNotFound.code, "Method not found: initialize")
        case "initialize" =>
          reply(id, InitializeResult("2025-11-25", ServerCapabilities(tools = Some(Json.Obj())), serverInfo).toJsonAST.toOption.get)
        case "ping"       => reply(id, Json.Obj())
        case "tools/list" => reply(id, tools.toJsonAST.toOption.get)
        case "tools/call" =>
          val arguments = params.get("arguments").flatMap(_.asObject).getOrElse(Json.Obj())
          params.get("name").flatMap(_.asString) match
            case Some("echo") =>
              val echoed = arguments.get("text").flatMap(_.asString).getOrElse("")
              if mode == "modern" then
                val version = params.get("_meta").flatMap(_.asObject).flatMap(_.get(McpMeta.ProtocolVersion)).getOrElse(Json.Null)
                reply(id, text(s"echo: $echoed", Json.Obj("protocolVersion" -> version)))
              else
                for
                  _      <- send(Json.Obj(
                              "jsonrpc" -> Json.Str("2.0"),
                              "method" -> Json.Str("notifications/message"),
                              "params" -> Json.Obj("level" -> Json.Str("info"), "data" -> Json.Str("echoing")),
                            ))
                  pinged <- ask(s"server-ping-${id.toJson}", "ping")
                  _      <- reply(id, text(s"echo: $echoed", Json.Obj("pinged" -> Json.Bool(pinged.exists(_.get("result").isDefined)))))
                yield ()
            case Some("slow")  => ZIO.unit
            case Some("crash") => ZIO.succeed(java.lang.Runtime.getRuntime.halt(3))
            case other         => error(id, ErrorCode.InvalidParams.code, s"unknown tool: $other")
        case other => error(id, ErrorCode.MethodNotFound.code, s"Method not found: $other")

  def run =
    for
      mode      <- System.env("FIXTURE_MODE").map(_.getOrElse("legacy"))
      ignoreEof <- System.env("FIXTURE_IGNORE_EOF").map(_.contains("true"))
      delay     <- System.env("FIXTURE_START_DELAY_MS").map(_.flatMap(_.toLongOption).getOrElse(0L))
      _         <- ZIO.sleep(delay.millis)
      writeLock <- Semaphore.make(1)
      waiting   <- Ref.make(Map.empty[String, Promise[Nothing, Json.Obj]])
      connection = Connection(mode, writeLock, waiting)
      _         <- ZStream.fromInputStream(java.lang.System.in)
                     .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
                     .filter(_.nonEmpty)
                     .mapZIO(line => ZIO.fromEither(line.fromJson[Json.Obj]).mapError(e => RuntimeException(s"bad line: $e")))
                     .foreach(connection.handle)
      _         <- ZIO.never.when(ignoreEof)
    yield ()
