package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.auth.Principal
import zio.*
import zio.http.{Header, MediaType, Response}
import zio.json.*
import zio.json.ast.Json
import zio.stream.{ZPipeline, ZStream}

import java.io.{InputStream, OutputStream}
import java.nio.charset.StandardCharsets

/**
 * The stdio transport's wire for a server, over any byte stream: newline-delimited
 * JSON-RPC messages, read from `input` and written to `output`. The same framing
 * serves a server launched as a subprocess (its stdin/stdout, [[McpServer.serveStdio]])
 * and a connection to a long-running server (a Unix domain or TCP socket,
 * [[McpServer.serveStream]]), as the transport specification recommends.
 *
 * Every request is served statelessly by the server's own dispatch, as over
 * Streamable HTTP, and concurrently: a slow tool call doesn't hold up the next
 * request. Request-scoped notifications (progress, log messages) are written as
 * they happen, followed by the response. `notifications/cancelled` interrupts the
 * request it names, and nothing more is written for it. Other notifications, and
 * responses (the server sends no requests), are read and ignored.
 *
 * There is no HTTP layer, so neither origin checks nor HTTP authentication apply
 * (the stdio transport takes credentials from its environment). A server with
 * [[McpServer.auth]] configured sees every request as made by `principal`.
 */
object McpStreamServer:

  /**
   * Serve the messages read from `input` until it ends (or fails), writing to
   * `output`. Requests still running then are interrupted: the stdio transport's
   * shutdown is closing the server's input. Reading blocks a thread until data or
   * end of input arrives, so close `input` to stop serving.
   */
  def serve[R](
    server: McpServer[R],
    input: InputStream,
    output: OutputStream,
    principal: Option[Principal] = None,
    pathParams: Map[String, String] = Map.empty,
  ): ZIO[R, Throwable, Unit] =
    for
      lock     <- Semaphore.make(1)
      inFlight <- Ref.make(Map.empty[RequestId, Fiber.Runtime[Nothing, Unit]])
      write     = (line: String) => lock.withPermit(ZIO.attemptBlocking {
                    output.write((line + "\n").getBytes(StandardCharsets.UTF_8))
                    output.flush()
                  })
      _        <- lines(input)
                    .mapZIO(line => receive(server, line, write, inFlight, principal, pathParams))
                    .runDrain
                    .ensuring(inFlight.getAndSet(Map.empty).flatMap(fibers => ZIO.foreachDiscard(fibers.values)(_.interruptFork)))
    yield ()

  private def lines(input: InputStream): ZStream[Any, Throwable, String] =
    ZStream
      .fromInputStream(input, chunkSize = 8192)
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
      .filter(_.exists(!_.isWhitespace))

  private def receive[R](
    server: McpServer[R],
    line: String,
    write: String => Task[Unit],
    inFlight: Ref[Map[RequestId, Fiber.Runtime[Nothing, Unit]]],
    principal: Option[Principal],
    pathParams: Map[String, String],
  ): ZIO[R, Throwable, Unit] =
    line.fromJson[Json.Obj] match
      case Left(error) =>
        write(JsonRpcError.fromCode(None, ErrorCode.ParseError, s"Parse error: $error").toJson)
      case Right(message) =>
        val method = message.get("method").flatMap(_.asString)
        val id     = message.get("id").flatMap(_.as[RequestId].toOption)
        (method, id) match
          case (Some("notifications/cancelled"), None) =>
            val target = message.get("params").flatMap(_.asObject).flatMap(_.get("requestId")).flatMap(_.as[RequestId].toOption)
            ZIO.foreachDiscard(target)(requestId =>
              inFlight.modify(m => m.get(requestId) -> (m - requestId)).flatMap(ZIO.foreachDiscard(_)(_.interruptFork))
            )
          case (Some(_), None) => ZIO.unit // other notifications need no answer
          case (None, _)       => ZIO.unit // a response: this server sends no requests
          case (Some(_), Some(requestId)) =>
            for
              // The fiber removes itself when done; cancellation removes it first.
              done  <- Promise.make[Nothing, Unit]
              fiber <- (done.await *> respond(server, requestId, message, write, principal, pathParams))
                         .catchAllCause(cause => ZIO.logWarningCause(s"MCP request $requestId failed", cause))
                         .ensuring(inFlight.update(_ - requestId))
                         // Not the reader's child: requests are interrupted explicitly (cancelled, or
                         // the input ended) without waiting for a blocked one to notice.
                         .forkDaemon
              _     <- inFlight.update(_.updated(requestId, fiber)) *> done.succeed(())
            yield ()

  /** Dispatch one request and write what the server answers. */
  private def respond[R](
    server: McpServer[R],
    id: RequestId,
    message: Json.Obj,
    write: String => Task[Unit],
    principal: Option[Principal],
    pathParams: Map[String, String],
  ): ZIO[R, Throwable, Unit] =
    server.streamDispatch(message, principal, pathParams).flatMap: response =>
      val eventStream = response.header(Header.ContentType).exists(_.mediaType.matches(MediaType.text.`event-stream`))
      if eventStream then
        // Write each SSE event's message as it arrives, up to the response to this
        // request: a legacy stream would otherwise stay open with keep-alives.
        sseMessages(response.body.asStream)
          .mapZIO(json => write(compact(json)).as(isResponseTo(json, id)))
          .takeUntil(identity)
          .runDrain
      else
        response.body.asString.flatMap: body =>
          if body.exists(!_.isWhitespace) then
            body.fromJson[Json] match
              case Right(json) => write(json.toJson)
              case Left(_)     => write(httpError(id, response, body))
          else if response.status.isSuccess then ZIO.unit
          else write(httpError(id, response, body))

  /** The `data` payloads of a stream of server-sent events. */
  private def sseMessages(bytes: ZStream[Any, Throwable, Byte]): ZStream[Any, Throwable, Json] =
    bytes
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
      .map(_.stripSuffix("\r"))
      .mapAccum(Chunk.empty[String]) { (data, line) =>
        if line.isEmpty then (Chunk.empty, Option.when(data.nonEmpty)(data.mkString("\n")))
        else if line.startsWith("data:") then (data :+ line.drop(5).stripPrefix(" "), None)
        else (data, None) // event names, ids, comments (keep-alives)
      }
      .collectSome
      .map(_.fromJson[Json])
      .collect { case Right(json) => json }

  private def isResponseTo(json: Json, id: RequestId): Boolean =
    json.asObject.exists(o => o.get("method").isEmpty && o.get("id").contains(id.asJson))

  private def compact(json: Json): String = json.toJson

  /** A JSON-RPC error for an HTTP-level failure that carried no JSON-RPC message. */
  private def httpError(id: RequestId, response: Response, body: String): String =
    val detail = if body.isBlank then "" else s": ${body.take(200)}"
    JsonRpcError.fromCode(Some(id), ErrorCode.InternalError, s"request failed (HTTP ${response.status.code})$detail").toJson
