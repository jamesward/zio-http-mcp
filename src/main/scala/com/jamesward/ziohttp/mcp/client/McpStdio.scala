package com.jamesward.ziohttp.mcp.client

import com.jamesward.ziohttp.mcp.*
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.process.{Command, Process, ProcessInput}
import zio.stream.ZStream

import java.nio.charset.StandardCharsets
import java.nio.file.Path

/**
 * How to launch and talk to an MCP server over the stdio transport.
 *
 * @param command          the server executable
 * @param args             its arguments
 * @param env              environment variables added to the client's own
 * @param workingDirectory the server's working directory; the client's when unset
 * @param clientInfo       identity reported to the server
 * @param preferredVersion as for [[McpClientConfig.preferredVersion]]: a modern
 *                         preference probes with `server/discover` and falls
 *                         back to the `initialize` handshake for a legacy server
 * @param onInputRequest   as for [[McpClientConfig.onInputRequest]]
 * @param stderr           receives each line the server writes to stderr, which
 *                         the stdio transport reserves for logs
 * @param discoverTimeout  how long the `server/discover` probe waits before
 *                         treating the server as legacy: some legacy servers
 *                         never answer an unknown pre-`initialize` request
 * @param shutdownTimeout  on close, how long to wait for the server to exit
 *                         after its stdin closes, and then after it is
 *                         terminated, before it is killed
 */
final case class McpStdioConfig(
  command: String,
  args: Chunk[String] = Chunk.empty,
  env: Map[String, String] = Map.empty,
  workingDirectory: Option[Path] = None,
  clientInfo: Implementation = Implementation("zio-http-mcp-client", "0.1.0"),
  preferredVersion: ProtocolVersion = ProtocolVersion.latest,
  onInputRequest: Option[InputRequest => IO[McpClientError, Json]] = None,
  stderr: String => UIO[Unit] = line => ZIO.logDebug(s"MCP server stderr: $line"),
  discoverTimeout: Duration = 5.seconds,
  shutdownTimeout: Duration = 5.seconds,
)

/**
 * The stdio wire: newline-delimited JSON-RPC over a subprocess's stdin and
 * stdout. Responses are matched to requests by id; an interrupted request is
 * cancelled with `notifications/cancelled`. A legacy server may also send
 * requests: `ping` is answered and anything else gets method-not-found, since
 * the client declares no client-side capabilities.
 *
 * `pending` is `None` once the server's stdout has ended, so a request made
 * after that fails at once instead of waiting forever.
 */
private[client] final class StdioWire private (
  stdin: Queue[Option[Chunk[Byte]]],
  pending: Ref[Option[Map[RequestId, Promise[McpClientError, Json]]]],
) extends McpWire:
  import StdioWire.*

  def request(o: Outgoing): IO[McpClientError, Json] =
    for
      promise <- Promise.make[McpClientError, Json]
      _       <- pending.modify:
                   case Some(waiting) => ZIO.unit -> Some(waiting.updated(o.id, promise))
                   case None          => ZIO.fail(serverGone) -> None
                 .flatten
      result  <- (write(o.body) *> promise.await)
                   .onInterrupt(forget(o.id) *> cancel(o.id))
    yield result

  def notify(method: String, body: String): IO[McpClientError, Unit] = write(body)

  // the process is stopped by the scope that launched it
  def close: IO[McpClientError, Unit] = ZIO.unit

  private def write(message: String): UIO[Unit] =
    stdin.offer(Some(Chunk.fromArray((message + "\n").getBytes(StandardCharsets.UTF_8)))).unit

  private def forget(id: RequestId): UIO[Unit] = pending.update(_.map(_ - id))

  private def cancel(id: RequestId): UIO[Unit] =
    val params = Json.Obj("requestId" -> id.asJson, "reason" -> Json.Str("the client abandoned the request"))
    write((JsonRpcMessage.Notification("notifications/cancelled", Some(params)): JsonRpcMessage).toJson)

  /** Handles one line of the server's stdout. */
  private def receive(line: String): UIO[Unit] =
    line.fromJson[Json.Obj] match
      case Left(error) => ZIO.logWarning(s"MCP server wrote a line that is not a JSON-RPC message ($error): ${line.take(200)}")
      case Right(obj) =>
        val method = obj.get("method").flatMap(_.asString)
        val id = obj.get("id").flatMap(_.as[RequestId].toOption)
        (method, id) match
          case (Some(m), Some(requestId)) => answer(requestId, m)
          case (Some(m), None)            => ZIO.logDebug(s"MCP server notification: $m")
          case (None, Some(requestId))    =>
            pending.modify(waiting => waiting.flatMap(_.get(requestId)) -> waiting.map(_ - requestId)).flatMap:
              case Some(promise) => promise.complete(ZIO.fromEither(McpClient.extractResult(obj))).unit
              case None          => ZIO.logDebug(s"MCP server answered a request that is no longer waiting: $requestId")
          case (None, None) => ZIO.logWarning(s"MCP server sent a message with neither a method nor an id: ${line.take(200)}")

  /** Answers a request from a legacy server. */
  private def answer(id: RequestId, method: String): UIO[Unit] =
    val reply =
      if method == "ping" then JsonRpcResponse(id, Json.Obj()).toJson
      else JsonRpcError.fromCode(Some(id), ErrorCode.MethodNotFound, s"Method not found: $method").toJson
    write(reply)

  /** The server's stdout ended: nothing more can be answered. */
  private def serverClosed: UIO[Unit] =
    pending.getAndSet(None).flatMap(waiting => ZIO.foreachDiscard(waiting.toList.flatMap(_.values))(_.fail(serverGone)))

private[client] object StdioWire:

  private val serverGone = McpClientError.Transport("the MCP server process closed its stdout")

  /** A started server, its stdin, and the fibers pumping its stdout and stderr. */
  private final case class Running(process: Process, stdin: Queue[Option[Chunk[Byte]]], pumps: Fiber[Nothing, Any])

  /** Starts the server for the life of the scope. */
  def launch(config: McpStdioConfig): ZIO[Scope, McpClientError, StdioWire] =
    for
      stdin   <- Queue.unbounded[Option[Chunk[Byte]]]
      pending <- Ref.make(Option(Map.empty[RequestId, Promise[McpClientError, Json]]))
      wire     = StdioWire(stdin, pending)
      _       <- ZIO.acquireRelease(start(config, stdin, wire))(stop(_, config.shutdownTimeout))
    yield wire

  /**
   * Starts the process with stdin fed from `stdin` (`None` closes it), and pumps
   * its stdout and stderr on fibers that belong to the process rather than to a
   * reader: a read blocked on a pipe can't be interrupted, so nothing may wait on
   * one. The pumps end at end of file, once the process has exited.
   */
  private def start(config: McpStdioConfig, stdin: Queue[Option[Chunk[Byte]]], wire: StdioWire): IO[McpClientError, Running] =
    val input = ProcessInput.fromStream(ZStream.fromQueue(stdin).collectWhileSome.flattenChunks, flushChunksEagerly = true)
    val command = Command(config.command, config.args*).env(config.env).stdin(input)
    for
      process <- config.workingDirectory.fold(command)(dir => command.workingDirectory(dir.toFile)).run
                   .mapError(e => McpClientError.Transport(s"could not start MCP server '${config.command}': ${e.getMessage}", Some(e)))
      out     <- nonBlank(process.stdout.linesStream).foreach(wire.receive)
                   .catchAllCause(cause => ZIO.logDebugCause("MCP server stdout ended with an error", cause))
                   .ensuring(wire.serverClosed)
                   .forkDaemon
      err     <- nonBlank(process.stderr.linesStream).foreach(config.stderr)
                   .catchAllCause(cause => ZIO.logDebugCause("MCP server stderr ended with an error", cause))
                   .forkDaemon
    yield Running(process, stdin, out.zip(err))

  private def nonBlank[E](lines: ZStream[Any, E, String]): ZStream[Any, E, String] =
    lines.filter(_.exists(!_.isWhitespace))

  /**
   * The stdio transport's shutdown: close stdin and wait for the server to
   * exit, terminate it (SIGTERM) after `timeout`, and kill it after another.
   *
   * This runs as a finalizer, which is uninterruptible, and so is anything it
   * races: a timeout could not stop the wait. Each wait is therefore made
   * interruptible on its own (a pending interruption of the fiber closing the
   * scope does not reach it, so the cleanup still runs), and it waits on the
   * process's exit future rather than on zio-process's `exitCode`, which
   * blocks a thread in `waitFor` until the process exits.
   */
  private def stop(running: Running, timeout: Duration): UIO[Unit] =
    val process = running.process
    val exited: UIO[Boolean] =
      ZIO.interruptible(process.execute(_.onExit().nn).flatMap(ZIO.fromCompletableFuture(_)))
        .timeout(timeout).map(_.isDefined).orElseSucceed(false)
    (for
      _      <- running.stdin.offer(None)
      inTime <- exited
      _      <- ZIO.unless(inTime):
                  for
                    _          <- ZIO.logWarning(s"MCP server did not exit within $timeout of its stdin closing; terminating it")
                    _          <- process.execute(_.destroy())
                    terminated <- exited
                    _          <- process.execute(_.destroyForcibly()).unless(terminated)
                  yield ()
      _      <- ZIO.interruptible(running.pumps.await).timeout(timeout)
    yield ()).ignoreLogged
