package com.jamesward.ziohttp.mcp.auth

import com.jamesward.ziohttp.mcp.*
import zio.*
import zio.http.*
import zio.json.*

/**
 * Bearer-token protection for a server that is not an OAuth resource server.
 *
 * A request that doesn't carry an accepted `Authorization: Bearer <token>` is answered with HTTP 200 and a
 * JSON-RPC error (`-32001`, [[ErrorCode.Unauthorized]]) naming the problem, never with HTTP 401. A client treats a
 * 401 as "this server uses OAuth": claude.ai offers "Sign in now (Detected)" and probes the
 * `.well-known/oauth-*` documents; Claude Code reports "Dynamic Client Registration rejected". With a JSON-RPC
 * error on `initialize` the client shows the message instead, which says what is missing. No `WWW-Authenticate`
 * challenge and no metadata endpoint is involved. Use it for servers whose callers hold a pre-shared token
 * (a header the client is configured with, or a token a proxy adds); use [[McpAuth]] for OAuth.
 *
 * {{{
 * server.mcpStatelessRoutes @@ StaticBearer.aspect(token => ZIO.succeed(token == expected))
 * }}}
 *
 * A notification without a token gets `202 Accepted` and is dropped; a `GET` or `DELETE` without a token gets
 * `405` (the SSE stream and session teardown need the token too, and a `405` is what a server without them says).
 */
object StaticBearer:
  private given CanEqual[Method, Method] = CanEqual.derived

  val MissingMessage: String =
    "Not authenticated: send an `Authorization: Bearer <token>` header with each request. This server does not use OAuth."
  val RejectedMessage: String =
    "Not authenticated: the bearer token was not accepted. This server does not use OAuth."

  def aspect[R](verify: String => URIO[R, Boolean]): HandlerAspect[R, Unit] =
    HandlerAspect.interceptIncomingHandler(Handler.fromFunctionZIO[Request]: req =>
      bearer(req) match
        case None => rejection(req, MissingMessage).flatMap(ZIO.fail(_))
        case Some(token) =>
          verify(token).flatMap: accepted =>
            if accepted then ZIO.succeed((req, ())) else rejection(req, RejectedMessage).flatMap(ZIO.fail(_)))

  private def bearer(req: Request): Option[String] =
    req.rawHeader("Authorization").flatMap: h =>
      Option.when(h.regionMatches(true, 0, "Bearer ", 0, 7))(h.drop(7).trim).filter(_.nonEmpty)

  private def rejection(req: Request, message: String): UIO[Response] =
    req.method match
      case Method.POST =>
        req.body.asString.orElseSucceed("").map: text =>
          text.fromJson[JsonRpcMessage] match
            case Right(JsonRpcMessage.Notification(_, _)) => Response.status(Status.Accepted)
            case Right(JsonRpcMessage.Request(id, _, _)) => error(Some(id), message)
            case Left(_)                                 => error(None, message)
      case _ => ZIO.succeed(Response.status(Status.MethodNotAllowed))

  private def error(id: Option[RequestId], message: String): Response =
    Response.json(JsonRpcError(id, ErrorDetail(ErrorCode.Unauthorized.code, message)).toJson)
