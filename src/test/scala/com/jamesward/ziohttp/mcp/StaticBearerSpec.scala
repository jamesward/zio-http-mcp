package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.auth.StaticBearer
import zio.*
import zio.http.*
import zio.schema.*
import zio.test.*

/** The "Bearer-only servers (no OAuth)" examples in the README. */
object StaticBearerSpec extends ZIOSpecDefault:
  case class NameInput(name: String) derives Schema

  private val expected = "s3cret"

  private val server = McpServer("bearer-only", "1.0.0")
    .tool(McpTool("greet").description("Greets someone").handle: (input: NameInput) =>
      ZIO.succeed(s"Hello, ${input.name}!"))

  private val routes = server.mcpStatelessRoutes @@ StaticBearer.aspect(token => ZIO.succeed(token == expected))
  private val openRoutes = server.mcpStatelessRoutes @@ StaticBearer.aspect(token => ZIO.succeed(token == expected), open = StaticBearer.Handshake)

  private def post(body: String, auth: Option[String], routes: Routes[Any, Response] = routes) =
    val headers = Headers("Content-Type", "application/json") ++ Headers("Accept", "application/json, text/event-stream") ++
      auth.fold(Headers.empty)(a => Headers("Authorization", a))
    for
      url <- ZIO.fromEither(URL.decode("/mcp"))
      response <- routes.runZIO(Request.post(url, Body.fromString(body)).addHeaders(headers))
      text <- response.body.asString
    yield (response, text)

  private val initialize =
    """{"jsonrpc":"2.0","id":7,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"t","version":"1"}}}"""

  def spec = suite("StaticBearer")(
    test("a request without a token gets HTTP 200 and a JSON-RPC error, never a 401 or a challenge") {
      for
        (res, text) <- post(initialize, None)
      yield assertTrue(
        res.status == Status.Ok,
        res.headers.get("WWW-Authenticate").isEmpty,
        text.contains("\"id\":7"),
        text.contains("\"code\":-32001"),
        text.contains("Authorization: Bearer"),
        !text.contains("\"result\""),
      )
    },
    test("tools/list and tools/call without a token are refused the same way, and echo a string id") {
      for
        (list, listText) <- post("""{"jsonrpc":"2.0","id":"a1","method":"tools/list"}""", None)
        (call, callText) <- post("""{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"greet","arguments":{"name":"x"}}}""", Some("Basic abc"))
      yield assertTrue(list.status == Status.Ok, listText.contains("\"id\":\"a1\""), listText.contains("-32001"), call.status == Status.Ok, !callText.contains("Hello"))
    },
    test("a wrong token says it was not accepted") {
      for
        (res, text) <- post(initialize, Some("Bearer nope"))
      yield assertTrue(res.status == Status.Ok, res.headers.get("WWW-Authenticate").isEmpty, text.contains("was not accepted"))
    },
    test("the right token reaches the server") {
      for
        (init, initText) <- post(initialize, Some(s"Bearer $expected"))
        (call, callText) <- post("""{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"greet","arguments":{"name":"Ada"}}}""", Some(s"bearer $expected"))
      yield assertTrue(init.status == Status.Ok, initText.contains("\"result\""), callText.contains("Hello, Ada!"))
    },
    test("a notification without a token is accepted and dropped; a GET is a 405") {
      for
        (note, _) <- post("""{"jsonrpc":"2.0","method":"notifications/initialized"}""", None)
        url <- ZIO.fromEither(URL.decode("/mcp"))
        get <- routes.runZIO(Request.get(url))
        junk <- post("not json", None)
      yield assertTrue(note.status == Status.Accepted, get.status == Status.MethodNotAllowed, junk._1.status == Status.Ok, junk._2.contains("-32001"))
    },
    test("with open = Handshake the handshake works without a token and everything else still needs one") {
      for
        (init, initText) <- post(initialize, None, openRoutes)
        (ping, pingText) <- post("""{"jsonrpc":"2.0","id":5,"method":"ping"}""", None, openRoutes)
        (list, listText) <- post("""{"jsonrpc":"2.0","id":6,"method":"tools/list"}""", None, openRoutes)
        (_, wrongText) <- post(initialize, Some("Bearer nope"), openRoutes)
        (_, goodText) <- post("""{"jsonrpc":"2.0","id":8,"method":"tools/list"}""", Some(s"Bearer $expected"), openRoutes)
      yield assertTrue(
        init.status == Status.Ok, initText.contains("\"result\""), initText.contains("bearer-only"), initText.contains("\"id\":7"),
        ping.status == Status.Ok, pingText.contains("\"result\""),
        list.status == Status.Ok, listText.contains("-32001"), !listText.contains("greet"), list.headers.get("WWW-Authenticate").isEmpty,
        wrongText.contains("was not accepted"),
        goodText.contains("greet"),
      )
    },
  )
