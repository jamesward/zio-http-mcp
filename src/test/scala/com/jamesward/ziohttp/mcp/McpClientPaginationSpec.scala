package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.client.*
import zio.*
import zio.http.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*
import zio.test.TestAspect.*

object McpClientPaginationSpec extends ZIOSpecDefault:

  private def rpcResult(id: Json, result: String): Response =
    Response.json(s"""{"jsonrpc":"2.0","id":${id.toJson},"result":$result}""")

  private def paginatedServer(seen: Ref[List[Option[String]]]): Routes[Any, Response] =
    Routes(
      Method.POST / "mcp" -> handler { (request: Request) =>
        request.body.asString.flatMap: body =>
          val json = body.fromJson[Json.Obj].toOption.getOrElse(Json.Obj())
          val method = json.get("method").flatMap(_.asString).getOrElse("")
          val id = json.get("id").getOrElse(Json.Null)
          method match
            case "initialize" =>
              ZIO.succeed(rpcResult(id,
                """{"protocolVersion":"2025-11-25","capabilities":{"tools":{}},"serverInfo":{"name":"pages","version":"1"}}""")
                .addHeader("mcp-session-id", "pagination-session"))
            case "notifications/initialized" =>
              ZIO.succeed(Response.status(Status.Accepted))
            case "tools/list" =>
              val cursor = json.get("params").flatMap(_.asObject).flatMap(_.get("cursor")).flatMap(_.asString)
              seen.update(_ :+ cursor) *>
                ZIO.succeed(
                  if cursor.isEmpty then rpcResult(id,
                    """{"tools":[{"name":"first","inputSchema":{"type":"object"}}],"nextCursor":"page-2"}""")
                  else rpcResult(id,
                    """{"tools":[{"name":"second","inputSchema":{"type":"object"}}]}""")
                )
            case _ => ZIO.succeed(Response.status(Status.Accepted))
      }
    ).sandbox

  def spec = suite("McpClient tools/list pagination")(
    test("listTools follows nextCursor and preserves page order") {
      for
        seen <- Ref.make(List.empty[Option[String]])
        port <- Server.install(paginatedServer(seen))
        tools <- ZIO.scoped:
          for
            client <- McpClient.connect(McpClientConfig(
              s"http://localhost:$port/mcp",
              preferredVersion = ProtocolVersion.V2025_11_25,
            ))
            tools <- client.listTools
          yield tools
        cursors <- seen.get
      yield assertTrue(
        tools.map(_.name.value).toList == List("first", "second"),
        cursors == List(None, Some("page-2")),
      )
    }
  ).provide(Server.defaultWith(_.onAnyOpenPort), Client.default) @@
    withLiveClock @@ timeout(1.minute) @@ sequential
