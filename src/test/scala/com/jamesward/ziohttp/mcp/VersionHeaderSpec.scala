package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.client.*
import zio.*
import zio.http.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*
import zio.test.TestAspect.*

/**
 * Regression guard for a documented real-world interop bug (e.g. vercel/ai
 * #14413, which broke servers like Figma Dev Mode MCP): after the `initialize`
 * handshake a client MUST send the *negotiated* protocol version — the one the
 * server returned — in the `MCP-Protocol-Version` header of every subsequent
 * request, not the version it originally requested.
 *
 * A tiny hand-rolled server negotiates a different version (`2025-11-25`) than
 * the client asks for (`2025-06-18`) and records the header the client sends on
 * the follow-up call.
 */
object VersionHeaderSpec extends ZIOSpecDefault:

  private def rpcResult(id: Json, result: String): Response =
    Response.json(s"""{"jsonrpc":"2.0","id":${id.toJson},"result":$result}""")

  /** Server that always negotiates `2025-11-25` and captures the
    * `mcp-protocol-version` header seen on `tools/list`. */
  private def captureServer(seen: Ref[Option[String]]): Routes[Any, Response] =
    Routes(
      Method.POST / "mcp" -> handler { (req: Request) =>
        req.body.asString.flatMap: body =>
          val obj    = body.fromJson[Json.Obj].toOption.getOrElse(Json.Obj())
          val method = obj.get("method").flatMap(_.asString).getOrElse("")
          val id     = obj.get("id").getOrElse(Json.Null)
          method match
            case "initialize" =>
              // Downgrade: respond with 2025-11-25 regardless of the request.
              ZIO.succeed(rpcResult(id,
                """{"protocolVersion":"2025-11-25","capabilities":{},"serverInfo":{"name":"cap","version":"1"}}""")
                .addHeader("mcp-session-id", "sess-1"))
            case "notifications/initialized" =>
              ZIO.succeed(Response.status(Status.Accepted))
            case "tools/list" =>
              seen.set(req.rawHeader("mcp-protocol-version")) *>
                ZIO.succeed(rpcResult(id, """{"tools":[]}"""))
            case _ =>
              ZIO.succeed(Response.status(Status.Accepted))
      }
    ).sandbox

  override def spec =
    suite("VersionHeaderSpec")(
      test("client sends the negotiated version in the header, not the requested one"):
        for
          seen   <- Ref.make[Option[String]](None)
          port   <- Server.install(captureServer(seen))
          _      <- ZIO.scoped:
                      for
                        client <- McpClient.connect(McpClientConfig(
                                    s"http://localhost:$port/mcp",
                                    preferredVersion = ProtocolVersion.V2025_06_18, // client asks for 2025-06-18
                                  ))
                        _      <- client.listTools
                      yield ()
          header <- seen.get
        yield assertTrue(
          // the server negotiated 2025-11-25, so the follow-up header must be 2025-11-25
          header.contains("2025-11-25"),
          !header.contains("2025-06-18"),
        )
    ).provide(Server.defaultWith(_.onAnyOpenPort), Client.default) @@
      withLiveClock @@ timeout(1.minute) @@ sequential
