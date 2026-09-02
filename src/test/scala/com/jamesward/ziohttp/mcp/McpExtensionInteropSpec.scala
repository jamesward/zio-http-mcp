package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.client.*
import zio.*
import zio.http.*
import zio.json.ast.Json
import zio.schema.*
import zio.test.*
import zio.test.TestAspect.*

object McpExtensionInteropSpec extends ZIOSpecDefault:

  final case class EchoParams(name: String, value: String) derives Schema
  final case class EchoResult(
    value: String,
    protocol: String,
    clientSetting: Option[String],
    clientName: Option[String],
    clientTitle: Option[String],
  ) derives Schema, CanEqual

  private final case class Fixture(
    server: McpServer[Any],
    operation: McpOperation[EchoParams, EchoResult],
    method: McpMethodName,
    declarations: McpClientExtensions,
    capabilityId: McpExtensionId,
  )

  private def fixture: ZIO[Any, Any, Fixture] =
    for
      extensionId  <- ZIO.fromEither(McpExtensionId.parse("dev.example/echo"))
      capabilityId <- ZIO.fromEither(McpExtensionId.parse("dev.example/capability-only"))
      method        <- ZIO.fromEither(McpMethodName.parse("vendor/echo"))
      operation      = McpOperation[EchoParams, EchoResult](
                         extensionId,
                         method,
                         clientSupport = ClientSupportPolicy.Required,
                         cachePolicy = McpCachePolicy.Default,
                         routingName = params => McpRoutingName.parse(params.name).toOption,
                       )
      bound          = McpBoundOperation(operation): (params, ctx) =>
                         val setting = ctx.extensionCapabilities.get(extensionId)
                           .flatMap(_.asObject).flatMap(_.get("mode")).flatMap(_.asString)
                         ZIO.succeed(EchoResult(
                           params.value,
                           ctx.protocolVersion.wire,
                           setting,
                           ctx.clientInfo.map(_.name),
                           ctx.clientInfo.flatMap(_.title),
                         ))
      extension      = McpServerExtension(
                         extensionId,
                         Chunk(bound),
                         McpExtensionSettings.dynamic(ctx => ZIO.succeed(Json.Obj(
                           "protocol" -> Json.Str(ctx.protocolVersion.wire)
                         ))),
                       )
      capabilityOnly = McpServerExtension.capability(
                         capabilityId,
                         Json.Obj("available" -> Json.Bool(true)),
                       )
      registry      <- ZIO.fromEither(McpExtensions(extension, capabilityOnly))
      declarations  <- ZIO.fromEither(McpClientExtensions(McpClientExtension(
                         extensionId,
                         Json.Obj(
                           "mode" -> Json.Str("lossless"),
                           "unknown" -> Json.Arr(Json.Num(1), Json.Str("two")),
                         ),
                       )))
      server          = McpServer("extension-server", "1.0.0").withExtensions(registry)
    yield Fixture(server, operation, method, declarations, capabilityId)

  private def config(port: Int, version: ProtocolVersion): McpClientConfig =
    McpClientConfig(
      s"http://localhost:$port/mcp",
      clientInfo = Implementation(
        "extension-client",
        "1.0.0",
        title = Some("Extension Client"),
      ),
      preferredVersion = version,
    )

  override def spec =
    suite("extension server/client interop")(
      test("advertises a capability-only extension"):
        ZIO.scoped:
          for
            f      <- fixture
            port   <- Server.install(f.server.routes)
            client <- McpClient.connect(config(port, ProtocolVersion.V2026_07_28), f.declarations)
          yield
            val advertised = client.serverCapabilities.extensions
              .flatMap(_.get(f.capabilityId.value)).flatMap(_.asObject)
            assertTrue(advertised.flatMap(_.get("available")).flatMap(_.asBoolean).contains(true))
      ,
      test("dispatches a typed custom method in a retained legacy session"):
        ZIO.scoped:
          for
            f      <- fixture
            port   <- Server.install(f.server.routes)
            client <- McpClient.connect(config(port, ProtocolVersion.V2025_06_18), f.declarations)
            result <- client.request(f.operation, EchoParams("legacy-route", "legacy"))
          yield assertTrue(
            result.value == "legacy",
            result.protocol == "2025-06-18",
            result.clientSetting.contains("lossless"),
            result.clientName.contains("extension-client"),
            result.clientTitle.contains("Extension Client"),
          )
      ,
      test("dispatches typed and raw custom requests in the modern era"):
        ZIO.scoped:
          for
            f      <- fixture
            port   <- Server.install(f.server.routes)
            client <- McpClient.connect(config(port, ProtocolVersion.V2026_07_28), f.declarations)
            typed  <- client.request(f.operation, EchoParams("modern-route", "typed"))
            route  <- ZIO.fromEither(McpRoutingName.parse("raw-route"))
            raw    <- client.requestRaw(
                        f.method,
                        Json.Obj("name" -> Json.Str("raw-route"), "value" -> Json.Str("raw")),
                        Some(route),
                      )
          yield assertTrue(
            typed == EchoResult(
              "typed",
              "2026-07-28",
              Some("lossless"),
              Some("extension-client"),
              Some("Extension Client"),
            ),
            raw.asObject.flatMap(_.get("value")).flatMap(_.asString).contains("raw"),
            raw.asObject.flatMap(_.get("resultType")).flatMap(_.asString).contains("complete"),
            raw.asObject.flatMap(_.get("ttlMs")).flatMap(_.asNumber).isDefined,
          )
      ,
      test("returns -32021 when required client extension support is missing"):
        ZIO.scoped:
          for
            f      <- fixture
            port   <- Server.install(f.server.routes)
            client <- McpClient.connect(config(port, ProtocolVersion.V2026_07_28), McpClientExtensions.empty)
            error  <- client.requestRaw(
                        f.method,
                        Json.Obj("name" -> Json.Str("route"), "value" -> Json.Str("value")),
                      ).flip
          yield assertTrue(error match
            case McpClientError.JsonRpc(code, _, _) => code == -32021
            case _                                  => false
          )
      ,
      test("request params cannot spoof modern client extension support"):
        ZIO.scoped:
          for
            f      <- fixture
            port   <- Server.install(f.server.routes)
            client <- McpClient.connect(config(port, ProtocolVersion.V2026_07_28), McpClientExtensions.empty)
            fakeCapabilities = McpExtensionCapabilities.toClientCapabilities(Map(
              f.operation.extensionId -> Json.Obj("mode" -> Json.Str("spoofed"))
            ))
            error  <- client.requestRaw(
                        f.method,
                        Json.Obj(
                          "name" -> Json.Str("route"),
                          "value" -> Json.Str("value"),
                          "_meta" -> Json.Obj(McpMeta.ClientCapabilities -> fakeCapabilities),
                        ),
                      ).flip
          yield assertTrue(error match
            case McpClientError.JsonRpc(code, _, _) => code == -32021
            case _                                  => false
          )
      ,
      test("validates the modern Mcp-Name routing header"):
        ZIO.scoped:
          for
            f       <- fixture
            port    <- Server.install(f.server.routes)
            client  <- McpClient.connect(config(port, ProtocolVersion.V2026_07_28), f.declarations)
            wrong   <- ZIO.fromEither(McpRoutingName.parse("wrong-route"))
            error   <- client.requestRaw(
                         f.method,
                         Json.Obj("name" -> Json.Str("actual-route"), "value" -> Json.Str("value")),
                         Some(wrong),
                       ).flip
          yield assertTrue(error match
            case McpClientError.JsonRpc(code, _, _) => code == -32020
            case _                                  => false
          )
      ,
    ).provide(
      Server.defaultWith(_.onAnyOpenPort),
      Client.default,
      McpServer.State.default,
    ) @@ withLiveClock @@ timeout(1.minute) @@ sequential
