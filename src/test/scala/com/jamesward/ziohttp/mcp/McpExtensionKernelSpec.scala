package com.jamesward.ziohttp.mcp

import zio.*
import zio.json.ast.Json
import zio.schema.*
import zio.test.*

object McpExtensionKernelSpec extends ZIOSpecDefault:

  final case class EmptyParams() derives Schema
  final case class EmptyResult(ok: Boolean) derives Schema
  final case class SearchParams(query: String) derives Schema
  final case class SearchResult(matches: Chunk[String]) derives Schema

  private def extensionId(value: String): IO[McpExtensionIdError, McpExtensionId] =
    ZIO.fromEither(McpExtensionId.parse(value))

  private def methodName(value: String): IO[McpMethodNameError, McpMethodName] =
    ZIO.fromEither(McpMethodName.parse(value))

  private def operation(
    id: McpExtensionId,
    method: McpMethodName,
  ): McpBoundOperation[Any] =
    McpBoundOperation(McpOperation[EmptyParams, EmptyResult](id, method)):
      (_: EmptyParams, _: McpRequestContext) => ZIO.succeed(EmptyResult(true))

  override def spec =
    suite("generalized extension kernel")(
      test("README generalized extension example registers atomically"):
        val registered = for
          id     <- McpExtensionId.parse("dev.example/search")
          method <- McpMethodName.parse("search/query")
          operation = McpOperation[SearchParams, SearchResult](id, method)
          extension = McpServerExtension(
            id,
            Chunk(McpBoundOperation(operation): (params, _) =>
              ZIO.succeed(SearchResult(Chunk(params.query)))
            ),
            Json.Obj("version" -> Json.Num(1)),
          )
          registry <- McpExtensions(extension)
        yield McpServer("search", "1.0.0").withExtensions(registry)
        assertTrue(registered.isRight)
      ,
      test("parses extension IDs and MCP method-like names"):
        val validId = McpExtensionId.parse("dev.example/search")
        val invalidIds = Chunk("", "example/search", "dev..example/search", "Dev.example/search", "dev.example/")
        val validMethods = Chunk("ping", "vendor/search", "dev.example/search-v2")
        val invalidMethods = Chunk("", "/search", "vendor//search", "vendor/search?x")
        assertTrue(
          validId.map(_.value) == Right("dev.example/search"),
          invalidIds.forall(McpExtensionId.parse(_).isLeft),
          validMethods.forall(McpMethodName.parse(_).isRight),
          invalidMethods.forall(McpMethodName.parse(_).isLeft),
        )
      ,
      test("rejects duplicate extension IDs"):
        for
          id <- extensionId("dev.example/one")
          first = McpServerExtension.capability(id, Json.Obj("a" -> Json.Bool(true)))
          second = McpServerExtension.capability(id, Json.Obj("b" -> Json.Bool(true)))
          result = McpExtensions(first, second)
        yield assertTrue(result == Left(McpExtensionsError.DuplicateExtensionId(id)))
      ,
      test("rejects duplicate methods across extensions"):
        for
          firstId  <- extensionId("dev.example/one")
          secondId <- extensionId("dev.example/two")
          method   <- methodName("vendor/search")
          first     = McpServerExtension(firstId, Chunk(operation(firstId, method)), Json.Obj())
          second    = McpServerExtension(secondId, Chunk(operation(secondId, method)), Json.Obj())
          result    = McpExtensions(first, second)
        yield assertTrue(result == Left(McpExtensionsError.DuplicateMethod(method)))
      ,
      test("rejects operation and enclosing extension ID mismatches"):
        for
          outerId    <- extensionId("dev.example/outer")
          operationId <- extensionId("dev.example/inner")
          method      <- methodName("vendor/search")
          result       = McpExtensions(McpServerExtension(
                           outerId,
                           Chunk(operation(operationId, method)),
                           Json.Obj(),
                         ))
        yield assertTrue(result == Left(McpExtensionsError.OperationExtensionMismatch(
          outerId,
          operationId,
          method,
        )))
      ,
      test("rejects duplicate client extension declarations"):
        for
          id <- extensionId("dev.example/client")
          declaration = com.jamesward.ziohttp.mcp.client.McpClientExtension(id, Json.Obj())
          result = com.jamesward.ziohttp.mcp.client.McpClientExtensions(declaration, declaration)
        yield assertTrue(result == Left(
          com.jamesward.ziohttp.mcp.client.McpClientExtensionsError.DuplicateExtensionId(id)
        ))
      ,
      test("rejects closed core method shadowing across request families"):
        for
          id      <- extensionId("dev.example/core-shadow")
          methods <- ZIO.foreach(Chunk(
                       "initialize",
                       "tools/list",
                       "notifications/initialized",
                       "sampling/createMessage",
                       "elicitation/create",
                     ))(methodName)
          results  = methods.map: method =>
                       McpExtensions(McpServerExtension(id, Chunk(operation(id, method)), Json.Obj()))
        yield assertTrue(results.zip(methods).forall: (result, method) =>
          result == Left(McpExtensionsError.CoreMethodShadow(method))
        )
      ,
      test("allows extension-defined methods inside a core namespace"):
        for
          id <- extensionId("io.modelcontextprotocol/skills")
          methods <- ZIO.foreach(Chunk(
                       "resources/directory/read",
                       "resources/vendor-search",
                     ))(methodName)
          results = methods.map(method =>
            McpExtensions(McpServerExtension(id, Chunk(operation(id, method)), Json.Obj()))
          )
        yield assertTrue(results.forall(_.isRight))
      ,
      test("represents invalid extension capability IDs and preserves their settings"):
        val invalidSettings = Json.Obj("future" -> Json.Arr(Json.Num(1), Json.Str("two")))
        val parsed = McpExtensionCapabilities.parse(Json.Obj(
          "extensions" -> Json.Obj("Not Valid" -> invalidSettings)
        ))
        assertTrue(
          parsed.valid.isEmpty,
          parsed.invalid == Chunk(McpExtensionCapabilityParseError(
            "Not Valid",
            invalidSettings,
            McpExtensionIdError.Invalid("Not Valid"),
          )),
        )
      ,
      test("ignores core capability fields and represents a malformed extensions container"):
        val coreOnly = McpExtensionCapabilities.parse(Json.Obj(
          "roots" -> Json.Obj("listChanged" -> Json.Bool(true))
        ))
        val malformed = McpExtensionCapabilities.parse(Json.Obj(
          "extensions" -> Json.Str("invalid")
        ))
        assertTrue(
          coreOnly == McpExtensionCapabilitiesParseResult(Map.empty, Chunk.empty),
          malformed.invalid == Chunk(McpExtensionCapabilityParseError(
            "extensions",
            Json.Str("invalid"),
            McpExtensionIdError.Invalid("extensions"),
          )),
        )
      ,
      test("server owns modern envelope and cache fields"):
        for
          ttl <- ZIO.fromEither(McpCacheTtl.parse(2500L))
          attacker = Implementation("attacker", "0")
          server = Implementation("server", "1")
          result = Json.Obj(
            "value" -> Json.Str("kept"),
            "resultType" -> Json.Str("input_required"),
            "ttlMs" -> Json.Num(-1),
            "cacheScope" -> Json.Str("public"),
            "_meta" -> Json.Obj(
              McpMeta.ServerInfo -> Json.Obj(
                "name" -> Json.Str(attacker.name),
                "version" -> Json.Str(attacker.version),
              ),
              "custom" -> Json.Bool(true),
            ),
          )
          uncached = ModernEnvelope.completeServerOwned(result, server, McpCachePolicy.NotCacheable)
          cached = ModernEnvelope.completeServerOwned(
            result,
            server,
            McpCachePolicy.Cacheable(ttl, McpCacheScope.Private),
          )
          serverInfo = cached.get("_meta").flatMap(_.asObject).flatMap(_.get(McpMeta.ServerInfo))
            .flatMap(_.as[Implementation].toOption)
          customMeta = cached.get("_meta").flatMap(_.asObject).flatMap(_.get("custom"))
        yield assertTrue(
          uncached.get("resultType").flatMap(_.asString).contains("complete"),
          uncached.get("ttlMs").isEmpty,
          uncached.get("cacheScope").isEmpty,
          cached.get("ttlMs").flatMap(_.asNumber).exists(_.value.longValue == 2500L),
          cached.get("cacheScope").flatMap(_.asString).contains("private"),
          serverInfo.contains(server),
          customMeta.contains(Json.Bool(true)),
        )
      ,
      test("supports capability-only and context-dynamic settings"):
        for
          id <- extensionId("dev.example/capability")
          extension = McpServerExtension(
            id,
            Chunk.empty,
            McpExtensionSettings.dynamic(ctx => ZIO.succeed(Json.Obj(
              "protocol" -> Json.Str(ctx.protocolVersion.wire),
              "path" -> Json.Str(ctx.pathParams.getOrElse("tenant", "missing")),
            ))),
          )
          registry <- ZIO.fromEither(McpExtensions(extension))
          settings <- registry.settings(McpRequestContext(
                        ProtocolVersion.V2026_07_28,
                        callerPathParams = Map("tenant" -> "acme"),
                      ))
        yield assertTrue(
          registry.values.headOption.exists(_.operations.isEmpty),
          settings.get(id).contains(Json.Obj(
            "protocol" -> Json.Str("2026-07-28"),
            "path" -> Json.Str("acme"),
          )),
        )
      ,
      test("retains unknown extension settings losslessly"):
        for
          id <- extensionId("unknown.example/vendor")
          settings = Json.Obj(
            "nested" -> Json.Arr(Json.Str("a"), Json.Num(2)),
            "enabled" -> Json.Bool(true),
          )
          parsed = McpExtensionCapabilities.parse(Json.Obj(
            "extensions" -> Json.Obj(id.value -> settings)
          ))
        yield assertTrue(
          parsed.valid.get(id).contains(settings),
          parsed.invalid.isEmpty,
        )
      ,
    )
