package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.auth.*
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

object ProtectedResourceMetadataSpec extends ZIOSpecDefault:

  private val resourceUri = ResourceUri.parse("https://mcp.example.com/mcp").toOption.get
  private val authServer  = AuthorizationServer("https://auth.example.com")
  private val verifier    = TokenVerifier.fromFunction[Any](_ => ZIO.fail(AuthError.Missing))

  override def spec =
    suite("ProtectedResourceMetadataSpec")(

      test("required fields are present in the JSON output"):
        val auth = McpAuth(
          authorizationServers = NonEmptyChunk(authServer),
          scopesSupported = Chunk(OauthScope("mcp:tools")),
          verifier = verifier,
        )
        val prm = ProtectedResourceMetadata.fromAuth(auth, resourceUri)
        val json = prm.toJson.fromJson[Json.Obj].toOption.get
        assertTrue(
          json.get("resource").flatMap(_.asString).contains("https://mcp.example.com/mcp"),
          json.get("authorization_servers").flatMap(_.asArray).exists(_.exists(_.asString.contains("https://auth.example.com"))),
          json.get("bearer_methods_supported").flatMap(_.asArray).exists(_.exists(_.asString.contains("header"))),
        )
      ,

      test("scopes_supported is omitted when empty"):
        val auth = McpAuth(
          authorizationServers = NonEmptyChunk(authServer),
          scopesSupported = Chunk.empty,
          verifier = verifier,
        )
        val json = ProtectedResourceMetadata.fromAuth(auth, resourceUri).toJson
        assertTrue(!json.contains("scopes_supported"))
      ,

      test("resource_name and resource_documentation round-trip"):
        val auth = McpAuth(
          authorizationServers = NonEmptyChunk(authServer),
          scopesSupported = Chunk.empty,
          resourceName = Some("Example MCP"),
          resourceDocumentation = Some("https://example.com/docs"),
          verifier = verifier,
        )
        val json = ProtectedResourceMetadata.fromAuth(auth, resourceUri).toJson.fromJson[Json.Obj].toOption.get
        assertTrue(
          json.get("resource_name").flatMap(_.asString).contains("Example MCP"),
          json.get("resource_documentation").flatMap(_.asString).contains("https://example.com/docs"),
        )
      ,

      test("ResourceUri.parse rejects URIs with fragments"):
        assertTrue(
          ResourceUri.parse("https://mcp.example.com#frag").isLeft,
          ResourceUri.parse("https://mcp.example.com/mcp").isRight,
        )
      ,

      test("ResourceUri.parse rejects URIs without a scheme"):
        assertTrue(
          ResourceUri.parse("mcp.example.com").isLeft,
          ResourceUri.parse("/mcp").isLeft,
        )
      ,

      test("ResourceUri.matchesAudience tolerates trailing slash and case differences"):
        val uri = ResourceUri.parse("https://mcp.example.com/mcp").toOption.get
        assertTrue(
          uri.matchesAudience("https://mcp.example.com/mcp"),
          uri.matchesAudience("HTTPS://MCP.EXAMPLE.COM/mcp"),
          !uri.matchesAudience("https://other.example.com/mcp"),
        )
      ,

      test("ResourceUri.matchesAudience strips trailing slash on bare hosts"):
        val uri = ResourceUri.parse("https://mcp.example.com").toOption.get
        assertTrue(
          uri.matchesAudience("https://mcp.example.com"),
          uri.matchesAudience("https://mcp.example.com/"),
        )
      ,
    )
