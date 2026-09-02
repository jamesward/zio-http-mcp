package com.jamesward.ziohttp.mcp

import zio.*
import zio.json.ast.Json
import zio.test.*

object McpSkillsDomainSpec extends ZIOSpecDefault:
  private val digestText = "sha256:" + "a" * 64

  private def parsedResource(uri: String, size: Long): IO[Any, McpSkillResource] =
    for
      parsedUri <- ZIO.fromEither(McpSkillResourceUri.parse(uri))
      digest    <- ZIO.fromEither(McpSkillDigest.parse(digestText))
      parsedSize <- ZIO.fromEither(McpSkillSize.parse(size))
    yield McpSkillResource(parsedUri, digest, parsedSize)

  private def frontmatter(name: String): McpSkillFrontmatter =
    McpSkillFrontmatter(Json.Obj(
      "name" -> Json.Str(name),
      "description" -> Json.Str("A useful skill"),
      "future" -> Json.Obj("nested" -> Json.Arr(Json.Num(1), Json.Bool(true))),
    ))

  override def spec =
    suite("Skills domain and codecs")(
      test("skill URIs accept any scheme but enforce the SKILL.md path and Agent Skills name rules"):
        val accepted = Chunk(
          "skill://refunds/SKILL.md",
          "github://owner/repo/skills/refunds/SKILL.md",
          "custom:///org/refunds/SKILL.md",
        )
        val rejected = Chunk(
          "skill://refunds/README.md",
          "skill://refunds/SKILL.md/child",
          "/refunds/SKILL.md",
          "skill://SKILL.md",
          "skill://Refunds/SKILL.md",
          "skill://refund--requests/SKILL.md",
          s"skill://${"a" * 65}/SKILL.md",
          "skill://refunds/SKILL.md?revision=1",
          "github://owner/skills/refunds/../other/SKILL.md",
          "github://owner/skills/refunds/%2e%2e/other/SKILL.md",
        )
        assertTrue(
          accepted.forall(McpSkillUri.parse(_).isRight),
          rejected.forall(McpSkillUri.parse(_).isLeft),
          McpSkillResourceUri.parse("github://owner/skills/refunds/../secret.txt").isLeft,
          McpSkillResourceUri.parse("github://owner/skills/refunds/%2e%2e/secret.txt").isLeft,
        )
      ,
      test("digest and size parsed values reject malformed states"):
        assertTrue(
          McpSkillDigest.parse(digestText).isRight,
          McpSkillDigest.parse("sha256:" + "A" * 64).isLeft,
          McpSkillDigest.parse("sha256:" + "a" * 63).isLeft,
          McpSkillSize.parse(0).isRight,
          McpSkillSize.parse(-1).isLeft,
        )
      ,
      test("static manifests enforce entry inclusion, uniqueness, containment, count, and total size"):
        for
          uri       <- ZIO.fromEither(McpSkillUri.parse("github://owner/skills/refunds/SKILL.md"))
          entry     <- parsedResource(uri.value, 10)
          sibling   <- parsedResource("github://owner/skills/refunds/reference.md", 20)
          duplicate  = McpSkillEntry.static(uri, frontmatter("refunds"), NonEmptyChunk(entry, entry))
          valid      = McpSkillEntry.static(uri, frontmatter("refunds"), NonEmptyChunk(entry, sibling))
          missing    = McpSkillEntry.static(uri, frontmatter("refunds"), NonEmptyChunk(sibling))
          outside   <- parsedResource("github://owner/skills/other/file.md", 1)
          escaped    = McpSkillEntry.static(uri, frontmatter("refunds"), NonEmptyChunk(entry, outside))
          root       <- parsedResource("github://owner/skills/refunds", 1)
          rootAsFile  = McpSkillEntry.static(uri, frontmatter("refunds"), NonEmptyChunk(entry, root))
          hugeEntry <- parsedResource(uri.value, McpSkillEntry.MaximumTotalBytes + 1L)
          huge       = McpSkillEntry.static(uri, frontmatter("refunds"), NonEmptyChunk(hugeEntry))
          manyChunk <- ZIO.foreach(0 until 513)(index =>
                         parsedResource(s"github://owner/skills/refunds/$index.txt", 1)
                       )
          manyResources <- ZIO.fromOption(NonEmptyChunk.fromChunk(Chunk.fromIterable(manyChunk)))
                             .orElseFail("Expected generated resources to be nonempty")
          many = McpSkillEntry.static(uri, frontmatter("refunds"), manyResources)
        yield assertTrue(
          valid.isRight,
          duplicate == Left(McpSkillEntryError.DuplicateResourceUri(entry.uri)),
          missing == Left(McpSkillEntryError.MissingEntryResource(uri)),
          escaped == Left(McpSkillEntryError.ResourceOutsideSkill(outside.uri, uri)),
          rootAsFile == Left(McpSkillEntryError.ResourceOutsideSkill(root.uri, uri)),
          huge == Left(McpSkillEntryError.TotalSizeTooLarge(
            BigInt(McpSkillEntry.MaximumTotalBytes) + BigInt(1),
            McpSkillEntry.MaximumTotalBytes,
          )),
          many == Left(McpSkillEntryError.TooManyResources(513, McpSkillEntry.MaximumResources)),
        )
      ,
      test("entry construction enforces frontmatter name and description"):
        for
          uri <- ZIO.fromEither(McpSkillUri.parse("skill://refunds/SKILL.md"))
          wrong = McpSkillEntry.dynamic(uri, frontmatter("other"))
          missing = McpSkillEntry.dynamic(uri, McpSkillFrontmatter(Json.Obj(
            "name" -> Json.Str("refunds")
          )))
        yield assertTrue(
          wrong == Left(McpSkillEntryError.UriNameMismatch("refunds", "other")),
          missing == Left(McpSkillEntryError.MissingFrontmatterDescription),
        )
      ,
      test("entry construction enforces delegated optional frontmatter constraints"):
        for
          uri <- ZIO.fromEither(McpSkillUri.parse("skill://refunds/SKILL.md"))
          invalidName = McpSkillEntry.dynamic(uri, frontmatter("Refunds"))
          longDescription = McpSkillEntry.dynamic(uri, McpSkillFrontmatter(Json.Obj(
            "name" -> Json.Str("refunds"),
            "description" -> Json.Str("x" * 1025),
          )))
          badCompatibility = McpSkillEntry.dynamic(uri, McpSkillFrontmatter(Json.Obj(
            "name" -> Json.Str("refunds"),
            "description" -> Json.Str("A useful skill"),
            "compatibility" -> Json.Str(" "),
          )))
          badMetadata = McpSkillEntry.dynamic(uri, McpSkillFrontmatter(Json.Obj(
            "name" -> Json.Str("refunds"),
            "description" -> Json.Str("A useful skill"),
            "metadata" -> Json.Obj("version" -> Json.Num(1)),
          )))
        yield assertTrue(
          invalidName == Left(McpSkillEntryError.InvalidFrontmatterName(
            "Refunds",
            McpSkillNameError.InvalidCharacters("Refunds"),
          )),
          longDescription == Left(McpSkillEntryError.FrontmatterDescriptionTooLong(1025, 1024)),
          badCompatibility == Left(McpSkillEntryError.InvalidFrontmatterCompatibility),
          badMetadata == Left(McpSkillEntryError.InvalidFrontmatterMetadata),
        )
      ,
      test("wire codec preserves semantic JSON frontmatter and static/dynamic resource shapes"):
        for
          uri      <- ZIO.fromEither(McpSkillUri.parse("skill://refunds/SKILL.md"))
          resource <- parsedResource(uri.value, 10)
          static   <- ZIO.fromEither(McpSkillEntry.static(uri, frontmatter("refunds"), NonEmptyChunk(resource)))
          dynamic  <- ZIO.fromEither(McpSkillEntry.dynamic(uri, frontmatter("refunds")))
          staticJson <- ZIO.fromEither(McpSkillsWireCodecs.listResult.encode(
                          McpSkillsListResult(Chunk(static), Some("next"))
                        ))
          dynamicJson <- ZIO.fromEither(McpSkillsWireCodecs.getResult.encode(McpSkillsGetResult(dynamic)))
          roundTrip <- ZIO.fromEither(McpSkillsWireCodecs.listResult.decode(staticJson))
        yield
          val expectedStatic = Json.Obj(
            "skills" -> Json.Arr(Json.Obj(
              "uri" -> Json.Str(uri.value),
              "frontmatter" -> frontmatter("refunds").value,
              "resources" -> Json.Arr(Json.Obj(
                "uri" -> Json.Str(uri.value),
                "digest" -> Json.Str(digestText),
                "size" -> Json.Num(10),
              )),
            )),
            "nextCursor" -> Json.Str("next"),
          )
          val expectedDynamic = Json.Obj(
            "skill" -> Json.Obj(
              "uri" -> Json.Str(uri.value),
              "frontmatter" -> frontmatter("refunds").value,
              "resources" -> Json.Str("dynamic"),
            )
          )
          assertTrue(
            staticJson.asObject.contains(expectedStatic),
            dynamicJson.asObject.contains(expectedDynamic),
            roundTrip == McpSkillsListResult(Chunk(static), Some("next")),
          )
      ,
    )
