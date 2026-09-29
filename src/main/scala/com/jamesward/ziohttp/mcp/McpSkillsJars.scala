package com.jamesward.ziohttp.mcp

import org.snakeyaml.engine.v2.api.{Load, LoadSettings}
import zio.*
import zio.json.ast.Json

import java.net.{JarURLConnection, URLConnection, URLEncoder}
import java.nio.ByteBuffer
import java.nio.charset.{CodingErrorAction, StandardCharsets}
import java.nio.file.{Files, Path, Paths}
import java.security.MessageDigest
import java.util.Base64
import scala.jdk.CollectionConverters.*
import scala.util.Try

/** A skill found on the classpath that could not be served, with the reason it was skipped. */
final case class McpSkillsJarsSkipped(path: String, reason: String)

object McpSkillsJarsSkipped:
  given CanEqual[McpSkillsJarsSkipped, McpSkillsJarsSkipped] = CanEqual.derived

/**
 * Serves [[https://skillsjars.com SkillsJars]] over the Skills extension.
 *
 * A SkillsJar is an Agent Skill packaged as a JAR (published to Maven Central under
 * `com.skillsjars`) with its files under `META-INF/skills/<owner>/<repo>/<skill>/`. Every
 * directory holding a `SKILL.md` under `META-INF/skills/` on the classpath becomes one skill,
 * addressed as `skill://<owner>/<repo>/<skill>/SKILL.md`; the files beside it are its
 * static resources (with SHA-256 digests), readable through core `resources/read` and
 * browsable through `resources/directory/read`.
 *
 * Skills whose `SKILL.md` frontmatter fails validation, or which exceed the static-entry
 * limits, are left out and reported in [[skipped]] rather than failing the load.
 *
 * {{{
 * for
 *   jars <- McpSkillsJars.load
 * yield McpServer("skills", "1.0.0")
 *   .withExtensions(jars.extensions)
 *   .resourceSource(jars.resources)
 * }}}
 */
final class McpSkillsJars private (
  skills: Chunk[McpSkillsJars.LoadedSkill],
  val skipped: Chunk[McpSkillsJarsSkipped],
):
  import McpSkillsJars.*

  /** The loaded skills, in classpath path order. */
  val entries: Chunk[McpSkillEntry] = skills.map(_.entry)

  private val byUri: Map[String, LoadedSkill] = skills.map(skill => skill.entry.uri.value -> skill).toMap
  private val files: Map[String, SkillFile] = skills.flatMap(_.files).map(file => file.uri -> file).toMap
  private val directories: Map[String, Chunk[ResourceDefinition]] = directoryListings(skills)

  val source: McpSkillsSource[Any] = new McpSkillsSource[Any]:
    def list(
      params: McpSkillsListParams,
      ctx: McpRequestContext,
    ): IO[McpSkillsSourceError, McpSkillsListResult] =
      params.cursor match
        case None         => ZIO.succeed(McpSkillsListResult(entries))
        case Some(cursor) => ZIO.fail(McpSkillsSourceError.InvalidParams(s"Unknown cursor: $cursor"))

    def get(uri: McpSkillUri, ctx: McpRequestContext): IO[McpSkillsSourceError, McpSkillEntry] =
      ZIO.fromOption(byUri.get(uri.value).map(_.entry))
        .orElseFail(McpSkillsSourceError.InvalidParams(s"Unknown skill: ${uri.value}"))

  val directory: McpSkillsDirectorySource[Any] = new McpSkillsDirectorySource[Any]:
    def read(
      params: ResourceDirectoryReadParams,
      ctx: McpRequestContext,
    ): IO[McpSkillsSourceError, ResourcesListResult] =
      params.cursor match
        case Some(cursor) => ZIO.fail(McpSkillsSourceError.InvalidParams(s"Unknown cursor: $cursor"))
        case None =>
          ZIO.fromOption(directories.get(params.uri.stripSuffix("/")))
            .map(children => ResourcesListResult(children))
            .orElseFail(McpSkillsSourceError.InvalidParams(s"Not a directory resource: ${params.uri}"))

  /**
   * Lists each skill's `SKILL.md` in `resources/list` and reads every skill file through
   * `resources/read`: textual files as `text`, everything else as a base64 `blob`.
   */
  val resources: McpResourceSource[Any] = new McpResourceSource[Any]:
    def listResources(ctx: McpToolContext): UIO[Chunk[ResourceDefinition]] =
      ZIO.succeed(skills.map: skill =>
        ResourceDefinition(
          skill.entry.uri.value,
          skill.entry.uri.name.value,
          description = skill.entry.frontmatter.value.get("description").flatMap(_.asString),
          mimeType = Some(MarkdownMimeType),
        )
      )

    def listResourceTemplates(ctx: McpToolContext): UIO[Chunk[ResourceTemplateDefinition]] =
      ZIO.succeed(Chunk.empty)

    def readResource(uri: String, ctx: McpToolContext): IO[ToolError, Chunk[ResourceContents]] =
      ZIO.fromOption(files.get(uri))
        .map(file => Chunk(file.contents))
        .orElseFail(ToolError(s"Resource not found: $uri"))

  /** `skills/list`, `skills/get`, and `resources/directory/read` for the loaded skills. */
  def extensions: McpExtensions[Any] = McpSkills.withDirectory(source, directory)

object McpSkillsJars:
  /** The directory SkillsJars place their skills under. */
  val ResourcePrefix: String = "META-INF/skills/"

  private val SkillFileName = "SKILL.md"
  private val MarkdownMimeType = "text/markdown"
  private val DirectoryMimeType = "inode/directory"

  /** Load every skill under `META-INF/skills/` visible to the thread's context class loader. */
  def load: Task[McpSkillsJars] =
    ZIO.attempt(Option(Thread.currentThread.getContextClassLoader).getOrElse(getClass.getClassLoader))
      .flatMap(load(_))

  /**
   * Load every skill under `META-INF/skills/` visible to `classLoader`. When the same path
   * appears in more than one classpath entry, the first one wins, as with `getResource`.
   */
  def load(classLoader: ClassLoader): Task[McpSkillsJars] =
    for
      paths   <- ZIO.attemptBlocking(scan(classLoader))
      (skills, skipped) = build(paths)
      _       <- ZIO.foreachDiscard(skipped)(skip =>
                   ZIO.logWarning(s"Skipping SkillsJar skill ${skip.path}: ${skip.reason}")
                 )
    yield new McpSkillsJars(skills, skipped)

  private[mcp] final case class SkillFile(uri: String, path: String, mimeType: String, bytes: Array[Byte]):
    def contents: ResourceContents =
      textOf(mimeType, bytes) match
        case Some(text) => ResourceContents(uri, mimeType = Some(mimeType), text = Some(text))
        case None =>
          ResourceContents(uri, mimeType = Some(mimeType), blob = Some(Base64.getEncoder.encodeToString(bytes)))

  private[mcp] final case class LoadedSkill(entry: McpSkillEntry, root: String, files: Chunk[SkillFile])

  // --- classpath scanning ---

  /** Paths relative to `META-INF/skills/`, mapped to their bytes, in sorted order. */
  private def scan(classLoader: ClassLoader): Map[String, Array[Byte]] =
    val found = scala.collection.mutable.LinkedHashMap.empty[String, Array[Byte]]
    def add(path: String, read: => Array[Byte]): Unit =
      if path.nonEmpty && !found.contains(path) then found.update(path, read)

    classLoader.getResources(ResourcePrefix.stripSuffix("/")).asScala.foreach: url =>
      url.getProtocol match
        case "jar" =>
          val connection = url.openConnection().asInstanceOf[JarURLConnection]
          connection.setUseCaches(false)
          val jar = connection.getJarFile
          try
            jar.entries().asScala
              .filter(entry => !entry.isDirectory && entry.getName.startsWith(ResourcePrefix))
              .foreach: entry =>
                add(entry.getName.stripPrefix(ResourcePrefix), jar.getInputStream(entry).readAllBytes())
          finally jar.close()
        case "file" =>
          val root = Paths.get(url.toURI)
          val stream = Files.walk(root)
          try
            stream.iterator().asScala.filter(Files.isRegularFile(_)).foreach: file =>
              add(relativePath(root, file), Files.readAllBytes(file))
          finally stream.close()
        case _ => ()

    found.toMap

  private def relativePath(root: Path, file: Path): String =
    root.relativize(file).iterator().asScala.map(_.toString).mkString("/")

  // --- skill assembly ---

  private def build(paths: Map[String, Array[Byte]]): (Chunk[LoadedSkill], Chunk[McpSkillsJarsSkipped]) =
    val roots = Chunk.fromIterable(paths.keys)
      .filter(path => path.endsWith("/" + SkillFileName))
      .map(_.stripSuffix("/" + SkillFileName))
      .sorted
    // Each file belongs to its nearest enclosing skill root, so nested skills stay separate.
    val owned: Map[String, Chunk[String]] = Chunk.fromIterable(paths.keys)
      .flatMap(path => roots.filter(root => path.startsWith(root + "/")).maxByOption(_.length).map(_ -> path))
      .groupBy(_._1)
      .map((root, pairs) => root -> pairs.map(_._2).sorted)

    val results = roots.map(root => loadSkill(root, owned.getOrElse(root, Chunk.empty), paths))
    (results.collect { case Right(skill) => skill }, results.collect { case Left(skip) => skip })

  private def loadSkill(
    root: String,
    ownedPaths: Chunk[String],
    bytes: Map[String, Array[Byte]],
  ): Either[McpSkillsJarsSkipped, LoadedSkill] =
    val manifestPath = s"$root/$SkillFileName"
    def skip(reason: String) = McpSkillsJarsSkipped(ResourcePrefix + manifestPath, reason)

    // SKILL.md first, then the rest in path order.
    val ordered = ownedPaths.filter(_ == manifestPath) ++ ownedPaths.filterNot(_ == manifestPath)
    val files = ordered.map: path =>
      SkillFile(uriFor(path), path, mimeTypeOf(path), bytes(path))

    for
      uri         <- McpSkillUri.parse(uriFor(manifestPath)).left.map(error => skip(error.toString))
      manifest     = new String(bytes(manifestPath), StandardCharsets.UTF_8)
      frontmatter <- parseFrontmatter(manifest).left.map(skip)
      resources   <- files.foldLeft[Either[McpSkillsJarsSkipped, Chunk[McpSkillResource]]](Right(Chunk.empty)):
                       case (Right(acc), file) => resourceOf(file).left.map(skip).map(acc :+ _)
                       case (left, _)          => left
      nonEmpty    <- NonEmptyChunk.fromChunk(resources).toRight(skip("No resources"))
      entry       <- McpSkillEntry.static(uri, McpSkillFrontmatter(frontmatter), nonEmpty)
                       .left.map(error => skip(error.toString))
    yield LoadedSkill(entry, uriFor(root), files)

  private def resourceOf(file: SkillFile): Either[String, McpSkillResource] =
    for
      uri    <- McpSkillResourceUri.parse(file.uri).left.map(_.toString)
      digest <- McpSkillDigest.parse(sha256(file.bytes)).left.map(_.toString)
      size   <- McpSkillSize.parse(file.bytes.length.toLong).left.map(_.toString)
    yield McpSkillResource(uri, digest, size)

  private def directoryListings(skills: Chunk[LoadedSkill]): Map[String, Chunk[ResourceDefinition]] =
    skills.flatMap: skill =>
      val rootSegments = segments(skill.files.head.path).length - 1
      val relative = skill.files.map(file => file -> segments(file.path).drop(rootSegments))
      val dirs = relative.flatMap((_, parts) => parts.indices.map(parts.take(_))).distinct
      dirs.map: dir =>
        val dirUri = (skill.root +: dir.map(encodeSegment)).mkString("/")
        val children = relative.collect:
          case (file, parts) if parts.length == dir.length + 1 && parts.startsWith(dir) =>
            ResourceDefinition(file.uri, parts.last, mimeType = Some(file.mimeType))
        val subdirNames = relative.collect:
          case (_, parts) if parts.length > dir.length + 1 && parts.startsWith(dir) => parts(dir.length)
        val subdirs = subdirNames.distinct.sorted.map: name =>
          ResourceDefinition(s"$dirUri/${encodeSegment(name)}", name, mimeType = Some(DirectoryMimeType))
        dirUri -> (subdirs ++ children)
    .toMap

  // --- helpers ---

  private def segments(path: String): Chunk[String] = Chunk.fromArray(path.split('/')).filter(_.nonEmpty)

  private def uriFor(path: String): String = "skill://" + segments(path).map(encodeSegment).mkString("/")

  private def encodeSegment(segment: String): String =
    URLEncoder.encode(segment, StandardCharsets.UTF_8).replace("+", "%20")

  private def sha256(bytes: Array[Byte]): String =
    "sha256:" + MessageDigest.getInstance("SHA-256").digest(bytes).map(b => f"${b & 0xff}%02x").mkString

  /** Parse the YAML frontmatter delimited by `---` lines at the top of a SKILL.md. */
  private[mcp] def parseFrontmatter(markdown: String): Either[String, Json.Obj] =
    val lines = markdown.stripPrefix("﻿").linesIterator.toList
    lines match
      case first :: rest if first.trim == "---" =>
        val body = rest.takeWhile(_.trim != "---")
        if body.length == rest.length then Left("Unterminated SKILL.md frontmatter")
        else
          Try(new Load(LoadSettings.builder().build()).loadFromString(body.mkString("\n"))).toEither
            .left.map(error => s"Invalid SKILL.md frontmatter: ${error.getMessage}")
            .flatMap:
              case map: java.util.Map[?, ?] => yamlToJson(map).asObject.toRight("SKILL.md frontmatter must be a mapping")
              case _                        => Left("SKILL.md frontmatter must be a mapping")
      case _ => Left("SKILL.md has no frontmatter")

  private def yamlToJson(value: Any): Json = Option(value.asInstanceOf[AnyRef]).fold(Json.Null):
    case s: String                 => Json.Str(s)
    case b: java.lang.Boolean      => Json.Bool(b)
    case i: java.lang.Integer      => Json.Num(i.intValue)
    case l: java.lang.Long         => Json.Num(l.longValue)
    case b: java.math.BigInteger   => Json.Num(BigDecimal(b))
    case d: java.lang.Double       => Json.Num(d.doubleValue)
    case m: java.util.Map[?, ?]    =>
      Json.Obj(Chunk.fromIterable(m.asScala.map((k, v) => String.valueOf(k) -> yamlToJson(v))))
    case l: java.util.List[?]      => Json.Arr(Chunk.fromIterable(l.asScala.map(yamlToJson)))
    case other                     => Json.Str(String.valueOf(other))

  private val KnownMimeTypes: Map[String, String] = Map(
    "md" -> MarkdownMimeType,
    "markdown" -> MarkdownMimeType,
    "txt" -> "text/plain",
    "py" -> "text/x-python",
    "sh" -> "text/x-shellscript",
    "js" -> "text/javascript",
    "mjs" -> "text/javascript",
    "ts" -> "text/typescript",
    "json" -> "application/json",
    "yaml" -> "application/yaml",
    "yml" -> "application/yaml",
    "xml" -> "application/xml",
    "xsd" -> "application/xml",
    "html" -> "text/html",
    "css" -> "text/css",
    "csv" -> "text/csv",
    "svg" -> "image/svg+xml",
    "png" -> "image/png",
    "jpg" -> "image/jpeg",
    "jpeg" -> "image/jpeg",
    "gif" -> "image/gif",
    "pdf" -> "application/pdf",
    "ttf" -> "font/ttf",
    "otf" -> "font/otf",
    "woff" -> "font/woff",
    "woff2" -> "font/woff2",
  )

  private val TextualApplicationTypes = Set("application/json", "application/yaml", "application/xml", "image/svg+xml")

  private def mimeTypeOf(path: String): String =
    val name = path.substring(path.lastIndexOf('/') + 1)
    val extension = name.lastIndexOf('.') match
      case -1    => ""
      case index => name.substring(index + 1).toLowerCase
    KnownMimeTypes.get(extension)
      .orElse(Option(URLConnection.guessContentTypeFromName(name)))
      .getOrElse(if extension.isEmpty then "text/plain" else "application/octet-stream")

  /** The UTF-8 text of a file whose MIME type is textual, or `None` to send it as a blob. */
  private def textOf(mimeType: String, bytes: Array[Byte]): Option[String] =
    if mimeType.startsWith("text/") || TextualApplicationTypes.contains(mimeType) then
      Try(
        StandardCharsets.UTF_8.newDecoder()
          .onMalformedInput(CodingErrorAction.REPORT)
          .onUnmappableCharacter(CodingErrorAction.REPORT)
          .decode(ByteBuffer.wrap(bytes))
          .toString
      ).toOption
    else None
