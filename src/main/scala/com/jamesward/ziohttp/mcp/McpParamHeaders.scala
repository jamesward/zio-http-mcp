package com.jamesward.ziohttp.mcp

import zio.*
import zio.json.ast.Json

import java.nio.charset.StandardCharsets
import java.util.Base64

/**
 * One tool parameter a server asks to have mirrored into an HTTP header: the
 * `x-mcp-header` annotation on a property of the tool's `inputSchema`
 * (2026-07-28 Streamable HTTP, Custom Headers from Tool Parameters).
 *
 * @param path the chain of `properties` keys from the schema root to the parameter
 * @param name the name in `Mcp-Param-{name}`
 */
final case class McpParamHeader(path: Chunk[String], name: String):
  /** The header's full name. */
  def headerName: String = s"Mcp-Param-$name"

object McpParamHeader:
  given CanEqual[McpParamHeader, McpParamHeader] = CanEqual.derived

/** Reading, validating and encoding `x-mcp-header` annotations. */
object McpParamHeaders:

  val Annotation = "x-mcp-header"

  /** The largest integer an annotated parameter may carry: JavaScript's safe range. */
  private val MaxSafeInteger: BigDecimal = BigDecimal(9007199254740991L)

  /**
   * The annotations of a tool's `inputSchema`, or why the tool definition is
   * invalid. Each annotation must be a non-empty HTTP token, unique among the
   * schema's annotations regardless of case, on a property typed `string`,
   * `integer` or `boolean`, and reachable from the root through `properties`
   * keys alone. An annotation anywhere else (under `items`, `anyOf`, `$defs`, a
   * `$ref`, ...) makes the definition invalid.
   */
  def annotations(inputSchema: Json.Obj): Either[String, Chunk[McpParamHeader]] =
    val all = occurrences(inputSchema, Chunk.empty)
    val found = all.map: (location, value) =>
      for
        name <- value.asString.toRight(s"'$Annotation' at ${render(location)} is not a string")
        path <- propertyPath(location).toRight(
                  s"'$Annotation' at ${render(location)} is not reachable from the schema root through 'properties' alone")
        _    <- Either.cond(name.nonEmpty, (), s"'$Annotation' at ${render(location)} is empty")
        _    <- Either.cond(name.forall(isTchar), (), s"'$Annotation' value '$name' is not an HTTP header token")
        tpe   = schemaAt(inputSchema, location.dropRight(1)).flatMap(_.get("type")).flatMap(_.asString)
        _    <- Either.cond(tpe.exists(Set("string", "integer", "boolean").contains), (),
                  s"'$Annotation' '$name' is on a parameter of type ${tpe.getOrElse("(none)")}; only string, integer and boolean may be mirrored")
      yield McpParamHeader(path, name)
    for
      headers <- found.foldLeft[Either[String, Chunk[McpParamHeader]]](Right(Chunk.empty)): (acc, next) =>
                   acc.flatMap(hs => next.map(hs :+ _))
      dupes    = headers.groupBy(_.name.toLowerCase).collect { case (n, hs) if hs.size > 1 => n }
      _       <- Either.cond(dupes.isEmpty, (), s"'$Annotation' values are not unique: ${dupes.mkString(", ")}")
    yield headers

  /**
   * The headers for a call's `arguments`: one per annotation whose parameter has
   * a non-null value, encoded per Value Encoding. A value of the wrong kind for
   * its annotation (an object, or an integer out of range) is an error.
   */
  def headers(annotated: Chunk[McpParamHeader], arguments: Json.Obj): Either[String, Chunk[(String, String)]] =
    annotated.foldLeft[Either[String, Chunk[(String, String)]]](Right(Chunk.empty)): (acc, header) =>
      acc.flatMap: out =>
        valueAt(arguments, header.path) match
          case None                   => Right(out)
          case Some(_: Json.Null.type) => Right(out)
          case Some(Json.Str(s))      => Right(out :+ (header.headerName -> encode(s)))
          case Some(Json.Bool(b))     => Right(out :+ (header.headerName -> b.toString))
          case Some(Json.Num(n)) if n.scale <= 0 || n.stripTrailingZeros.scale <= 0 =>
            val value = BigDecimal(n)
            if value.abs > MaxSafeInteger then Left(s"'${header.name}' value $value is outside the safe integer range")
            else Right(out :+ (header.headerName -> value.toBigInt.toString))
          case Some(other) => Left(s"'${header.name}' value ${other.toString} can't be mirrored into a header")

  /**
   * Check a call's `Mcp-Param-*` headers against its `arguments`, as a server
   * that processes the body MUST (Server Behavior for Custom Headers): every
   * annotated parameter with a non-null value needs its header, and the header,
   * Base64-decoded when it carries the sentinel, must equal the value. A header
   * with characters a header value can't hold, or a sentinel that isn't valid
   * Base64, is refused too. `header` looks a header up by name, ignoring case.
   */
  def validate(
    annotated: Chunk[McpParamHeader],
    arguments: Json.Obj,
    header: String => Option[String],
  ): Either[String, Unit] =
    annotated.foldLeft[Either[String, Unit]](Right(())): (acc, param) =>
      acc.flatMap: _ =>
        val expected: Option[Either[String, String]] = valueAt(arguments, param.path) match
          case None                    => None
          case Some(_: Json.Null.type) => None
          case Some(Json.Str(v))       => Some(Right(v))
          case Some(Json.Bool(b))      => Some(Right(b.toString))
          case Some(Json.Num(n))       => Some(Right(BigDecimal(n).bigDecimal.stripTrailingZeros.toPlainString))
          case Some(other)             => Some(Left(s"'${param.name}' has a value that can't be mirrored: $other"))
        (expected, header(param.headerName)) match
          case (None, _) => Right(())
          case (Some(Left(e)), _) => Left(e)
          case (Some(Right(_)), None) => Left(s"Missing ${param.headerName} header for '${param.path.mkString(".")}'")
          case (Some(Right(value)), Some(raw)) =>
            for
              _       <- Either.cond(raw.forall(c => (c >= 0x20 && c <= 0x7e) || c == '\t'), (),
                           s"${param.headerName} holds characters a header value can't carry")
              decoded <- decode(raw).toRight(s"${param.headerName} is not valid Base64")
              same     = valueAt(arguments, param.path) match
                           case Some(Json.Num(n)) => decoded.toDoubleOption.exists(d => BigDecimal(d) == BigDecimal(n))
                           case _                 => decoded == value
              _       <- Either.cond(same, (), s"${param.headerName} '$decoded' does not match the body value '$value'")
            yield ()

  /** A header value as sent: Base64-decoded when it carries the sentinel, `None` when that Base64 is invalid. */
  def decode(raw: String): Option[String] =
    val prefix = "=?base64?"
    val suffix = "?="
    if raw.startsWith(prefix) && raw.endsWith(suffix) && raw.length >= prefix.length + suffix.length then
      val encoded = raw.substring(prefix.length, raw.length - suffix.length)
      // Padded Base64 only: the decoder alone would accept a missing `=`.
      if encoded.length % 4 != 0 then None
      else
        try Some(new String(Base64.getDecoder.decode(encoded), StandardCharsets.UTF_8))
        catch case _: IllegalArgumentException => None
    else Some(raw)

  /**
   * A value as an HTTP header carries it: as-is when it is visible ASCII,
   * spaces and tabs without leading or trailing whitespace, and otherwise (or
   * when it looks like the sentinel) as `=?base64?{UTF-8 Base64}?=`.
   */
  def encode(value: String): String =
    val plain = value.forall(c => (c >= 0x20 && c <= 0x7e) || c == '\t') &&
      value == value.strip() &&
      !(value.startsWith("=?base64?") && value.endsWith("?="))
    if plain then value
    else "=?base64?" + Base64.getEncoder.encodeToString(value.getBytes(StandardCharsets.UTF_8)) + "?="

  private def isTchar(c: Char): Boolean =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || "!#$%&'*+-.^_`|~".contains(c)

  /** Every `x-mcp-header` key in the schema, with the JSON path to it. */
  private def occurrences(json: Json, at: Chunk[String]): Chunk[(Chunk[String], Json)] =
    json match
      case Json.Obj(fields) =>
        fields.flatMap: (key, value) =>
          if key == Annotation then Chunk((at :+ key, value))
          else occurrences(value, at :+ key)
      case Json.Arr(items) =>
        Chunk.fromIterable(items.zipWithIndex).flatMap((item, i) => occurrences(item, at :+ i.toString))
      case _ => Chunk.empty

  /** The property path of an annotation at `properties/a/properties/b/x-mcp-header`. */
  private def propertyPath(location: Chunk[String]): Option[Chunk[String]] =
    val keys = location.dropRight(1)
    if keys.isEmpty || keys.size % 2 != 0 then None
    else
      val pairs = Chunk.fromIterable(keys.grouped(2).map(_.toList).toList)
      if pairs.forall { case "properties" :: _ :: Nil => true; case _ => false } then Some(pairs.map(_(1)))
      else None

  private def schemaAt(schema: Json.Obj, path: Chunk[String]): Option[Json.Obj] =
    path.foldLeft[Option[Json]](Some(schema))((node, key) => node.flatMap(_.asObject).flatMap(_.get(key))).flatMap(_.asObject)

  private def valueAt(arguments: Json.Obj, path: Chunk[String]): Option[Json] =
    path.foldLeft[Option[Json]](Some(arguments))((node, key) => node.flatMap(_.asObject).flatMap(_.get(key)))

  private def render(location: Chunk[String]): String = location.mkString("/")
