package com.jamesward.ziohttp.mcp

import zio.json.*

import java.util.regex.Pattern

enum McpExtensionIdError:
  case Empty
  case Invalid(value: String)

object McpExtensionIdError:
  given CanEqual[McpExtensionIdError, McpExtensionIdError] = CanEqual.derived

opaque type McpExtensionId = String

object McpExtensionId:
  private val pattern = Pattern.compile(
    "(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?/[A-Za-z0-9](?:[A-Za-z0-9._-]*[A-Za-z0-9])?"
  )

  def parse(value: String): Either[McpExtensionIdError, McpExtensionId] =
    if value.isEmpty then Left(McpExtensionIdError.Empty)
    else if pattern.matcher(value).matches() then Right(value)
    else Left(McpExtensionIdError.Invalid(value))


  private[mcp] def fromValid(value: String): McpExtensionId = value
  extension (id: McpExtensionId) def value: String = id

  given CanEqual[McpExtensionId, McpExtensionId] = CanEqual.derived
  given JsonEncoder[McpExtensionId] = JsonEncoder.string
  given JsonDecoder[McpExtensionId] = JsonDecoder.string.mapOrFail: value =>
    parse(value).left.map(_.toString)

enum McpMethodNameError:
  case Empty
  case Invalid(value: String)

object McpMethodNameError:
  given CanEqual[McpMethodNameError, McpMethodNameError] = CanEqual.derived

opaque type McpMethodName = String

object McpMethodName:
  private val pattern = Pattern.compile(
    "[A-Za-z0-9](?:[A-Za-z0-9._-]*[A-Za-z0-9])?(?:/[A-Za-z0-9](?:[A-Za-z0-9._-]*[A-Za-z0-9])?)*"
  )

  def parse(value: String): Either[McpMethodNameError, McpMethodName] =
    if value.isEmpty then Left(McpMethodNameError.Empty)
    else if pattern.matcher(value).matches() then Right(value)
    else Left(McpMethodNameError.Invalid(value))

  private[mcp] def fromValid(value: String): McpMethodName = value

  extension (name: McpMethodName) def value: String = name

  given CanEqual[McpMethodName, McpMethodName] = CanEqual.derived
  given JsonEncoder[McpMethodName] = JsonEncoder.string
  given JsonDecoder[McpMethodName] = JsonDecoder.string.mapOrFail: value =>
    parse(value).left.map(_.toString)

enum McpRoutingNameError:
  case Empty
  case ContainsControlCharacter

object McpRoutingNameError:
  given CanEqual[McpRoutingNameError, McpRoutingNameError] = CanEqual.derived

opaque type McpRoutingName = String

object McpRoutingName:
  def parse(value: String): Either[McpRoutingNameError, McpRoutingName] =
    if value.isEmpty then Left(McpRoutingNameError.Empty)
    else if value.exists(_.isControl) then Left(McpRoutingNameError.ContainsControlCharacter)
    else Right(value)

  extension (name: McpRoutingName) def value: String = name


  private[mcp] def fromValid(value: String): McpRoutingName = value
  given CanEqual[McpRoutingName, McpRoutingName] = CanEqual.derived
