package com.jamesward.ziohttp.mcp

import zio.*

/** A [[McpWebhookStore]] in `Ref`s, for tests. `durable` only changes what the server may grant. */
final case class TestWebhookStore(
  durable: Boolean,
  subscriptions: Ref[Map[McpWebhookSubscriptionId, McpWebhookSubscription]],
  verifications: Ref[Map[(McpWebhookPrincipalId, McpWebhookUrl), McpWebhookVerification]],
) extends McpWebhookStore:
  def get(id: McpWebhookSubscriptionId): IO[McpWebhookStoreError, Option[McpWebhookSubscription]] =
    subscriptions.get.map(_.get(id))

  def all: IO[McpWebhookStoreError, Chunk[McpWebhookSubscription]] =
    subscriptions.get.map(m => Chunk.fromIterable(m.values))

  def put(subscription: McpWebhookSubscription): IO[McpWebhookStoreError, Unit] =
    subscriptions.update(_.updated(subscription.id, subscription))

  def update(id: McpWebhookSubscriptionId)(f: McpWebhookSubscription => McpWebhookSubscription)
    : IO[McpWebhookStoreError, Option[McpWebhookSubscription]] =
    subscriptions.modify: m =>
      m.get(id).map(f) match
        case Some(updated) => (Some(updated), m.updated(id, updated))
        case None          => (None, m)

  def delete(id: McpWebhookSubscriptionId): IO[McpWebhookStoreError, Unit] =
    subscriptions.update(_ - id)

  def verification(principal: McpWebhookPrincipalId, url: McpWebhookUrl): IO[McpWebhookStoreError, Option[McpWebhookVerification]] =
    verifications.get.map(_.get((principal, url)))

  def putVerification(verification: McpWebhookVerification): IO[McpWebhookStoreError, Unit] =
    verifications.update(_.updated((verification.principal, verification.url), verification))

object TestWebhookStore:
  def make(durable: Boolean = false): UIO[TestWebhookStore] =
    for
      subscriptions <- Ref.make(Map.empty[McpWebhookSubscriptionId, McpWebhookSubscription])
      verifications <- Ref.make(Map.empty[(McpWebhookPrincipalId, McpWebhookUrl), McpWebhookVerification])
    yield TestWebhookStore(durable, subscriptions, verifications)
