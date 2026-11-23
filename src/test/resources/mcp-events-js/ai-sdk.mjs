// The Vercel AI SDK's MCP client (@ai-sdk/mcp) and its MCP Events webhook handler.
import { createMCPClient, experimental_createMCPEventWebhook } from '@ai-sdk/mcp';

/** The AI SDK ships no store: a Map, keyed by the client's routing key. */
function memoryStore() {
  const byKey = new Map();
  return {
    get: async (key) => byKey.get(key),
    getById: async (id) => [...byKey.values()].find((s) => s.id === id),
    set: async (subscription) => void byKey.set(subscription.key, structuredClone(subscription)),
    update: async (key, patch) => void byKey.set(key, { ...byKey.get(key), ...patch }),
    delete: async (key) => void byKey.delete(key),
  };
}

export function make({ callbackUrl, onEvent, onError }) {
  const store = memoryStore();
  const webhook = experimental_createMCPEventWebhook({ store, onEvent: ({ event }) => onEvent(event), onError });
  const connect = async (url) => {
    const client = await createMCPClient({ transport: { type: 'http', url }, experimental_events: { store } });
    return {
      subscribe: async () => {
        const { id } = await client.experimental_events.subscribe({
          name: 'incident.created',
          arguments: { severity: 'P1' },
          delivery: { mode: 'webhook', url: callbackUrl, allowInsecureLocalhost: true },
        });
        return { label: id, unsubscribe: () => client.experimental_events.unsubscribe({ id }) };
      },
      close: () => client.close(),
    };
  };
  return { webhook, connect };
}
