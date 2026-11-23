// TanStack AI's MCP client (@tanstack/ai-mcp) and its MCP Events webhook handler.
import { createMCPClient } from '@tanstack/ai-mcp';
import { createMCPEventWebhook, inMemoryMCPEventStore } from '@tanstack/ai-mcp/events';

export function make({ callbackUrl, onEvent, onError }) {
  const store = inMemoryMCPEventStore();
  const webhook = createMCPEventWebhook({ store, onEvent: ({ event }) => onEvent(event), onError });
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
