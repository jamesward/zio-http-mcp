// Mastra's MCP client (@mastra/mcp). Its webhook receiver is the client itself (events.handleWebhook), and a
// subscription is an async iterable of occurrences.
import { MCPClient } from '@mastra/mcp';

export function make({ callbackUrl, onEvent, onError }) {
  let client;
  const webhook = (request) => client.events.handleWebhook(request);
  const connect = async (url) => {
    client = new MCPClient({
      servers: { test: { url: new URL(url) } },
      webhook: { url: callbackUrl },
      registerProcessHooks: false,
    });
    return {
      subscribe: async () => {
        const subscription = await client.events.subscribe('test', 'incident.created', { severity: 'P1' }, { delivery: 'webhook' });
        subscription.onError(onError);
        (async () => {
          for await (const occurrence of subscription) await onEvent(occurrence);
        })().catch(onError);
        return { label: subscription.mode, unsubscribe: () => subscription.cancel() };
      },
      close: () => client.disconnect(),
    };
  };
  return { webhook, connect };
}
