// Subscribes to `incident.created` with one JS MCP client's MCP Events support, receives one event at a webhook on
// 127.0.0.1, then unsubscribes. Usage: node probe.mjs <ai-sdk|tanstack|mastra> <MCP endpoint URL>
// Each client module exports make({ callbackUrl, onEvent, onError }), answering { webhook, connect }: webhook is a Web
// Request handler, and connect(mcpUrl) answers { subscribe(), close() }, where subscribe() answers { label, unsubscribe() }.
// Prints RESULT lines for the spec to read:
//   RESULT subscribed <subscription id, or delivery mode>
//   RESULT verification <status> skew=<seconds>           the challenge, as the client's webhook answered it, and how far
//                                                         its webhook-timestamp was from this container's clock
//   RESULT delivery <status> <webhook-id> <body eventId>  each event POST, as the client's webhook answered it
//   RESULT event <eventId> <name> <data as JSON>          what the client's onEvent got
//   RESULT timeout                                        no onEvent within EVENT_TIMEOUT_MS
//   RESULT unsubscribed
//   RESULT error <message>
import http from 'node:http';

const EVENT_TIMEOUT_MS = 20_000;
const [sdk, mcpUrl] = process.argv.slice(2);
const result = (...parts) => console.log(['RESULT', ...parts].join(' '));
const oneLine = (value) => String(value).replace(/\s+/g, ' ');

const clients = {
  'ai-sdk': () => import('./ai-sdk.mjs'),
  tanstack: () => import('./tanstack.mjs'),
  mastra: () => import('./mastra.mjs'),
};

/** A webhook receiver on 127.0.0.1 that hands each request to `handle` (a Web Request handler) and records its answer. */
function callbackServer(handle) {
  const server = http.createServer(async (req, res) => {
    const chunks = [];
    for await (const chunk of req) chunks.push(chunk);
    const body = Buffer.concat(chunks);
    const request = new Request(`http://127.0.0.1:${server.address().port}${req.url}`, {
      method: req.method,
      headers: Object.entries(req.headers).flatMap(([k, v]) => (Array.isArray(v) ? v.map((x) => [k, x]) : [[k, v]])),
      body: req.method === 'GET' || req.method === 'HEAD' ? undefined : body,
    });
    let response;
    try {
      response = await handle(request);
    } catch (e) {
      result('error', oneLine(`webhook handler threw: ${e?.stack ?? e}`));
      response = new Response(null, { status: 500 });
    }
    let parsed = {};
    try {
      parsed = JSON.parse(body.toString('utf8'));
    } catch {}
    const skew = Math.round(Date.now() / 1000 - Number(req.headers['webhook-timestamp']));
    if (parsed.type === 'verification') result('verification', response.status, `skew=${skew}`);
    else result('delivery', response.status, req.headers['webhook-id'] ?? '-', parsed.eventId ?? '-');
    res.writeHead(response.status, Object.fromEntries(response.headers));
    res.end(Buffer.from(await response.arrayBuffer()));
  });
  return new Promise((resolve) => server.listen(0, '127.0.0.1', () => resolve(server)));
}

async function main() {
  const client = await clients[sdk]();
  let received;
  const event = new Promise((resolve) => (received = resolve));
  let webhook;
  const server = await callbackServer((request) => webhook(request));
  const made = client.make({
    callbackUrl: `http://127.0.0.1:${server.address().port}/hooks`,
    onEvent: async (e) => received(e),
    onError: (e) => result('error', oneLine(`webhook: ${e?.message ?? e}${e?.cause ? ` (${e.cause?.message ?? e.cause})` : ''}`)),
  });
  webhook = made.webhook;
  const mcp = await made.connect(mcpUrl);
  try {
    const subscription = await mcp.subscribe();
    result('subscribed', subscription.label);
    const timeout = new Promise((resolve) => setTimeout(() => resolve(undefined), EVENT_TIMEOUT_MS));
    const got = await Promise.race([event, timeout]);
    if (got) result('event', got.eventId, got.name, JSON.stringify(got.data));
    else result('timeout');
    await subscription.unsubscribe();
    result('unsubscribed');
  } finally {
    await mcp.close();
    server.close();
  }
}

main()
  .catch((e) => result('error', oneLine(e?.stack ?? e)))
  .finally(() => process.exit(0));
