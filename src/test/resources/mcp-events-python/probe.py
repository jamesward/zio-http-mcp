# Subscribes to `incident.created` with the Python MCP SDK's Events extension (python-sdk PR 3822), receives one event
# at a WebhookReceiver on 127.0.0.1, then unsubscribes. Usage: python probe.py <MCP endpoint URL>
# Prints the same RESULT lines as ../mcp-events-js/probe.mjs:
#   RESULT subscribed <subscription id>
#   RESULT verification <status> skew=<seconds>
#   RESULT delivery <status> <webhook-id> <body eventId>
#   RESULT event <eventId> <name> <data as JSON>
#   RESULT timeout
#   RESULT unsubscribed
#   RESULT error <message>
import base64
import json
import secrets
import socket
import sys
import time
import traceback

import anyio
import uvicorn

from mcp.client import Client
from mcp.experimental.events.client import Events
from mcp.experimental.events.receiver import WebhookReceiver
from mcp.experimental.events.types import EventOccurrence

EVENT_TIMEOUT_SECONDS = 20


def result(*parts: object) -> None:
    print(" ".join(["RESULT", *(" ".join(str(p).split()) for p in parts)]), flush=True)


class Recorded:
    """Wraps the receiver to print each request and the status the receiver answered it with."""

    def __init__(self, app):
        self.app = app

    async def __call__(self, scope, receive, send):
        if scope["type"] != "http":
            return await self.app(scope, receive, send)
        chunks = []

        async def teed():
            message = await receive()
            chunks.append(message.get("body", b""))
            return message

        status = {}

        async def sending(message):
            if message["type"] == "http.response.start":
                status["code"] = message["status"]
            await send(message)

        await self.app(scope, teed, sending)
        headers = {k.decode().lower(): v.decode() for k, v in scope["headers"]}
        try:
            body = json.loads(b"".join(chunks) or b"{}")
        except ValueError:
            body = {}
        if body.get("type") == "verification":
            skew = round(time.time() - int(headers.get("webhook-timestamp", "0")))
            result("verification", status.get("code"), f"skew={skew}")
        else:
            result("delivery", status.get("code"), headers.get("webhook-id", "-"), body.get("eventId", "-"))


def free_port() -> int:
    with socket.socket() as s:
        s.bind(("127.0.0.1", 0))
        return s.getsockname()[1]


async def main(mcp_url: str) -> None:
    secret = "whsec_" + base64.b64encode(secrets.token_bytes(32)).decode()
    received = anyio.Event()
    got: list[EventOccurrence] = []

    async def handle(delivery) -> None:
        if isinstance(delivery, EventOccurrence):
            got.append(delivery)
            received.set()

    port = free_port()
    server = uvicorn.Server(uvicorn.Config(Recorded(WebhookReceiver(secret, handle)), host="127.0.0.1", port=port,
                                           log_level="warning"))
    async with anyio.create_task_group() as task_group:
        task_group.start_soon(server.serve)
        while not server.started:
            await anyio.sleep(0.05)
        try:
            async with Client(mcp_url, extensions=[Events()]) as client:
                events = client.extension(Events)
                async with events.subscribe("incident.created", {"severity": "P1"},
                                            url=f"http://127.0.0.1:{port}/hooks", secret=secret) as subscription:
                    result("subscribed", subscription.id)
                    with anyio.move_on_after(EVENT_TIMEOUT_SECONDS):
                        await received.wait()
                    if got:
                        e = got[0]
                        result("event", e.event_id, e.name, json.dumps(e.data, separators=(",", ":")))
                    else:
                        result("timeout")
                result("unsubscribed")
        except BaseException as e:  # noqa: BLE001 - every failure is a RESULT line for the spec
            for leaf in leaves(e):
                result("error", "".join(traceback.format_exception_only(leaf)))
        finally:
            server.should_exit = True


def leaves(e: BaseException):
    if isinstance(e, BaseExceptionGroup):
        for x in e.exceptions:
            yield from leaves(x)
    else:
        yield e


anyio.run(main, sys.argv[1])
