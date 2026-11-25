import assert from "node:assert/strict";
import test from "node:test";

import { setupContinuePlugin } from "../dist/plugin.js";

const LOCATION = "/tmp/opencode-continue-test";

function createHarness() {
  const commands = new Map();
  const queuedEvents = [];
  const syntheticCalls = [];
  const getWaiters = [];
  const syntheticWaiters = [];
  let eventResolver;
  let getCount = 0;

  const notify = (waiters) => waiters.splice(0).forEach((resolve) => resolve());

  const ctx = {
    location: {
      directory: LOCATION,
      project: {
        id: "project_test",
        directory: LOCATION,
        canonical: LOCATION,
      },
    },
    command: {
      transform: async (callback) => {
        callback({
          add: (definition) => commands.set(definition.name, definition),
        });
        return { dispose: async () => {} };
      },
    },
    session: {
      get: async () => {
        getCount += 1;
        notify(getWaiters);
        return { location: { directory: LOCATION } };
      },
      synthetic: async (input) => {
        syntheticCalls.push(input);
        notify(syntheticWaiters);
      },
    },
    event: {
      subscribe: ({ signal } = {}) => ({
        async *[Symbol.asyncIterator]() {
          while (!signal?.aborted) {
            const event = queuedEvents.length
              ? queuedEvents.shift()
              : await new Promise((resolve) => {
                  eventResolver = resolve;
                  signal?.addEventListener(
                    "abort",
                    () => {
                      eventResolver = undefined;
                      resolve(undefined);
                    },
                    { once: true },
                  );
                });
            if (!event) return;
            yield event;
          }
        },
      }),
    },
  };

  return {
    ctx,
    commands,
    syntheticCalls,
    emit(event) {
      if (eventResolver) {
        const resolve = eventResolver;
        eventResolver = undefined;
        resolve(event);
      } else {
        queuedEvents.push(event);
      }
    },
    async waitForGets(count) {
      if (getCount >= count) return;
      await new Promise((resolve) => getWaiters.push(resolve));
    },
    async waitForSynthetic(count) {
      if (syntheticCalls.length >= count) return;
      await new Promise((resolve) => syntheticWaiters.push(resolve));
    },
  };
}

function failed(sessionID) {
  return {
    type: "session.execution.failed",
    data: { sessionID },
  };
}

function succeeded(sessionID) {
  return {
    type: "session.execution.succeeded",
    data: { sessionID },
  };
}

async function executeCommand(harness, sessionID, text) {
  const command = harness.commands.get("auto-continue");
  assert.ok(command);
  await command.execute({
    sessionID,
    prompt: { text },
    delivery: "steer",
  });
}

test("registers the auto-continue command", async () => {
  const harness = createHarness();
  const cleanup = await setupContinuePlugin(harness.ctx);

  assert.equal(harness.commands.get("auto-continue")?.name, "auto-continue");
  await cleanup();
});

test("does not continue by default", async () => {
  const harness = createHarness();
  const cleanup = await setupContinuePlugin(harness.ctx);

  harness.emit(failed("ses_default"));
  harness.emit(succeeded("ses_default"));
  await harness.waitForGets(2);

  assert.deepEqual(harness.syntheticCalls, []);
  await cleanup();
});

test("continues an enabled session exactly once", async () => {
  const harness = createHarness();
  const cleanup = await setupContinuePlugin(harness.ctx);
  await executeCommand(harness, "ses_enabled", "on");

  harness.emit(failed("ses_enabled"));
  await harness.waitForSynthetic(1);

  assert.deepEqual(harness.syntheticCalls, [
    {
      sessionID: "ses_enabled",
      text: "continue",
      metadata: { source: "opencode-continue" },
    },
  ]);
  await cleanup();
});

test("does not continue after disabling a session", async () => {
  const harness = createHarness();
  const cleanup = await setupContinuePlugin(harness.ctx);
  await executeCommand(harness, "ses_disabled", "on");
  await executeCommand(harness, "ses_disabled", "off");

  harness.emit(failed("ses_disabled"));
  harness.emit(succeeded("ses_disabled"));
  await harness.waitForGets(2);

  assert.deepEqual(harness.syntheticCalls, []);
  await cleanup();
});

test("tracks the mode independently per session", async () => {
  const harness = createHarness();
  const cleanup = await setupContinuePlugin(harness.ctx);
  await executeCommand(harness, "ses_enabled", "on");

  harness.emit(failed("ses_enabled"));
  await harness.waitForSynthetic(1);
  harness.emit(failed("ses_disabled"));
  harness.emit(succeeded("ses_disabled"));
  await harness.waitForGets(3);

  assert.deepEqual(
    harness.syntheticCalls.map(({ sessionID }) => sessionID),
    ["ses_enabled"],
  );
  await cleanup();
});

test("clears the mode when a session is deleted", async () => {
  const harness = createHarness();
  const cleanup = await setupContinuePlugin(harness.ctx);
  await executeCommand(harness, "ses_deleted", "on");

  harness.emit({
    type: "session.deleted",
    data: { sessionID: "ses_deleted" },
  });
  harness.emit(failed("ses_deleted"));
  harness.emit(succeeded("ses_deleted"));
  await harness.waitForGets(2);

  assert.deepEqual(harness.syntheticCalls, []);
  await cleanup();
});

test("rejects invalid command arguments", async () => {
  const harness = createHarness();
  const cleanup = await setupContinuePlugin(harness.ctx);

  await assert.rejects(
    executeCommand(harness, "ses_invalid", "maybe"),
    /auto-continue expects "on" or "off"/,
  );
  await cleanup();
});
