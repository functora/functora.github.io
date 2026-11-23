import assert from "node:assert/strict";
import test from "node:test";

import { eventBelongsToLocation } from "../dist/location.js";

const pluginLocation = { directory: "/tmp/opencode" };

test("rejects a global event for a session owned by another location", () => {
  assert.equal(
    eventBelongsToLocation(pluginLocation, undefined, {
      directory: "/home/x/project",
    }),
    false,
  );
});

test("accepts a global event for a session in the plugin location", () => {
  assert.equal(
    eventBelongsToLocation(pluginLocation, undefined, pluginLocation),
    true,
  );
});

test("rejects an event whose location differs from the plugin location", () => {
  assert.equal(
    eventBelongsToLocation(
      pluginLocation,
      { directory: "/home/x/project" },
      pluginLocation,
    ),
    false,
  );
});

test("accepts a public location for a workspace-backed plugin", () => {
  assert.equal(
    eventBelongsToLocation(
      { directory: "/tmp/opencode", workspaceID: "workspace-a" },
      undefined,
      { directory: "/tmp/opencode" },
    ),
    true,
  );
});
