import assert from "node:assert/strict";
import test from "node:test";

import {
  autoContinueModeOf,
  parseAutoContinueMode,
  setAutoContinueMode,
} from "../dist/runtime.js";

test("defaults auto continuation to disabled", () => {
  assert.equal(autoContinueModeOf(new Map(), "ses_default"), "disabled");
});

test("parses explicit on and off arguments", () => {
  assert.equal(parseAutoContinueMode("on"), "enabled");
  assert.equal(parseAutoContinueMode(" OFF "), "disabled");
});

test("rejects invalid auto continuation arguments", () => {
  assert.throws(() => parseAutoContinueMode("maybe"), /on.*off/i);
});

test("tracks auto continuation independently per session", () => {
  const enabled = setAutoContinueMode(new Map(), "ses_enabled", "enabled");
  const disabled = setAutoContinueMode(enabled, "ses_enabled", "disabled");

  assert.equal(autoContinueModeOf(enabled, "ses_enabled"), "enabled");
  assert.equal(autoContinueModeOf(enabled, "ses_other"), "disabled");
  assert.equal(autoContinueModeOf(disabled, "ses_enabled"), "disabled");
});
