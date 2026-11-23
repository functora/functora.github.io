import assert from "node:assert/strict";
import test from "node:test";

import {
  isObservedEvent,
  markContinuationExhausted,
  reduceSessionControl,
  sessionIDOf,
} from "../dist/policy.js";

const empty = () => new Map();

test("ignores unrelated event types", () => {
  assert.equal(isObservedEvent({ type: "model.updated" }), false);
  assert.equal(
    isObservedEvent({
      type: "session.execution.failed",
      data: { sessionID: "ses_failed" },
    }),
    true,
  );
});

test("continues a failed session", () => {
  const result = reduceSessionControl(empty(), {
    type: "session.execution.failed",
    data: { sessionID: "ses_failed" },
  });

  assert.equal(result.action, "continue");
});

test("does not continue again when the continuation attempt fails", () => {
  const failed = reduceSessionControl(empty(), {
    type: "session.execution.failed",
    data: { sessionID: "ses_failed" },
  });
  const started = reduceSessionControl(failed.state, {
    type: "session.execution.started",
    data: { sessionID: "ses_failed" },
  });
  const continuationFailed = reduceSessionControl(started.state, {
    type: "session.execution.failed",
    data: { sessionID: "ses_failed" },
  });

  assert.equal(failed.action, "continue");
  assert.equal(started.action, "stop");
  assert.equal(continuationFailed.action, "stop");
});

test("does not retry when the continuation request fails", () => {
  const failed = reduceSessionControl(empty(), {
    type: "session.execution.failed",
    data: { sessionID: "ses_failed" },
  });
  const exhausted = markContinuationExhausted(failed.state, "ses_failed");
  const continuationFailed = reduceSessionControl(exhausted, {
    type: "session.execution.failed",
    data: { sessionID: "ses_failed" },
  });

  assert.equal(continuationFailed.action, "stop");
});

test("continues a later independently started execution", () => {
  const failed = reduceSessionControl(empty(), {
    type: "session.execution.failed",
    data: { sessionID: "ses_failed" },
  });
  const continuationFailed = reduceSessionControl(failed.state, {
    type: "session.execution.failed",
    data: { sessionID: "ses_failed" },
  });
  const started = reduceSessionControl(continuationFailed.state, {
    type: "session.execution.started",
    data: { sessionID: "ses_failed" },
  });
  const laterFailed = reduceSessionControl(started.state, {
    type: "session.execution.failed",
    data: { sessionID: "ses_failed" },
  });

  assert.equal(continuationFailed.action, "stop");
  assert.equal(started.action, "stop");
  assert.equal(laterFailed.action, "continue");
});

test("stops after successful execution", () => {
  const result = reduceSessionControl(empty(), {
    type: "session.execution.succeeded",
    data: { sessionID: "ses_succeeded" },
  });

  assert.equal(result.action, "stop");
});

test("waits for permission and form answers", () => {
  const permission = reduceSessionControl(empty(), {
    type: "permission.asked",
    data: { id: "per_1", sessionID: "ses_waiting" },
  });
  const form = reduceSessionControl(permission.state, {
    type: "form.created",
    data: {
      form: { id: "form_1", sessionID: "ses_waiting" },
    },
  });

  assert.equal(permission.action, "wait");
  assert.equal(form.action, "wait");
  assert.equal(form.state.get("ses_waiting")?.pending.size, 2);
});

test("does not continue while user input is pending", () => {
  const asked = reduceSessionControl(empty(), {
    type: "permission.asked",
    data: { id: "per_1", sessionID: "ses_waiting" },
  });
  const failed = reduceSessionControl(asked.state, {
    type: "session.execution.failed",
    data: { sessionID: "ses_waiting" },
  });

  const control = failed.state.get("ses_waiting");
  assert.equal(failed.action, "wait");
  assert.equal(
    control?.status === "waiting" && control.failure.status === "pending",
    true,
  );
});

test("continues after user input resolves a failed session", () => {
  const asked = reduceSessionControl(empty(), {
    type: "permission.asked",
    data: { id: "per_1", sessionID: "ses_waiting" },
  });
  const failed = reduceSessionControl(asked.state, {
    type: "session.execution.failed",
    data: { sessionID: "ses_waiting" },
  });
  const replied = reduceSessionControl(failed.state, {
    type: "permission.replied",
    data: { requestID: "per_1", sessionID: "ses_waiting" },
  });

  assert.equal(replied.action, "continue");
});

test("continues after a form answer resolves a failed session", () => {
  const asked = reduceSessionControl(empty(), {
    type: "form.created",
    data: {
      form: { id: "form_1", sessionID: "ses_waiting" },
    },
  });
  const failed = reduceSessionControl(asked.state, {
    type: "session.execution.failed",
    data: { sessionID: "ses_waiting" },
  });
  const replied = reduceSessionControl(failed.state, {
    type: "form.replied",
    data: { id: "form_1", sessionID: "ses_waiting" },
  });

  assert.equal(replied.action, "continue");
});

test("continues when another pending input received a reply", () => {
  const permission = reduceSessionControl(empty(), {
    type: "permission.asked",
    data: { id: "per_1", sessionID: "ses_waiting" },
  });
  const form = reduceSessionControl(permission.state, {
    type: "form.created",
    data: {
      form: { id: "form_1", sessionID: "ses_waiting" },
    },
  });
  const failed = reduceSessionControl(form.state, {
    type: "session.execution.failed",
    data: { sessionID: "ses_waiting" },
  });
  const replied = reduceSessionControl(failed.state, {
    type: "permission.replied",
    data: { requestID: "per_1", sessionID: "ses_waiting" },
  });
  const cancelled = reduceSessionControl(replied.state, {
    type: "form.cancelled",
    data: { id: "form_1", sessionID: "ses_waiting" },
  });

  assert.equal(replied.action, "wait");
  assert.equal(cancelled.action, "continue");
});

test("does not count a reply before failure as the post-failure reply", () => {
  const permission = reduceSessionControl(empty(), {
    type: "permission.asked",
    data: { id: "per_1", sessionID: "ses_waiting" },
  });
  const form = reduceSessionControl(permission.state, {
    type: "form.created",
    data: {
      form: { id: "form_1", sessionID: "ses_waiting" },
    },
  });
  const replied = reduceSessionControl(form.state, {
    type: "permission.replied",
    data: { requestID: "per_1", sessionID: "ses_waiting" },
  });
  const failed = reduceSessionControl(replied.state, {
    type: "session.execution.failed",
    data: { sessionID: "ses_waiting" },
  });
  const cancelled = reduceSessionControl(failed.state, {
    type: "form.cancelled",
    data: { id: "form_1", sessionID: "ses_waiting" },
  });

  assert.equal(replied.action, "wait");
  assert.equal(failed.action, "wait");
  assert.equal(cancelled.action, "stop");
});

test("does not continue after a pending form is cancelled", () => {
  const asked = reduceSessionControl(empty(), {
    type: "form.created",
    data: {
      form: { id: "form_1", sessionID: "ses_waiting" },
    },
  });
  const failed = reduceSessionControl(asked.state, {
    type: "session.execution.failed",
    data: { sessionID: "ses_waiting" },
  });
  const cancelled = reduceSessionControl(failed.state, {
    type: "form.cancelled",
    data: { id: "form_1", sessionID: "ses_waiting" },
  });

  assert.equal(cancelled.action, "stop");
});

test("does not continue when failure follows a cancelled form", () => {
  const asked = reduceSessionControl(empty(), {
    type: "form.created",
    data: {
      form: { id: "form_1", sessionID: "ses_waiting" },
    },
  });
  const cancelled = reduceSessionControl(asked.state, {
    type: "form.cancelled",
    data: { id: "form_1", sessionID: "ses_waiting" },
  });
  const failed = reduceSessionControl(cancelled.state, {
    type: "session.execution.failed",
    data: { sessionID: "ses_waiting" },
  });

  assert.equal(cancelled.action, "stop");
  assert.equal(failed.action, "stop");
});

test("does not continue when a reply follows a cancellation", () => {
  const permission = reduceSessionControl(empty(), {
    type: "permission.asked",
    data: { id: "per_1", sessionID: "ses_waiting" },
  });
  const form = reduceSessionControl(permission.state, {
    type: "form.created",
    data: {
      form: { id: "form_1", sessionID: "ses_waiting" },
    },
  });
  const cancelled = reduceSessionControl(form.state, {
    type: "form.cancelled",
    data: { id: "form_1", sessionID: "ses_waiting" },
  });
  const failed = reduceSessionControl(cancelled.state, {
    type: "session.execution.failed",
    data: { sessionID: "ses_waiting" },
  });
  const replied = reduceSessionControl(failed.state, {
    type: "permission.replied",
    data: { requestID: "per_1", sessionID: "ses_waiting" },
  });

  assert.equal(cancelled.action, "wait");
  assert.equal(failed.action, "wait");
  assert.equal(replied.action, "stop");
});

test("does not retry a failure after its pending form was cancelled", () => {
  const asked = reduceSessionControl(empty(), {
    type: "form.created",
    data: {
      form: { id: "form_1", sessionID: "ses_waiting" },
    },
  });
  const failed = reduceSessionControl(asked.state, {
    type: "session.execution.failed",
    data: { sessionID: "ses_waiting" },
  });
  const cancelled = reduceSessionControl(failed.state, {
    type: "form.cancelled",
    data: { id: "form_1", sessionID: "ses_waiting" },
  });
  const duplicateFailure = reduceSessionControl(cancelled.state, {
    type: "session.execution.failed",
    data: { sessionID: "ses_waiting" },
  });

  assert.equal(cancelled.action, "stop");
  assert.equal(duplicateFailure.action, "stop");
});

test("continues a later independently started execution after cancellation", () => {
  const asked = reduceSessionControl(empty(), {
    type: "form.created",
    data: {
      form: { id: "form_1", sessionID: "ses_waiting" },
    },
  });
  const cancelled = reduceSessionControl(asked.state, {
    type: "form.cancelled",
    data: { id: "form_1", sessionID: "ses_waiting" },
  });
  const failed = reduceSessionControl(cancelled.state, {
    type: "session.execution.failed",
    data: { sessionID: "ses_waiting" },
  });
  const started = reduceSessionControl(failed.state, {
    type: "session.execution.started",
    data: { sessionID: "ses_waiting" },
  });
  const laterFailure = reduceSessionControl(started.state, {
    type: "session.execution.failed",
    data: { sessionID: "ses_waiting" },
  });

  assert.equal(failed.action, "stop");
  assert.equal(started.action, "stop");
  assert.equal(laterFailure.action, "continue");
});

test("does not continue again when a pending-input continuation fails", () => {
  const asked = reduceSessionControl(empty(), {
    type: "permission.asked",
    data: { id: "per_1", sessionID: "ses_waiting" },
  });
  const failed = reduceSessionControl(asked.state, {
    type: "session.execution.failed",
    data: { sessionID: "ses_waiting" },
  });
  const replied = reduceSessionControl(failed.state, {
    type: "permission.replied",
    data: { requestID: "per_1", sessionID: "ses_waiting" },
  });
  const continuationFailed = reduceSessionControl(replied.state, {
    type: "session.execution.failed",
    data: { sessionID: "ses_waiting" },
  });

  assert.equal(replied.action, "continue");
  assert.equal(continuationFailed.action, "stop");
});

test("does not continue after a normal user reply", () => {
  const asked = reduceSessionControl(empty(), {
    type: "form.created",
    data: {
      form: { id: "form_1", sessionID: "ses_waiting" },
    },
  });
  const replied = reduceSessionControl(asked.state, {
    type: "form.replied",
    data: { id: "form_1", sessionID: "ses_waiting" },
  });

  assert.equal(replied.action, "stop");
  assert.equal(replied.state.size, 0);
});

test("clears pending input after success or interruption", () => {
  const asked = reduceSessionControl(empty(), {
    type: "permission.asked",
    data: { id: "per_1", sessionID: "ses_waiting" },
  });
  const succeeded = reduceSessionControl(asked.state, {
    type: "session.execution.succeeded",
    data: { sessionID: "ses_waiting" },
  });
  const interrupted = reduceSessionControl(asked.state, {
    type: "session.execution.interrupted",
    data: { sessionID: "ses_waiting" },
  });

  assert.equal(succeeded.action, "stop");
  assert.equal(interrupted.action, "stop");
  assert.equal(succeeded.state.size, 0);
  assert.equal(interrupted.state.size, 0);
});

test("extracts the session id from every observed event", () => {
  assert.equal(
    sessionIDOf({
      type: "form.created",
      data: {
        form: { id: "form_1", sessionID: "ses_form" },
      },
    }),
    "ses_form",
  );
  assert.equal(
    sessionIDOf({
      type: "session.execution.failed",
      data: { sessionID: "ses_failed" },
    }),
    "ses_failed",
  );
});
