import type { AutoContinueMode } from "./runtime.js";

export const OBSERVED_EVENT_TYPES = [
  "session.execution.started",
  "session.execution.succeeded",
  "session.execution.failed",
  "session.execution.interrupted",
  "session.deleted",
  "permission.asked",
  "permission.replied",
  "form.created",
  "form.replied",
  "form.cancelled",
] as const;

export type ObservedEventType = (typeof OBSERVED_EVENT_TYPES)[number];

const OBSERVED_EVENT_TYPE_SET: ReadonlySet<string> = new Set(
  OBSERVED_EVENT_TYPES,
);

export function isObservedEvent(event: {
  readonly type: string;
}): event is ObservedEvent {
  return OBSERVED_EVENT_TYPE_SET.has(event.type);
}

export type ObservedEvent =
  | {
      readonly type: "session.execution.started";
      readonly data: { readonly sessionID: string };
    }
  | {
      readonly type: "session.execution.succeeded";
      readonly data: { readonly sessionID: string };
    }
  | {
      readonly type: "session.execution.failed";
      readonly data: { readonly sessionID: string };
    }
  | {
      readonly type: "session.execution.interrupted";
      readonly data: { readonly sessionID: string };
    }
  | {
      readonly type: "session.deleted";
      readonly data: { readonly sessionID: string };
    }
  | {
      readonly type: "permission.asked";
      readonly data: { readonly id: string; readonly sessionID: string };
    }
  | {
      readonly type: "permission.replied";
      readonly data: { readonly requestID: string; readonly sessionID: string };
    }
  | {
      readonly type: "form.created";
      readonly data: {
        readonly form: { readonly id: string; readonly sessionID: string };
      };
    }
  | {
      readonly type: "form.replied";
      readonly data: { readonly id: string; readonly sessionID: string };
    }
  | {
      readonly type: "form.cancelled";
      readonly data: { readonly id: string; readonly sessionID: string };
    };

type CancelledFailureState =
  | { readonly status: "cancelled-before-failure" }
  | { readonly status: "cancelled-after-failure" };

type DisabledFailureState = { readonly status: "disabled" };

type SuppressedFailureState =
  | { readonly status: "pending"; readonly replied: false }
  | DisabledFailureState
  | CancelledFailureState;

export type FailureState =
  | { readonly status: "none" }
  | { readonly status: "pending"; readonly replied: boolean }
  | DisabledFailureState
  | CancelledFailureState;

export type SessionControl =
  | {
      readonly status: "waiting";
      readonly pending: ReadonlySet<string>;
      readonly origin: "execution" | "continuation";
      readonly failure: FailureState;
    }
  | {
      readonly status: "continuation-pending";
    }
  | {
      readonly status: "continuation-exhausted";
    }
  | {
      readonly status: "failure-suppressed";
      readonly failure: SuppressedFailureState;
    };

export type SessionControls = ReadonlyMap<string, SessionControl>;

export type ContinuationAction = "continue" | "wait" | "stop";

export interface ControlUpdate {
  readonly state: SessionControls;
  readonly action: ContinuationAction;
}

function updateControl(
  state: SessionControls,
  sessionID: string,
  control?: SessionControl,
): SessionControls {
  const next = new Map(state);
  if (control) {
    next.set(sessionID, control);
  } else {
    next.delete(sessionID);
  }
  return next;
}

export function clearSessionControl(
  state: SessionControls,
  sessionID: string,
): SessionControls {
  return updateControl(state, sessionID);
}

export function suppressSessionContinuation(
  state: SessionControls,
  sessionID: string,
): SessionControls {
  const current = state.get(sessionID);
  if (current?.status !== "waiting") {
    return state;
  }
  return updateControl(state, sessionID, {
    ...current,
    failure:
      current.failure.status === "pending"
        ? { status: "disabled" }
        : current.failure,
  });
}

export function markContinuationExhausted(
  state: SessionControls,
  sessionID: string,
): SessionControls {
  return updateControl(state, sessionID, { status: "continuation-exhausted" });
}

function addPending(
  state: SessionControls,
  sessionID: string,
  key: string,
): ControlUpdate {
  const current = state.get(sessionID);
  if (current?.status === "continuation-exhausted") {
    return { state, action: "stop" };
  }

  const pending = new Set(
    current?.status === "waiting" ? current.pending : undefined,
  );
  pending.add(key);
  const failure: FailureState =
    current?.status === "waiting"
      ? current.failure
      : current?.status === "failure-suppressed"
        ? current.failure
        : { status: "none" };
  const origin: "execution" | "continuation" =
    current?.status === "continuation-pending"
      ? "continuation"
      : current?.status === "waiting"
        ? current.origin
        : "execution";

  return {
    state: updateControl(state, sessionID, {
      status: "waiting",
      pending,
      origin,
      failure,
    }),
    action: "wait",
  };
}

function resolvePending(
  state: SessionControls,
  sessionID: string,
  key: string,
  resolution: "reply" | "cancel",
): ControlUpdate {
  const current = state.get(sessionID);
  if (current?.status !== "waiting") {
    return { state, action: "stop" };
  }

  const pending = new Set(current.pending);
  if (!pending.delete(key)) {
    return { state, action: "stop" };
  }

  const failure: FailureState =
    resolution === "reply"
      ? current.failure.status === "pending"
        ? { status: "pending", replied: true }
        : current.failure
      : current.failure.status === "pending" && !current.failure.replied
        ? { status: "cancelled-after-failure" }
        : current.failure.status === "none"
          ? { status: "cancelled-before-failure" }
          : current.failure;

  if (pending.size > 0) {
    return {
      state: updateControl(state, sessionID, {
        ...current,
        pending,
        failure,
      }),
      action: "wait",
    };
  }

  if (current.origin === "continuation") {
    return {
      state: updateControl(
        state,
        sessionID,
        failure.status === "none" ||
          failure.status === "cancelled-before-failure"
          ? { status: "continuation-pending" }
          : { status: "continuation-exhausted" },
      ),
      action: "stop",
    };
  }

  if (failure.status === "none") {
    return {
      state: updateControl(state, sessionID),
      action: "stop",
    };
  }

  if (failure.status === "pending" && failure.replied) {
    return {
      state: updateControl(state, sessionID, {
        status: "continuation-pending",
      }),
      action: "continue",
    };
  }

  return {
    state: updateControl(state, sessionID, {
      status: "failure-suppressed",
      failure:
        failure.status === "pending"
          ? { status: "pending", replied: false }
          : failure,
    }),
    action: "stop",
  };
}

function reduceDisabledFailure(
  state: SessionControls,
  sessionID: string,
): ControlUpdate {
  const current = state.get(sessionID);
  if (current?.status === "waiting") {
    if (current.origin === "continuation") {
      return {
        state: updateControl(state, sessionID, {
          status: "continuation-exhausted",
        }),
        action: "stop",
      };
    }
    return {
      state: updateControl(state, sessionID, {
        ...current,
        failure: { status: "disabled" },
      }),
      action: "wait",
    };
  }
  if (current?.status === "continuation-pending") {
    return {
      state: updateControl(state, sessionID, {
        status: "continuation-exhausted",
      }),
      action: "stop",
    };
  }
  if (current?.status === "failure-suppressed") {
    return {
      state: updateControl(state, sessionID, {
        status: "failure-suppressed",
        failure:
          current.failure.status === "cancelled-before-failure"
            ? { status: "cancelled-after-failure" }
            : current.failure,
      }),
      action: "stop",
    };
  }
  if (current?.status === "continuation-exhausted") {
    return { state, action: "stop" };
  }
  return {
    state: updateControl(state, sessionID, {
      status: "failure-suppressed",
      failure: { status: "disabled" },
    }),
    action: "stop",
  };
}

export function sessionIDOf(event: ObservedEvent): string {
  switch (event.type) {
    case "session.execution.started":
    case "session.execution.succeeded":
    case "session.execution.failed":
    case "session.execution.interrupted":
    case "session.deleted":
    case "permission.asked":
    case "permission.replied":
    case "form.replied":
    case "form.cancelled":
      return event.data.sessionID;
    case "form.created":
      return event.data.form.sessionID;
  }
}

export function reduceSessionControl(
  state: SessionControls,
  event: ObservedEvent,
  mode: AutoContinueMode,
): ControlUpdate {
  switch (event.type) {
    case "session.execution.started": {
      const current = state.get(event.data.sessionID);
      return {
        state:
          current?.status === "continuation-pending"
            ? state
            : updateControl(state, event.data.sessionID),
        action: "stop",
      };
    }
    case "session.execution.succeeded":
      return {
        state: updateControl(state, event.data.sessionID),
        action: "stop",
      };
    case "session.execution.failed": {
      if (mode === "disabled") {
        return reduceDisabledFailure(state, event.data.sessionID);
      }
      const current = state.get(event.data.sessionID);
      if (current?.status === "waiting") {
        if (current.pending.size === 0) {
          return {
            state: updateControl(state, event.data.sessionID, {
              status: "continuation-pending",
            }),
            action: "continue",
          };
        }
        const failure: FailureState =
          current.failure.status === "none"
            ? { status: "pending", replied: false }
            : current.failure.status === "cancelled-before-failure"
              ? { status: "cancelled-after-failure" }
              : current.failure;
        return {
          state: updateControl(state, event.data.sessionID, {
            ...current,
            failure,
          }),
          action: "wait",
        };
      }
      if (current?.status === "failure-suppressed") {
        return {
          state: updateControl(state, event.data.sessionID, {
            status: "failure-suppressed",
            failure:
              current.failure.status === "cancelled-before-failure"
                ? { status: "cancelled-after-failure" }
                : current.failure,
          }),
          action: "stop",
        };
      }
      if (current?.status === "continuation-pending") {
        return {
          state: updateControl(state, event.data.sessionID, {
            status: "continuation-exhausted",
          }),
          action: "stop",
        };
      }
      if (current?.status === "continuation-exhausted") {
        return { state, action: "stop" };
      }
      return {
        state: updateControl(state, event.data.sessionID, {
          status: "continuation-pending",
        }),
        action: "continue",
      };
    }
    case "session.execution.interrupted":
      return {
        state: updateControl(state, event.data.sessionID),
        action: "stop",
      };
    case "session.deleted":
      return {
        state: updateControl(state, event.data.sessionID),
        action: "stop",
      };
    case "permission.asked":
      return addPending(
        state,
        event.data.sessionID,
        `permission:${event.data.id}`,
      );
    case "permission.replied":
      return resolvePending(
        state,
        event.data.sessionID,
        `permission:${event.data.requestID}`,
        "reply",
      );
    case "form.created":
      return addPending(
        state,
        event.data.form.sessionID,
        `form:${event.data.form.id}`,
      );
    case "form.replied":
      return resolvePending(
        state,
        event.data.sessionID,
        `form:${event.data.id}`,
        "reply",
      );
    case "form.cancelled":
      return resolvePending(
        state,
        event.data.sessionID,
        `form:${event.data.id}`,
        "cancel",
      );
  }
}
