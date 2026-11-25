import type { PluginContext } from "@opencode/plugin";
import { eventBelongsToLocation } from "./location.js";
import {
  clearSessionControl,
  isObservedEvent,
  markContinuationExhausted,
  reduceSessionControl,
  sessionIDOf,
  suppressSessionContinuation,
  type SessionControls,
} from "./policy.js";
import {
  autoContinueModeOf,
  parseAutoContinueMode,
  setAutoContinueMode,
  type AutoContinueModes,
} from "./runtime.js";

const CONTINUE_TEXT = "continue";

export async function setupContinuePlugin(ctx: PluginContext) {
  const controller = new AbortController();
  let controls: SessionControls = new Map();
  let modes: AutoContinueModes = new Map();

  await ctx.command.transform((editor) => {
    editor.add({
      name: "auto-continue",
      description: "Enable or disable automatic continuation for this session",
      execute: async ({ sessionID, prompt }) => {
        const mode = parseAutoContinueMode(prompt.text);
        modes = setAutoContinueMode(modes, sessionID, mode);
        if (mode === "disabled") {
          controls = suppressSessionContinuation(controls, sessionID);
        }
      },
    });
  });

  const eventTask = (async () => {
    try {
      for await (const event of ctx.event.subscribe({
        signal: controller.signal,
      })) {
        if (controller.signal.aborted) return;
        const eventLocation = event.location;
        if (!isObservedEvent(event)) continue;
        const sessionID = sessionIDOf(event);
        if (event.type === "session.deleted") {
          controls = clearSessionControl(controls, sessionID);
          modes = setAutoContinueMode(modes, sessionID, "disabled");
          continue;
        }
        const session = await ctx.session.get({ sessionID }).catch((error) => {
          console.error(
            `opencode-continue: could not locate session ${sessionID}:`,
            error,
          );
          return undefined;
        });
        if (!session || controller.signal.aborted) continue;

        if (
          !eventBelongsToLocation(ctx.location, eventLocation, session.location)
        ) {
          controls = clearSessionControl(controls, sessionID);
          continue;
        }

        const update = reduceSessionControl(
          controls,
          event,
          autoContinueModeOf(modes, sessionID),
        );
        controls = update.state;
        if (update.action !== "continue" || controller.signal.aborted) continue;
        try {
          await ctx.session.synthetic({
            sessionID,
            text: CONTINUE_TEXT,
            metadata: { source: "opencode-continue" },
          });
        } catch (error) {
          controls = markContinuationExhausted(controls, sessionID);
          console.error(
            `opencode-continue: could not continue session ${sessionID}:`,
            error,
          );
        }
      }
    } catch (error) {
      if (!controller.signal.aborted) {
        console.error("opencode-continue: event stream failed:", error);
      }
    }
  })();

  return async () => {
    controller.abort();
    await eventTask;
  };
}
