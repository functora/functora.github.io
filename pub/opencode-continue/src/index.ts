import { Plugin } from "@opencode/plugin";
import { eventBelongsToLocation } from "./location.js";
import {
  clearSessionControl,
  isObservedEvent,
  markContinuationExhausted,
  reduceSessionControl,
  sessionIDOf,
  type SessionControls,
} from "./policy.js";

const CONTINUE_TEXT = "continue";

export default Plugin.define({
  id: "opencode-continue",
  setup(ctx) {
    const controller = new AbortController();
    let controls: SessionControls = new Map();

    void (async () => {
      try {
        for await (const event of ctx.event.subscribe({
          signal: controller.signal,
        })) {
          const eventLocation = event.location;
          if (!isObservedEvent(event)) continue;
          const sessionID = sessionIDOf(event);
          const session = await ctx.session
            .get({ sessionID })
            .catch((error) => {
              console.error(
                `opencode-continue: could not locate session ${sessionID}:`,
                error,
              );
              return undefined;
            });
          if (!session) continue;

          if (
            !eventBelongsToLocation(
              ctx.location,
              eventLocation,
              session.location,
            )
          ) {
            controls = clearSessionControl(controls, sessionID);
            continue;
          }

          const update = reduceSessionControl(controls, event);
          controls = update.state;
          if (update.action !== "continue") continue;
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

    return () => controller.abort();
  },
});
