export type AutoContinueMode = "disabled" | "enabled";

export type AutoContinueModes = ReadonlyMap<string, AutoContinueMode>;

export function autoContinueModeOf(
  modes: AutoContinueModes,
  sessionID: string,
): AutoContinueMode {
  return modes.get(sessionID) ?? "disabled";
}

export function setAutoContinueMode(
  modes: AutoContinueModes,
  sessionID: string,
  mode: AutoContinueMode,
): AutoContinueModes {
  const next = new Map(modes);
  if (mode === "disabled") {
    next.delete(sessionID);
  } else {
    next.set(sessionID, mode);
  }
  return next;
}

export function parseAutoContinueMode(input: string): AutoContinueMode {
  switch (input.trim().toLowerCase()) {
    case "on":
      return "enabled";
    case "off":
      return "disabled";
    default:
      throw new Error('auto-continue expects "on" or "off"');
  }
}
