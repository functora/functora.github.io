import { Plugin } from "@opencode/plugin";
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { homedir } from "node:os";

interface ContentTemplate {
  readonly value?: string;
  readonly command?: string;
}
interface EventRule {
  readonly enabled?: boolean;
  readonly title?: ContentTemplate;
  readonly message?: ContentTemplate;
}
interface BackendConfig {
  readonly server?: string;
  readonly topic?: string;
  readonly token?: string;
  readonly priority?: string;
  readonly deeplink?: string;
  readonly title?: Record<string, ContentTemplate>;
  readonly message?: Record<string, ContentTemplate>;
}
interface Config {
  readonly enabled?: boolean;
  readonly events?: Record<string, EventRule>;
  readonly backend?: BackendConfig;
}

const CONFIG_PATHS = [
  join(homedir(), ".config", "opencode", "notification-ntfy.json"),
  join(process.env.HOME ?? "", ".config", "opencode", "notification-ntfy.json"),
];

const EVENT_ICONS: Record<string, string> = {
  "session.execution.succeeded": "✅",
  "session.execution.failed": "❌",
  "session.execution.interrupted": "🛑",
  "permission.asked": "❓",
  "form.created": "📝",
  "session.idle": "⏳",
};

const EVENT_TAGS: Record<string, string> = {
  "session.execution.succeeded": "white_check_mark",
  "session.execution.failed": "x",
  "session.execution.interrupted": "stop_sign",
  "permission.asked": "question",
  "form.created": "memo",
  "session.idle": "hourglass_flowing_sand",
};

const TITLE = "{project}";

const DESCRIPTIONS: Record<string, string> = {
  "session.execution.succeeded": "Finished.",
  "session.execution.failed": "{error}",
  "session.execution.interrupted": "{reason}",
  "permission.asked": "{permission_type}",
  "form.created": "Form pending.",
  "session.idle": "The agent has finished and is waiting for input.",
};

const NOTIFICATION_EVENTS = [
  "session.execution.succeeded",
  "session.execution.failed",
  "session.execution.interrupted",
  "permission.asked",
  "form.created",
  "session.idle",
] as const;

function loadConfig(): Config {
  let lastError: unknown;
  for (const path of CONFIG_PATHS) {
    if (!path) continue;
    try {
      return JSON.parse(readFileSync(path, "utf-8")) as Config;
    } catch (error) {
      lastError = error;
    }
  }
  console.error(
    `opencode-notification-ntfy: could not load config (tried ${CONFIG_PATHS.join(", ")}):`,
    lastError,
  );
  return {};
}

function substitute(value: string): string {
  return value
    .replace(/\{env:([^}]+)\}/g, (_m, name) => process.env[name] ?? "")
    .replace(/\{file:([^}]+)\}/g, (_m, filePath) => {
      const resolved = filePath.startsWith("/")
        ? filePath
        : filePath.startsWith("~")
          ? join(homedir(), filePath.slice(1))
          : join(homedir(), ".config", "opencode", filePath);
      try {
        return readFileSync(resolved, "utf-8").trim();
      } catch {
        return "";
      }
    });
}

function render(
  template: ContentTemplate | undefined,
  context: Record<string, string>,
  def: string,
): string {
  let out = substitute(template?.value ?? def);
  for (const [key, val] of Object.entries(context)) {
    out = out.replace(new RegExp(`{${key}}`, "g"), val ?? "");
  }
  return out;
}

function isRecord(value: unknown): value is Record<string, unknown> {
  return typeof value === "object" && value !== null && !Array.isArray(value);
}

export default Plugin.define({
  id: "opencode-notification-ntfy",
  setup(ctx) {
    const config = loadConfig();
    const backend = config.backend ?? {};
    const server = substitute(backend.server ?? "https://ntfy.sh");
    const topic = substitute(backend.topic ?? "");
    const url = `${server}/${topic}`;
    const token = typeof backend.token === "string" ? backend.token : undefined;
    const priority =
      typeof backend.priority === "string" ? backend.priority : "default";
    const deeplink =
      typeof backend.deeplink === "string"
        ? backend.deeplink
        : "opencodemobile://session/{sessionID}?project={projectPath}";
    const enabled = config.enabled !== false;
    const events = config.events ?? {};
    const enabledSet = new Set(
      NOTIFICATION_EVENTS.filter((e) => events[e]?.enabled !== false),
    );
    const project = ctx.location.directory.split("/").pop() ?? "agent";
    const projectPath = encodeURIComponent(ctx.location.directory);
    const sameLocation = (location?: LocationRef): boolean =>
      location !== undefined &&
      location.directory === ctx.location.directory &&
      location.workspaceID === ctx.location.workspaceID;
    const sessionGet = async (
      sessionID: string,
    ): Promise<SessionInfo | undefined> => {
      try {
        return (await ctx.session.get({ sessionID })) ?? undefined;
      } catch {
        return undefined;
      }
    };
    const sessionIDOf = (data: Record<string, unknown>): string =>
      typeof data.sessionID === "string"
        ? data.sessionID
        : isRecord(data.form) && typeof data.form.sessionID === "string"
          ? data.form.sessionID
          : "";
    const sessionNameOf = (
      data: Record<string, unknown>,
      session: SessionInfo | undefined,
    ): string =>
      typeof data.sessionName === "string"
        ? data.sessionName
        : typeof data.session_name === "string"
          ? data.session_name
          : typeof data.name === "string"
            ? data.name
            : typeof data.title === "string"
              ? data.title
              : typeof session?.name === "string"
                ? session.name
                : typeof session?.title === "string"
                  ? session.title
                  : "";
    const controller = new AbortController();

    const iconOf = (event: string): string => EVENT_ICONS[event] ?? "";
    const contextOf = (event: string) => ({ project, icon: iconOf(event) });

    const titleOf = (event: string, sessionName?: string) => {
      const rule = events[event];
      if (sessionName) return sessionName;
      return render(rule?.title, contextOf(event), TITLE);
    };
    const messageOf = (
      event: string,
      meta: Record<string, string>,
    ): string => {
      const rule = events[event];
      const sessionName = meta.sessionName ?? "";
      const sessionID = meta.sessionID ?? "";
      const description = render(
        { value: DESCRIPTIONS[event] ?? "" },
        { ...contextOf(event), ...meta },
        "",
      );
      const deeplinkURL = deeplink
        .replace("{sessionID}", sessionID)
        .replace("{projectPath}", projectPath);
      const deeplinkText = sessionID && deeplink
        ? `[${sessionName || sessionID}](${deeplinkURL})`
        : "";
      const defaultBody = [deeplinkText, description]
        .filter(Boolean)
        .join(" ");
      return render(
        rule?.message,
        {
          ...contextOf(event),
          ...meta,
          description,
        },
        defaultBody,
      );
    };

    const send = async (
      event: string,
      meta: Record<string, string>,
    ): Promise<void> => {
      if (
        !enabled ||
        !enabledSet.has(event as (typeof NOTIFICATION_EVENTS)[number]) ||
        !topic
      ) {
        console.error(
          `opencode-notification-ntfy: skipping ${event} enabled=${enabled} topic=${Boolean(topic)}`,
        );
        return;
      }
      const title = titleOf(event, meta.sessionName);
      const message = messageOf(event, meta);
      const headers: Record<string, string> = {
        Priority: priority,
        Markdown: "yes",
        Tags: EVENT_TAGS[event] ?? "",
      };
      if (title) headers["Title"] = title;
      if (token) headers["Authorization"] = `Bearer ${token}`;
      try {
        const response = await fetch(url, {
          method: "POST",
          headers,
          body: message,
          signal: controller.signal,
        });
        if (!response.ok) {
          console.error(
            `opencode-notification-ntfy: ntfy ${event} HTTP ${response.status}`,
          );
        }
        void response;
      } catch (error) {
        console.error(
          `opencode-notification-ntfy: ntfy ${event} fetch failed:`,
          error,
        );
      }
    };

    void (async () => {
      try {
        for await (const ev of ctx.event.subscribe({
          signal: controller.signal,
          types: [...NOTIFICATION_EVENTS, "session.status"],
        })) {
          const type = ev.type;
          const data =
            (ev as { readonly data?: Record<string, unknown> }).data ?? {};
          const sessionID = sessionIDOf(data);
          const session = sessionID ? await sessionGet(sessionID) : undefined;
          const sessionName = sessionNameOf(data, session);
          const owned = ev.location?.directory
            ? sameLocation(ev.location)
            : !session?.location || sameLocation(session.location);
          if (!owned) continue;

          if (type === "session.status") {
            if (!isRecord(data) || !isRecord(data.status)) continue;
            if (data.status.type !== "idle") continue;
            const notificationEvent = "session.idle";
            if (!enabledSet.has(notificationEvent)) continue;
            if (session?.parentID) continue;
            await send(notificationEvent, { sessionName, sessionID });
            continue;
          }
          if (type === "session.execution.succeeded") {
            await send(type, { sessionName, sessionID });
            continue;
          }
          if (type === "session.execution.failed") {
            if (session?.parentID) continue;
            const error = isRecord(data) ? data.error : undefined;
            const errorMessage =
              isRecord(error) && typeof error.message === "string"
                ? error.message
                : "";
            await send(type, { error: errorMessage, sessionName, sessionID });
            continue;
          }
          if (type === "session.execution.interrupted") {
            if (session?.parentID) continue;
            await send(type, {
              reason: typeof data.reason === "string" ? data.reason : "",
              sessionName,
              sessionID,
            });
            continue;
          }
          if (type === "permission.asked") {
            await send(type, {
              permission_type:
                typeof data.action === "string" ? data.action : "",
              sessionName,
              sessionID,
            });
            continue;
          }
          if (type === "form.created") {
            await send(type, { sessionName, sessionID });
            continue;
          }
        }
      } catch (error) {
        console.error(
          "opencode-notification-ntfy: event stream failed:",
          error,
        );
      }
    })();
    return () => controller.abort();
  },
});
