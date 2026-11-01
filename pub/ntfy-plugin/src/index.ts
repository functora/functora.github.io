import { Plugin } from "@opencode/plugin";
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { homedir } from "node:os";

interface ContentTemplate { readonly value?: string; readonly command?: string; }
interface EventRule { readonly enabled?: boolean; readonly title?: ContentTemplate; readonly message?: ContentTemplate; }
interface BackendConfig { readonly server?: string; readonly topic?: string; readonly token?: string; readonly priority?: string; readonly title?: Record<string, ContentTemplate>; readonly message?: Record<string, ContentTemplate>; }
interface Config { readonly enabled?: boolean; readonly events?: Record<string, EventRule>; readonly backend?: BackendConfig; }

const CONFIG_PATH = join(homedir(), ".config", "opencode", "notification-ntfy.json");

const DEFAULT_TITLES: Record<string, string> = {
  "session.execution.succeeded": "{project}: agent finished",
  "session.execution.failed": "{project}: agent error",
  "session.execution.interrupted": "{project}: agent interrupted",
  "permission.asked": "{project}: input required",
  "form.created": "{project}: form awaiting answers",
};
const DEFAULT_MESSAGES: Record<string, string> = {
  "session.execution.succeeded": "The agent has finished and is waiting for input.",
  "session.execution.failed": "An error occurred: {error}",
  "session.execution.interrupted": "The agent was interrupted: {reason}",
  "permission.asked": "The agent needs your input: {permission_type}",
  "form.created": "A form is awaiting your answers.",
};

function loadConfig(): Config {
  try {
    return JSON.parse(readFileSync(CONFIG_PATH, "utf-8")) as Config;
  } catch {
    return {};
  }
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

function render(template: ContentTemplate | undefined, context: Record<string, string>, def: string): string {
  const raw = template?.value ?? def;
  let out = substitute(raw).replace(/{project}/g, context.project);
  for (const [key, val] of Object.entries(context)) {
    out = out.replace(new RegExp(`{${key}}`, "g"), val ?? "");
  }
  return out;
}

const NOTIFICATION_EVENTS = [
  "session.execution.succeeded",
  "session.execution.failed",
  "session.execution.interrupted",
  "permission.asked",
  "form.created",
] as const;

export default Plugin.define({
  id: "opencode-notification-ntfy",
  setup(ctx) {
    const config = loadConfig();
    const backend = config.backend ?? {};
    const server = substitute(backend.server ?? "https://ntfy.sh");
    const topic = substitute(backend.topic ?? "");
    const url = `${server}/${topic}`;
    const token = typeof backend.token === "string" ? backend.token : undefined;
    const priority = typeof backend.priority === "string" ? backend.priority : "default";
    const enabled = config.enabled !== false;
    const events = config.events ?? {};
    const enabledSet = new Set(
      NOTIFICATION_EVENTS.filter((e) => events[e]?.enabled !== false),
    );
    const project = ctx.location.directory.split("/").pop() ?? "agent";
    const controller = new AbortController();

    const titleOf = (event: string) => {
      const rule = events[event];
      return render(rule?.title, { project }, DEFAULT_TITLES[event] ?? "");
    };
    const messageOf = (event: string, meta: Record<string, string>) => {
      const rule = events[event];
      return render(rule?.message, { project, ...meta }, DEFAULT_MESSAGES[event] ?? "");
    };

    const send = async (event: string, meta: Record<string, string>): Promise<void> => {
      if (!enabled || !enabledSet.has(event as typeof NOTIFICATION_EVENTS[number]) || !topic) return;
      const title = titleOf(event);
      const message = messageOf(event, meta);
      const headers: Record<string, string> = { Title: title, Priority: priority };
      if (token) headers["Authorization"] = `Bearer ${token}`;
      try {
        const response = await fetch(url, {
          method: "POST",
          headers,
          body: message,
          signal: controller.signal,
        });
        void response;
      } catch {
        // Notifications never crash the host.
      }
    };

    const isSubagent = async (sessionID: string): Promise<boolean> => {
      try {
        const session = await ctx.session.get({ sessionID });
        return Boolean(session?.parentID);
      } catch {
        return false;
      }
    };

    void (async () => {
      for await (const ev of ctx.event.subscribe({ signal: controller.signal })) {
        const type = ev.type;
        const record = (ev as { readonly data?: Record<string, unknown> }).data ?? {};
        const sessionID = typeof record.sessionID === "string" ? record.sessionID : "";

        if (type === "session.execution.succeeded") {
          await send(type, { project });
          continue;
        }
        if (type === "session.execution.failed") {
          if (sessionID && await isSubagent(sessionID)) continue;
          const error = record.error as { readonly message?: string } | undefined;
          await send(type, { project, error: error?.message ?? "" });
          continue;
        }
        if (type === "session.execution.interrupted") {
          if (sessionID && await isSubagent(sessionID)) continue;
          await send(type, { project, reason: typeof record.reason === "string" ? record.reason : "" });
          continue;
        }
        if (type === "permission.asked") {
          await send(type, { project, permission_type: typeof record.action === "string" ? record.action : "" });
          continue;
        }
        if (type === "form.created") {
          await send(type, { project });
          continue;
        }
      }
    })();
    return () => controller.abort();
  },
});
