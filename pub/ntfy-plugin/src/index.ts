import { Plugin } from "@opencode/plugin";
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { homedir } from "node:os";

interface ContentTemplate { readonly value?: string; readonly command?: string; }
interface EventRule { readonly enabled?: boolean; readonly title?: ContentTemplate; readonly message?: ContentTemplate; }
interface BackendConfig { readonly server?: string; readonly topic?: string; readonly token?: string; readonly priority?: string; readonly title?: Record<string, ContentTemplate>; readonly message?: Record<string, ContentTemplate>; }
interface Config { readonly enabled?: boolean; readonly events?: Record<string, EventRule>; readonly backend?: BackendConfig; }

const CONFIG_PATH = join(homedir(), ".config", "opencode", "notification-ntfy.json");

const EVENT_ICONS: Record<string, string> = {
  "session.execution.succeeded": "✅",
  "session.execution.failed": "❌",
  "session.execution.interrupted": "⏹️",
  "permission.asked": "❓",
  "form.created": "📝",
};

const TITLE = "{icon} {project}";

const DESCRIPTIONS: Record<string, string> = {
  "session.execution.succeeded": "Finished.",
  "session.execution.failed": "{error}",
  "session.execution.interrupted": "{reason}",
  "permission.asked": "{permission_type}",
  "form.created": "Form pending.",
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
  let out = substitute(template?.value ?? def);
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

    const iconOf = (event: string): string => EVENT_ICONS[event] ?? "";
    const contextOf = (event: string) => ({ project, icon: iconOf(event) });

    const titleOf = (event: string) => {
      const rule = events[event];
      return render(rule?.title, contextOf(event), TITLE);
    };
    const messageOf = (event: string, meta: Record<string, string>) => {
      const rule = events[event];
      return render(rule?.message, { ...contextOf(event), ...meta }, DESCRIPTIONS[event] ?? "");
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
          await send(type, {});
          continue;
        }
        if (type === "session.execution.failed") {
          if (sessionID && await isSubagent(sessionID)) continue;
          const error = record.error as { readonly message?: string } | undefined;
          await send(type, { error: error?.message ?? "" });
          continue;
        }
        if (type === "session.execution.interrupted") {
          if (sessionID && await isSubagent(sessionID)) continue;
          await send(type, { reason: typeof record.reason === "string" ? record.reason : "" });
          continue;
        }
        if (type === "permission.asked") {
          await send(type, { permission_type: typeof record.action === "string" ? record.action : "" });
          continue;
        }
        if (type === "form.created") {
          await send(type, {});
          continue;
        }
      }
    })();
    return () => controller.abort();
  },
});
