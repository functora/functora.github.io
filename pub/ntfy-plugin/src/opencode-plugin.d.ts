declare module "@opencode/plugin" {
  export interface PluginDefineOptions {
    readonly id: string;
    readonly setup: (ctx: PluginContext) => void | (() => void);
  }
  export interface Plugin {
    readonly define: (options: PluginDefineOptions) => unknown;
  }
  export const Plugin: Plugin;
}

interface LocationRef {
  readonly directory: string;
  readonly workspaceID?: string;
}

interface SessionInfo {
  readonly parentID?: string;
  readonly location?: LocationRef;
}

interface PluginContext {
  readonly location: LocationRef;
  readonly session: {
    readonly get: (opts: { readonly sessionID: string }) => Promise<SessionInfo | null>;
  };
  readonly event: {
    readonly subscribe: (opts?: {
      readonly signal?: AbortSignal;
      readonly types?: readonly string[];
    }) => AsyncIterable<{
      readonly type: string;
      readonly data?: unknown;
      readonly location?: LocationRef;
    }>;
  };
}
