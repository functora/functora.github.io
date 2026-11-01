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

interface PluginContext {
  readonly location: { readonly directory: string };
  readonly session: {
    readonly get: (opts: { readonly sessionID: string }) => Promise<{ readonly parentID?: string } | null>;
  };
  readonly event: {
    readonly subscribe: (opts?: {
      readonly signal?: AbortSignal;
      readonly types?: readonly string[];
    }) => AsyncIterable<{
      readonly type: string;
      readonly data?: unknown;
    }>;
  };
}
