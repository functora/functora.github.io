declare module "@opencode/plugin" {
  type Cleanup = () => void | Promise<void>;

  export interface PluginDefineOptions {
    readonly id: string;
    readonly setup: (
      ctx: PluginContext,
    ) => void | Cleanup | Promise<void | Cleanup>;
  }

  export interface Plugin {
    readonly define: (options: PluginDefineOptions) => unknown;
  }

  export const Plugin: Plugin;
}

interface LocationInfo {
  readonly directory: string;
  readonly workspaceID?: string;
  readonly project: {
    readonly id: string;
    readonly directory: string;
    readonly canonical: string;
  };
}

interface LocationPublicRef {
  readonly directory: string;
}

interface PluginEvent {
  readonly type: string;
  readonly data?: unknown;
  readonly location?: LocationPublicRef;
}

interface PluginContext {
  readonly location: LocationInfo;
  readonly session: {
    readonly get: (opts: {
      readonly sessionID: string;
    }) => Promise<{ readonly location: LocationPublicRef }>;
    readonly synthetic: (opts: {
      readonly sessionID: string;
      readonly text: string;
      readonly metadata?: Record<string, unknown>;
    }) => Promise<unknown>;
  };
  readonly event: {
    readonly subscribe: (opts?: {
      readonly signal?: AbortSignal;
    }) => AsyncIterable<PluginEvent>;
  };
}
