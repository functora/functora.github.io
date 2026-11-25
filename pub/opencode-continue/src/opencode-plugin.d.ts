declare module "@opencode/plugin" {
  type Cleanup = () => void | Promise<void>;

  export interface LocationInfo {
    readonly directory: string;
    readonly workspaceID?: string;
    readonly project: {
      readonly id: string;
      readonly directory: string;
      readonly canonical: string;
    };
  }

  export interface LocationPublicRef {
    readonly directory: string;
  }

  export interface PluginCommandInvocation {
    readonly sessionID: string;
    readonly prompt: { readonly text: string };
    readonly delivery: "steer" | "queue";
  }

  export interface PluginCommandDefinition {
    readonly name: string;
    readonly description?: string;
    readonly execute: (input: PluginCommandInvocation) => Promise<void>;
  }

  export interface PluginEvent {
    readonly type: string;
    readonly data?: unknown;
    readonly location?: LocationPublicRef;
  }

  export interface PluginContext {
    readonly location: LocationInfo;
    readonly command: {
      readonly transform: (
        callback: (editor: {
          readonly add: (definition: PluginCommandDefinition) => void;
        }) => void,
      ) => Promise<{ readonly dispose: () => Promise<void> }>;
    };
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
