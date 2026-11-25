import { Plugin } from "@opencode/plugin";
import { setupContinuePlugin } from "./plugin.js";

export default Plugin.define({
  id: "opencode-continue",
  setup: setupContinuePlugin,
});
