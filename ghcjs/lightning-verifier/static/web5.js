import { W as WebPlugin } from "./main.js";
class ToastWeb extends WebPlugin {
  async show(options) {
    if (typeof document !== "undefined") {
      let duration = 2e3;
      if (options.duration) {
        duration = options.duration === "long" ? 3500 : 2e3;
      }
      const toast = document.createElement("pwa-toast");
      toast.duration = duration;
      toast.message = options.text;
      document.body.appendChild(toast);
    }
  }
}
export {
  ToastWeb
};
