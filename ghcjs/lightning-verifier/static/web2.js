import { W as WebPlugin } from "./main.js";
class WebviewPrintWeb extends WebPlugin {
  async print(options) {
    const documentTitleTemp = document.title;
    document.title = options.name;
    window.onafterprint = () => document.title = documentTitleTemp;
    return window.print();
  }
}
export {
  WebviewPrintWeb
};
