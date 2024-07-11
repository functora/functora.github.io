import { WebviewPrint } from "capacitor-webview-print";

async function printCurrentPage(name) {
  await WebviewPrint.print({ name: name });
}

globalThis.printCurrentPage = printCurrentPage;
