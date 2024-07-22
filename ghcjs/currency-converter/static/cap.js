import { WebviewPrint } from "capacitor-webview-print";
import { Preferences } from "@capacitor/preferences";

async function printCurrentPage(name) {
  await WebviewPrint.print({ name: name });
}

async function selectStorage(key) {
  const { value } = await Preferences.get({ key: key });
  return value;
}

async function insertStorage(key, value) {
  await Preferences.set({ key: key, value: value });
}

globalThis.printCurrentPage = printCurrentPage;
globalThis.selectStorage = selectStorage;
globalThis.insertStorage = insertStorage;
