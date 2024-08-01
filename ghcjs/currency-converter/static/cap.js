import { WebviewPrint } from "capacitor-webview-print";
import { Preferences } from "@capacitor/preferences";
import { Browser } from "@capacitor/browser";

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

async function openBrowserPage(url) {
  try {
    await Browser.open({ url: url });
  } catch (e) {
    window.open(url, "_blank").focus();
  }
}

globalThis.printCurrentPage = printCurrentPage;
globalThis.selectStorage = selectStorage;
globalThis.insertStorage = insertStorage;
globalThis.openBrowserPage = openBrowserPage;
