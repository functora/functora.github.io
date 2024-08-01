import { WebviewPrint } from "capacitor-webview-print";
import { Preferences } from "@capacitor/preferences";
import { Browser } from "@capacitor/browser";
import { Share } from "@capacitor/share";

async function printCurrentPage(name) {
  return await WebviewPrint.print({ name: name });
}

async function selectStorage(key) {
  const { value } = await Preferences.get({ key: key });
  return value;
}

async function insertStorage(key, value) {
  return await Preferences.set({ key: key, value: value });
}

async function openBrowserPage(url) {
  try {
    return await Browser.open({ url: url });
  } catch (e) {
    return window.open(url, "_blank").focus();
  }
}

async function shareText(text) {
  const { value } = await Share.canShare();
  if (value) {
    return await Share.share({ text: text });
  } else {
    return await navigator.clipboard.writeText(text);
  }
}

globalThis.printCurrentPage = printCurrentPage;
globalThis.selectStorage = selectStorage;
globalThis.insertStorage = insertStorage;
globalThis.openBrowserPage = openBrowserPage;
globalThis.shareText = shareText;
