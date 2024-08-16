import { defineCustomElements } from "@ionic/pwa-elements/loader";
import { WebviewPrint } from "capacitor-webview-print";
import { Preferences } from "@capacitor/preferences";
import { Browser } from "@capacitor/browser";
import { Share } from "@capacitor/share";
import { Toast } from "@capacitor/toast";

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
    return await Browser.open({ url: url, windowName: "_blank" });
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

async function popupText(text) {
  return await Toast.show({ text: text });
}

defineCustomElements(window);
globalThis.printCurrentPage = printCurrentPage;
globalThis.selectStorage = selectStorage;
globalThis.insertStorage = insertStorage;
globalThis.openBrowserPage = openBrowserPage;
globalThis.shareText = shareText;
globalThis.popupText = popupText;
