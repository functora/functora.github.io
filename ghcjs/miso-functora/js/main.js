import "./jsaddle-compat";
import * as Compressor from "compressorjs";
import { defineCustomElements } from "@ionic/pwa-elements/loader";
import { Filesystem, Directory } from "@capacitor/filesystem";
import { WebviewPrint } from "capacitor-webview-print";
import { dataUriToBuffer } from "data-uri-to-buffer";
import { Preferences } from "@capacitor/preferences";
import { Clipboard } from "@capacitor/clipboard";
import { Browser } from "@capacitor/browser";
import { Share } from "@capacitor/share";
import { Toast } from "@capacitor/toast";
import { Capacitor } from "@capacitor/core";
import { Html5Qrcode } from "html5-qrcode";
import { saveAs } from "file-saver";
import {
  CapacitorBarcodeScanner,
  CapacitorBarcodeScannerTypeHint,
} from "@capacitor/barcode-scanner";

export async function printCurrentPage(name) {
  return await WebviewPrint.print({ name: name });
}

export async function selectStorage(key) {
  const { value } = await Preferences.get({ key: key });
  return value;
}

export async function insertStorage(key, value) {
  return await Preferences.set({ key: key, value: value });
}

export async function selectClipboard() {
  const { value } = await Clipboard.read();
  try {
    const { buffer: u8a, typeFull: mime } = dataUriToBuffer(value);
    let blob = new Blob([u8a], { type: mime });
    if (mime.startsWith("image")) {
      blob = await new Promise((resolve, reject) => {
        new Compressor(blob, {
          quality: 0.1,
          mimeType: "image/jpeg",
          success: resolve,
          error: reject,
        });
      });
    }
    return URL.createObjectURL(blob);
  } catch (e) {
    return value;
  }
}

export async function openBrowserPage(url) {
  try {
    return await Browser.open({ url: url, windowName: "_blank" });
  } catch (e) {
    return window.open(url, "_blank").focus();
  }
}

export async function shareText(text) {
  const { value } = await Share.canShare();
  if (value) {
    return await Share.share({ text: text });
  } else {
    return await navigator.clipboard.writeText(text);
  }
}

export async function popupText(text) {
  return await Toast.show({ text: text });
}

export async function selectBarcode() {
  if (!Capacitor.isNativePlatform()) {
    const devices = await Html5Qrcode.getCameras();
    if (!(devices && devices.length)) {
      throw new Error("Camera not found!");
    }
  }
  const { ScanResult } = await CapacitorBarcodeScanner.scanBarcode({
    hint: CapacitorBarcodeScannerTypeHint.ALL,
  });
  return ScanResult;
}

export async function saveFile(name, mime, ab) {
  const u8a = new Uint8Array(ab);
  if (Capacitor.isNativePlatform()) {
    const b64 = btoa(String.fromCharCode.apply(null, u8a));
    const { uri } = await Filesystem.writeFile({
      path: name,
      data: b64,
      directory: Directory.Documents,
    });
    return uri;
  } else {
    const blob = new Blob([u8a], { type: mime });
    await saveAs(blob, name);
    return null;
  }
}

export async function shareFiles(files) {
  if (Capacitor.isNativePlatform()) {
    const { value } = await Share.share({ files: files });
    return value;
  } else {
    return null;
  }
}

export function isNativePlatform() {
  return Capacitor.isNativePlatform();
}

export async function fetchUrlAsRfc2397(url) {
  const imgResp = await fetch(url);
  const imgBlob = await imgResp.blob();
  const rfc2397 = await new Promise((resolve, reject) => {
    var fr = new FileReader();
    fr.onload = () => {
      resolve(fr.result);
    };
    fr.onerror = reject;
    fr.readAsDataURL(imgBlob);
  });
  const utf8Encode = new TextEncoder();
  const ab = utf8Encode.encode(rfc2397).buffer;
  return ab;
}

defineCustomElements(window);
