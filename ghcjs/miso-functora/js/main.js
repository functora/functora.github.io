import "./jsaddle-compat";
import * as Compressor from "compressorjs";
import { defineCustomElements } from "@ionic/pwa-elements/loader";
import { Filesystem, Directory } from "@capacitor/filesystem";
import { OttRemitPlayInstallReferrer } from "capacitor-play-install-referrer-library";
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

export async function compressImage(prevImage, maxSizeKb) {
  //
  // TODO : default quality option
  //
  let opts = { quality: 0.2 };
  if (maxSizeKb) {
    opts = {
      quality: Math.min(0.2, maxSizeKb / prevImage.size),
      maxWidth: 768,
      maxHeight: 768,
    };
  }
  const nextImage = await new Promise((resolve, reject) => {
    new Compressor(prevImage, {
      mimeType: "image/jpeg",
      success: resolve,
      error: reject,
      ...opts,
    });
  });
  return nextImage;
}

export async function selectClipboard(opts = {}) {
  const { value } = await Clipboard.read();
  return await resolveDataUrl(value, opts);
}

export async function selectFile(file, opts = {}) {
  const value = await new Promise((resolve, reject) => {
    var fr = new FileReader();
    fr.onload = () => {
      resolve(fr.result);
    };
    fr.onerror = reject;
    fr.readAsDataURL(file);
  });
  return await resolveDataUrl(value, opts);
}

export async function resolveDataUrl(value, opts = {}) {
  try {
    const { buffer: u8a, typeFull: mime } = dataUriToBuffer(value);
    let blob = new Blob([u8a], { type: mime });
    if (mime.startsWith("image")) {
      let maxSizeKb = null;
      if (opts.maxSizeKb) {
        maxSizeKb = opts.maxSizeKb;
      }
      blob = await compressImage(blob, maxSizeKb);
    }
    if (opts.opfsDir && opts.opfsFile) {
      const rfc2397 = await new Promise((resolve, reject) => {
        var fr = new FileReader();
        fr.onload = () => {
          resolve(fr.result);
        };
        fr.onerror = reject;
        fr.readAsDataURL(blob);
      });
      await opfsWrite(rfc2397, opts);
    }
    return URL.createObjectURL(blob);
  } catch (e) {
    return value;
  }
}

export async function opfsWrite(value, { opfsDir, opfsFile }) {
  try {
    const root = await navigator.storage.getDirectory();
    const dir = await root.getDirectoryHandle(opfsDir, { create: true });
    const file = await dir.getFileHandle(opfsFile, { create: true });
    const stream = await file.createWritable();
    await stream.write(value);
    await stream.close();
  } catch (e) {
    console.log(
      "OPFS write failure: " +
        e.toString() +
        " dir: " +
        opfsDir +
        " file: " +
        opfsFile,
    );
  }
  return null;
}

export async function opfsRead({ opfsDir, opfsFile }) {
  try {
    const root = await navigator.storage.getDirectory();
    const dir = await root.getDirectoryHandle(opfsDir);
    const file = await dir.getFileHandle(opfsFile);
    const blob = await file.getFile();
    const uri = await blob.text();
    const res = await resolveDataUrl(uri);
    return res;
  } catch (e) {
    console.log(
      "OPFS read failure: " +
        e.toString() +
        " dir: " +
        opfsDir +
        " file: " +
        opfsFile,
    );
    return null;
  }
}

export async function opfsList(opfsDir) {
  try {
    const res = [];
    const root = await navigator.storage.getDirectory();
    const dir = await root.getDirectoryHandle(opfsDir);
    for await (let opfsFile of dir.keys()) {
      res.push(opfsFile);
    }
    return res;
  } catch (e) {
    console.log("OPFS list failure: " + e.toString() + " dir: " + opfsDir);
    return [];
  }
}

export async function opfsRemove({ opfsDir, opfsFile }) {
  try {
    const root = await navigator.storage.getDirectory();
    const dir = await root.getDirectoryHandle(opfsDir);
    const file = await dir.getFileHandle(opfsFile);
    await file.remove();
  } catch (e) {
    console.log(
      "OPFS remove failure: " +
        e.toString() +
        " dir: " +
        opfsDir +
        " file: " +
        opfsFile,
    );
  }
  return null;
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
    const b64 = await u8aToB64(u8a);
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

async function u8aToB64(u8a) {
  const b64 = await new Promise((resolve, reject) => {
    const fr = new FileReader();
    fr.onload = () => {
      resolve(fr.result);
    };
    fr.onerror = reject;
    fr.readAsDataURL(new Blob([u8a]));
  });
  return b64.slice(b64.indexOf(",") + 1);
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

export async function fetchInstallReferrerUri() {
  const { referrerUrl } =
    await OttRemitPlayInstallReferrer.getReferrerDetails();
  return referrerUrl;
}

defineCustomElements(window);
