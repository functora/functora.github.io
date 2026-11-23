import { focusCanvas, registerServiceWorker } from './egui.js';
import { APP_VERSION } from './version.js';
import init from './pkg/cryptogram.js';

const telegram = globalThis.Telegram?.WebApp;
telegram?.ready();
telegram?.expand();

await init();
focusCanvas();
registerServiceWorker(APP_VERSION);
