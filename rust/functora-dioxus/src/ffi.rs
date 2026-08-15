use crate::error::Error;
use crate::i18n::I18N;
use base64::Engine;
use dioxus::document::Eval;
use dioxus::prelude::*;
use either::Either;
use serde::{Deserialize, Serialize};
use std::cell::{Cell, RefCell};
use std::sync::atomic::{AtomicU64, Ordering};
use tap::prelude::*;

#[derive(Copy, Debug, Clone, PartialEq, Eq, Serialize, Deserialize, derive_more::Display)]
pub enum Theme {
    #[display("Light")]
    Light,
    #[display("Dark")]
    Dark,
}

impl Theme {
    #[must_use]
    pub fn next(self) -> Self {
        match self {
            Theme::Light => Theme::Dark,
            Theme::Dark => Theme::Light,
        }
    }

    #[must_use]
    pub fn to_js_value(&self) -> String {
        self.to_string().to_lowercase()
    }
}

pub async fn set_theme(theme: &Theme) -> Result<(), Error> {
    eval(
        theme.to_js_value(),
        r#"function(arg){
        const el = window.document.documentElement;
        if (arg === "dark") {
            el.removeAttribute("functora-theme-light");
            el.setAttribute("functora-theme-dark", "");
        } else {
            el.removeAttribute("functora-theme-dark");
            el.setAttribute("functora-theme-light", "");
        }
        return null;
        }"#,
    )
    .await
}
#[cfg(target_os = "android")]
pub use crate::android::{clipboard_write, print_page, read_clipboard, social_share};

#[cfg(not(target_os = "android"))]
pub use crate::web::{clipboard_write, print_page, read_clipboard, social_share};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FrameData {
    pub data: Vec<u8>,
    pub width: u32,
    pub height: u32,
}

#[derive(Deserialize)]
struct WireFrame {
    data: String,
    width: u32,
    height: u32,
}

pub async fn check_camera() -> Result<(), Error> {
    eval(
        (),
        r#"function(arg){
        if (!navigator.mediaDevices || !navigator.mediaDevices.getUserMedia) {
        throw new Error("Camera API not available");
        }
        return null;
        }"#,
    )
    .await
}

pub async fn start_camera() -> Result<(), Error> {
    eval(
        (),
        r#"function(arg){
        window.__qrScanActive = true;
        if (!window.__qrWatchdog) {
        window.__qrWatchdog = true;
        setInterval(function () {
        if (!window.__qrScanActive) return;
        var pending = (dioxus.js_to_rust && dioxus.js_to_rust.pending) ? dioxus.js_to_rust.pending.length : 0;
        if (pending > window.__qrLastPending) {
        window.__qrStall = (window.__qrStall || 0) + 1;
        if (window.__qrStall >= 3) {
        console.error("[cap] WATCHDOG reload pending=" + pending);
        window.location.reload();
        }
        } else {
        window.__qrLastPending = pending;
        window.__qrStall = 0;
        }
        }, 2000);
        }
        const stream = await navigator.mediaDevices.getUserMedia({
        video: {
        facingMode: "environment"
        }
        });
        const video = document.getElementById("qr-video");
        if (video) {
        video.srcObject = stream;
        }
        return null;
        }"#,
    )
    .await
}

const CAPTURE_WORKER_JS: &str = r#"
try {
const video = document.getElementById("qr-video");
const canvas = document.getElementById("qr-canvas");
if (!video || !canvas) throw new Error("Video or canvas not found");
while (window.__qrScanActive && video.isConnected && video.srcObject && !video.videoWidth) {
await new Promise(r => setTimeout(r, 100));
}
const ctx = canvas.getContext("2d", { willReadFrequently: true });
const MAX = 360;
while (window.__qrScanActive && video.isConnected && video.srcObject) {
let w = video.videoWidth;
let h = video.videoHeight;
if (!w || !h) {
await new Promise(r => setTimeout(r, 100));
continue;
}
const k = Math.min(1, MAX / Math.max(w, h));
w = Math.round(w * k);
h = Math.round(h * k);
canvas.width = w;
canvas.height = h;
try {
ctx.drawImage(video, 0, 0);
const imageData = ctx.getImageData(0, 0, w, h);
const rgba = imageData.data;
const luma = new Uint8Array(w * h);
for (let i = 0, j = 0; i < rgba.length; i += 4, j += 1) {
luma[j] = rgba[i] * 0.299 + rgba[i + 1] * 0.587 + rgba[i + 2] * 0.114;
}
let bin = "";
for (let i = 0; i < luma.length; i += 0x8000) {
bin += String.fromCharCode.apply(null, luma.subarray(i, i + 0x8000));
}
dioxus.send({Right: {data: btoa(bin), width: w, height: h}});
if (dioxus.js_to_rust && dioxus.js_to_rust.pending && dioxus.js_to_rust.pending.length >= 5) {
console.error("[cap] WATCHDOG reload pending=" + dioxus.js_to_rust.pending.length);
window.location.reload();
}
} catch (e) {
console.error("[cap] capture error " + String(e));
}
await new Promise(r => setTimeout(r, 300));
}
} catch (e) {
dioxus.send({Left: String(e)});
}
return null;
"#;

const STOP_WORKER_JS: &str = r"
window.__qrScanActive = false;
return null;
";

thread_local! {
    static CAPTURE_WORKER: RefCell<Option<(u64, Eval)>> = const { RefCell::new(None) };
    static CAPTURE_ARMED: Cell<bool> = const { Cell::new(false) };
}

static CAPTURE_SESSION: AtomicU64 = AtomicU64::new(0);

const SCAN_FRAME_TIMEOUT_MS: u64 = 5_000;

/// Starts a fresh capture session. Each scanner instance calls this once when it
/// owns the camera, so a dying instance's worker can never serve a replacement.
pub fn begin_capture_session() {
    let session = CAPTURE_SESSION.fetch_add(1, Ordering::Relaxed).saturating_add(1);
    CAPTURE_WORKER.with(|slot| {
        let mut stored = slot.borrow_mut();
        if let Some((current, _)) = stored.as_ref()
            && *current != session
        {
            *stored = None;
        }
    });
    CAPTURE_ARMED.with(|armed| armed.set(false));
}

/// Stops the capture worker unconditionally, even if the camera teardown eval failed.
pub fn stop_capture_worker() {
    let _ = dioxus::document::eval(STOP_WORKER_JS);
    CAPTURE_WORKER.with(|slot| *slot.borrow_mut() = None);
    CAPTURE_ARMED.with(|armed| armed.set(false));
}

fn create_capture_worker() -> Eval {
    dioxus::document::eval(CAPTURE_WORKER_JS)
}

pub async fn capture_frame() -> Result<FrameData, Error> {
    let session = CAPTURE_SESSION.load(Ordering::Relaxed);
    let existing = CAPTURE_WORKER.with(|slot| {
        slot.borrow()
            .as_ref()
            .and_then(|(stored, eval)| (*stored == session).then_some(*eval))
    });
    let mut worker = if let Some(worker) = existing {
        worker
    } else {
        let created = create_capture_worker();
        CAPTURE_WORKER.with(|slot| *slot.borrow_mut() = Some((session, created)));
        created
    };
    let frame = if CAPTURE_ARMED.with(Cell::get) {
        recv_frame(&mut worker, Some(SCAN_FRAME_TIMEOUT_MS)).await?
    } else {
        let frame = recv_frame(&mut worker, None).await?;
        CAPTURE_ARMED.with(|armed| armed.set(true));
        frame
    };
    let data = base64::engine::general_purpose::STANDARD.decode(frame.data)?;
    (FrameData {
        data,
        width: frame.width,
        height: frame.height,
    })
    .pipe(Ok)
}

fn frame_from_result(
    result: Result<Either<String, WireFrame>, dioxus::document::EvalError>,
) -> Result<WireFrame, Error> {
    match result? {
        Either::Right(frame) => Ok(frame),
        Either::Left(msg) => Err(Error::JS(msg)),
    }
}

#[cfg(target_arch = "wasm32")]
async fn recv_frame(eval: &mut Eval, timeout_ms: Option<u64>) -> Result<WireFrame, Error> {
    let mut recv = Box::pin(eval.recv::<Either<String, WireFrame>>());
    match timeout_ms {
        None => frame_from_result(recv.await),
        Some(timeout_ms) => {
            let mut timeout = Box::pin(gloo_timers::future::TimeoutFuture::new(
                u32::try_from(timeout_ms).unwrap_or(u32::MAX),
            ));
            let result = std::future::poll_fn(move |cx| {
                if let std::task::Poll::Ready(result) = std::pin::Pin::new(&mut recv).poll(cx) {
                    std::task::Poll::Ready(frame_from_result(result))
                } else {
                    match std::pin::Pin::new(&mut timeout).poll(cx) {
                        std::task::Poll::Ready(()) => std::task::Poll::Ready(Err(Error::CameraStalled)),
                        std::task::Poll::Pending => std::task::Poll::Pending,
                    }
                }
            })
            .await;
            result
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
async fn recv_frame(eval: &mut Eval, _timeout_ms: Option<u64>) -> Result<WireFrame, Error> {
    frame_from_result(eval.recv::<Either<String, WireFrame>>().await)
}

pub async fn stop_camera() -> Result<(), Error> {
    eval(
        (),
        r#"function(arg){
        window.__qrScanActive = false;
        const video = document.getElementById("qr-video");
        if (video && video.srcObject) {
        const tracks = video.srcObject.getTracks();
        tracks.forEach(track => track.stop());
        video.srcObject = null;
        }
        return null;
        }"#,
    )
    .await
}

#[derive(Serialize)]
pub struct ShareData {
    pub title: String,
    pub text: String,
    pub url: String,
}

#[cfg(target_arch = "wasm32")]
pub async fn sleep(millis: u64) -> Result<(), Error> {
    gloo_timers::future::TimeoutFuture::new(u32::try_from(millis).unwrap_or(u32::MAX)).await;
    Ok(())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn sleep(millis: u64) -> Result<(), Error> {
    std::thread::sleep(std::time::Duration::from_millis(millis));
    Ok(())
}

#[derive(Copy, Debug, Clone, PartialEq, Eq)]
pub enum PwaInstallOutcome {
    Accepted,
    Rejected,
    NotAvailable,
}

const PWA_INSTALL_TRIGGER_JS: &str = r#"function(arg) {
    if (!window.__functoraPwaDeferred) {
        return "NotAvailable";
    }
    const d = window.__functoraPwaDeferred;
    d.prompt();
    const result = await d.userChoice;
    window.__functoraPwaDeferred = null;
    return result.outcome === 'accepted' ? 'Accepted' : 'Rejected';
}
"#;

pub async fn trigger_pwa_install() -> Result<PwaInstallOutcome, Error> {
    let result: String = eval((), PWA_INSTALL_TRIGGER_JS).await?;
    Ok(match result.as_str() {
        "Accepted" => PwaInstallOutcome::Accepted,
        "Rejected" => PwaInstallOutcome::Rejected,
        _ => PwaInstallOutcome::NotAvailable,
    })
}

#[derive(Copy, Debug, Clone, PartialEq, Eq)]
pub enum InstallHint {
    Ios,
    Mac,
    Unavailable,
}

pub async fn install_hint() -> Result<InstallHint, Error> {
    let result: String = eval(
        (),
        r"function(arg){
        const ua = navigator.userAgent;
        if (/iPad|iPhone|iPod/.test(ua) ||
            (navigator.platform === 'MacIntel' && navigator.maxTouchPoints > 1)) {
        return 'ios';
        }
        if (/Macintosh/.test(ua) && /Safari\//.test(ua) &&
            !/Chrome|CriOS|Edg\/|OPR\/|FxiOS/.test(ua)) {
        const v = parseInt(ua.split('Version/')[1] || '', 10);
        if (v >= 17) {
        return 'mac';
        }
        }
        return '';
        }",
    )
    .await?;
    Ok(match result.as_str() {
        "ios" => InstallHint::Ios,
        "mac" => InstallHint::Mac,
        _ => InstallHint::Unavailable,
    })
}

pub fn write_clipboard<S: I18N + 'static>(
    val: String,
    mut message: impl Writable<Target = Option<S>> + 'static,
    success: S,
    map_error: impl FnOnce(crate::Error) -> S + 'static,
) {
    let _ = spawn(async move {
        match clipboard_write(val).await {
            Ok(()) => message.set(Some(success)),
            Err(e) => message.set(Some(map_error(e))),
        }
    });
}

pub(crate) async fn eval<A: Serialize + 'static, B: serde::de::DeserializeOwned + 'static>(
    arg: A,
    fun: &'static str,
) -> Result<B, Error> {
    let code = &format!(
        r#"
        let arg = await dioxus.recv();
        try {{
        let res = await (async {fun})(arg);
        dioxus.send({{"Right": res}});
        }} catch (e) {{
        dioxus.send({{"Left": String(e)}});
        }}
        "#
    );

    let mut eval = dioxus::document::eval(code);

    eval.send(arg)?;
    match eval.recv::<Either<String, B>>().await? {
        Either::Right(rhs) => Ok(rhs),
        Either::Left(lhs) => Err(Error::from(dioxus::document::EvalError::InvalidJs(lhs))),
    }
}
