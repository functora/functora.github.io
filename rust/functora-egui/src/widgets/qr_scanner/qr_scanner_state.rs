use crate::camera::FrameData;
use crate::error::Error;
use crate::widgets::camera_view::camera_view_state::{CameraViewState, FrameHandler};
use std::ops::ControlFlow;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

type ScanCallback = Arc<dyn Fn(String) + Send + Sync>;
type ErrorCallback = Arc<dyn Fn(&Error) + Send + Sync>;

struct ScanConfig {
    decode_period: Duration,
    dedupe: Duration,
    continuous: bool,
}

struct ScanRuntime {
    config: ScanConfig,
    on_scan: Option<ScanCallback>,
    on_error: Option<ErrorCallback>,
    last_decode: Option<Instant>,
    last_hit: Option<(String, Instant)>,
}

/// State for the automatic QR scanner. Embeds the shared camera feed engine
/// (`CameraViewState`) and adds rate-limited decoding plus callbacks.
pub struct QrScannerState {
    pub(crate) camera: CameraViewState,
    runtime: Arc<Mutex<ScanRuntime>>,
    decoded: Arc<Mutex<Option<String>>>,
}

impl Default for QrScannerState {
    fn default() -> Self {
        Self {
            camera: CameraViewState::new(),
            runtime: Arc::new(Mutex::new(ScanRuntime {
                config: ScanConfig {
                    decode_period: Duration::from_millis(200),
                    dedupe: Duration::from_millis(1500),
                    continuous: false,
                },
                on_scan: None,
                on_error: None,
                last_decode: None,
                last_hit: None,
            })),
            decoded: Arc::new(Mutex::new(None)),
        }
    }
}

impl QrScannerState {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    #[must_use]
    pub fn is_scanning(&self) -> bool {
        self.camera.is_running()
    }

    #[must_use]
    pub fn error(&self) -> Option<Arc<Error>> {
        self.camera.error()
    }

    #[must_use]
    pub fn decoded(&self) -> Option<String> {
        self.decoded.lock().ok().and_then(|slot| slot.clone())
    }

    #[must_use]
    pub fn take_decoded(&mut self) -> Option<String> {
        self.decoded.lock().ok().and_then(|mut slot| slot.take())
    }

    pub fn clear_error(&mut self) {
        self.camera.clear_error();
    }

    pub fn clear_decoded(&mut self) {
        if let Ok(mut slot) = self.decoded.lock() {
            *slot = None;
        }
    }

    /// Starts scanning automatically (idempotent while running).
    pub fn start(&mut self, ctx: &egui::Context) -> bool {
        if self.camera.is_running() {
            return true;
        }
        self.clear_error();
        self.camera.set_handler(Some(self.make_handler()));
        self.camera.start(ctx)
    }

    /// Stops scanning; freezes the last preview frame.
    pub fn stop(&mut self) {
        self.camera.stop();
    }

    pub(crate) fn drain_rgba(&self) -> Option<(Vec<u8>, u32, u32)> {
        self.camera.drain_rgba()
    }

    pub(crate) fn store_preview(&mut self, ctx: &egui::Context, rgba: &[u8], w: u32, h: u32) {
        self.camera.store_texture(ctx, rgba, w, h);
    }

    #[must_use]
    pub fn preview_texture(&mut self) -> Option<egui::TextureHandle> {
        self.camera.take_texture().clone()
    }

    pub(crate) fn on_error_callback(&self) -> Option<ErrorCallback> {
        self.runtime.lock().ok().and_then(|rt| rt.on_error.clone())
    }

    #[cfg(any(target_arch = "wasm32", not(target_os = "android")))]
    #[must_use]
    pub(crate) fn pick_slots(&self) -> PickSlots {
        PickSlots {
            decoded: Arc::clone(&self.decoded),
            runtime: Arc::clone(&self.runtime),
        }
    }

    /// Builder-facing configuration applied at `show` time before starting.
    pub(crate) fn configure(
        &mut self,
        fps: f32,
        decode_fps: f32,
        dedupe_ms: u64,
        continuous: bool,
        on_scan: Option<ScanCallback>,
        on_error: Option<ErrorCallback>,
    ) {
        let capture_period = Duration::from_millis(crate::utils::fps_to_interval_ms(fps));
        let raw_decode = Duration::from_millis(crate::utils::fps_to_interval_ms(
            decode_fps.clamp(0.5, 60.0),
        ));
        if let Ok(mut rt) = self.runtime.lock() {
            rt.config = ScanConfig {
                decode_period: raw_decode.max(capture_period),
                dedupe: Duration::from_millis(dedupe_ms),
                continuous,
            };
            rt.on_scan = on_scan;
            rt.on_error = on_error;
            rt.last_decode = None;
            rt.last_hit = None;
        }
        self.camera.set_fps(fps);
    }

    fn make_handler(&self) -> FrameHandler {
        let runtime = Arc::clone(&self.runtime);
        let decoded_slot = Arc::clone(&self.decoded);
        Box::new(move |ctx: &egui::Context, frame: &FrameData| {
            handle_frame(ctx, frame, &runtime, &decoded_slot)
        })
    }
}

fn handle_frame(
    ctx: &egui::Context,
    frame: &FrameData,
    runtime: &Arc<Mutex<ScanRuntime>>,
    decoded_slot: &Arc<Mutex<Option<String>>>,
) -> ControlFlow<(), ()> {
    let _ = (&frame.data, frame.width, frame.height);
    let now = Instant::now();
    let Some(snapshot) = runtime.lock().ok().map(|rt| {
        (
            rt.config.decode_period,
            rt.config.dedupe,
            rt.config.continuous,
            rt.last_decode,
        )
    }) else {
        return ControlFlow::Break(());
    };
    let (decode_period, dedupe, continuous, last_decode) = snapshot;
    if let Some(last) = last_decode
        && now.duration_since(last) < decode_period
    {
        ctx.request_repaint_after(decode_period.saturating_sub(now.duration_since(last)));
        return ControlFlow::Continue(());
    }
    if let Ok(mut rt) = runtime.lock() {
        rt.last_decode = Some(now);
    }

    #[cfg(feature = "qr")]
    let Some(text) = crate::qr::decode_qr_luma(&frame.data, frame.width, frame.height) else {
        ctx.request_repaint_after(decode_period);
        return ControlFlow::Continue(());
    };

    #[cfg(feature = "qr")]
    {
        let duplicate = continuous
            && runtime.lock().ok().is_some_and(|rt| {
                rt.last_hit
                    .as_ref()
                    .is_some_and(|(hit, seen)| *hit == text && now.duration_since(*seen) < dedupe)
            });
        if duplicate {
            ctx.request_repaint_after(dedupe.min(decode_period));
            return ControlFlow::Continue(());
        }
        if let Ok(mut slot) = decoded_slot.lock() {
            *slot = Some(text.clone());
        }
        if let Ok(mut rt) = runtime.lock() {
            rt.last_hit = continuous.then(|| (text.clone(), now));
        }
        if let Some(cb) = runtime.lock().ok().and_then(|rt| rt.on_scan.clone()) {
            cb(text);
        }
        ctx.request_repaint();
        if continuous {
            ControlFlow::Continue(())
        } else {
            ControlFlow::Break(())
        }
    }

    #[cfg(not(feature = "qr"))]
    {
        let _ = (dedupe, continuous, decoded_slot, last_decode);
        ctx.request_repaint_after(decode_period);
        ControlFlow::Continue(())
    }
}

/// Cross-thread handles for the file-picker fallback path.
#[cfg(any(target_arch = "wasm32", not(target_os = "android")))]
pub(crate) struct PickSlots {
    decoded: Arc<Mutex<Option<String>>>,
    runtime: Arc<Mutex<ScanRuntime>>,
}

#[cfg(any(target_arch = "wasm32", not(target_os = "android")))]
impl PickSlots {
    pub(crate) fn set_decoded(&self, text: String) {
        if let Ok(mut slot) = self.decoded.lock() {
            *slot = Some(text);
        }
        let stored = self.decoded.lock().ok().and_then(|slot| slot.clone());
        if let Some(callback) = self.runtime.lock().ok().and_then(|rt| rt.on_scan.clone())
            && let Some(code) = stored
        {
            callback(code);
        }
    }

    pub(crate) fn set_error(&self, err: &Error) {
        if let Some(cb) = self.runtime.lock().ok().and_then(|rt| rt.on_error.clone()) {
            cb(err);
        }
    }

    pub(crate) fn set_error_message(&self, message: &str) {
        self.set_error(&Error::JS(message.into()));
    }
}
