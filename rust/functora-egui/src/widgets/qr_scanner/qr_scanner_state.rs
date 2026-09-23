use crate::camera::FrameData;
use crate::error::Error;
use crate::widgets::camera_view::camera_view_state::{CameraViewState, FrameHandler};
use std::ops::ControlFlow;
use std::sync::{Arc, Mutex};
use std::time::Duration;

type ScanCallback = Arc<dyn Fn(String) + Send + Sync>;
type ErrorCallback = Arc<dyn Fn(&Error) + Send + Sync>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ScanConfig {
    decode_period: Duration,
    dedupe: Duration,
    continuous: bool,
}

struct ScanRuntime {
    config: ScanConfig,
    on_scan: Option<ScanCallback>,
    on_error: Option<ErrorCallback>,
    last_decode: Option<f64>,
    last_hit: Option<(String, f64)>,
}

#[cfg(feature = "qr")]
const DETECT_LONG_SIDE: f32 = 240.0;

#[cfg(feature = "qr")]
fn downscaled_luma(data: &[u8], width: u32, height: u32) -> Option<(Vec<u8>, u32, u32)> {
    if crate::utils::u32_to_f32(width.max(height)) <= DETECT_LONG_SIDE {
        return None;
    }
    let scale = DETECT_LONG_SIDE / crate::utils::u32_to_f32(width.max(height));
    let narrow_w = crate::utils::scaled_px(width, scale);
    let narrow_h = crate::utils::scaled_px(height, scale);
    let expected = u64::from(width).checked_mul(u64::from(height))?;
    if u64::try_from(data.len()).ok()? != expected {
        return None;
    }
    let narrow = (0..narrow_h)
        .flat_map(|row| (0..narrow_w).map(move |col| (row, col)))
        .filter_map(|(row, col)| {
            let src_x = u64::from(col) * u64::from(width) / u64::from(narrow_w);
            let src_y = u64::from(row) * u64::from(height) / u64::from(narrow_h);
            let idx = src_y.checked_mul(u64::from(width))?.checked_add(src_x)?;
            data.get(usize::try_from(idx).ok()?).copied()
        })
        .collect::<Vec<u8>>();
    (narrow.len() == crate::utils::pixel_area_len(narrow_w, narrow_h))
        .then_some((narrow, narrow_w, narrow_h))
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

    #[must_use]
    pub fn preview_size(&self) -> Option<(u32, u32)> {
        self.camera.preview_size()
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
            let next = ScanConfig {
                decode_period: raw_decode.max(capture_period),
                dedupe: Duration::from_millis(dedupe_ms),
                continuous,
            };
            if rt.config != next {
                rt.config = next;
                rt.last_decode = None;
                rt.last_hit = None;
            }
            rt.on_scan = on_scan;
            rt.on_error = on_error;
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
    let now = ctx.input(|i| i.time);
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
    let window_secs = decode_period.as_secs_f64();
    if let Some(last) = last_decode
        && (now - last).max(0.0) < window_secs
    {
        ctx.request_repaint_after(Duration::from_secs_f64(
            (window_secs - (now - last).max(0.0)).max(0.0),
        ));
        return ControlFlow::Continue(());
    }
    if let Ok(mut rt) = runtime.lock() {
        rt.last_decode = Some(now);
    }

    #[cfg(feature = "qr")]
    let shrunken = downscaled_luma(&frame.data, frame.width, frame.height);
    #[cfg(feature = "qr")]
    let (luma_view, detect_w, detect_h) = shrunken.as_ref().map_or(
        (&frame.data[..], frame.width, frame.height),
        |(narrow, narrow_w, narrow_h)| (&narrow[..], *narrow_w, *narrow_h),
    );
    #[cfg(feature = "qr")]
    let decoded_text = crate::qr::decode_qr_luma_fast(luma_view, detect_w, detect_h);
    #[cfg(feature = "qr")]
    let Some(text) = decoded_text else {
        ctx.request_repaint_after(decode_period);
        return ControlFlow::Continue(());
    };

    #[cfg(feature = "qr")]
    {
        let duplicate = continuous
            && runtime.lock().ok().is_some_and(|rt| {
                rt.last_hit.as_ref().is_some_and(|(hit, seen)| {
                    *hit == text && (now - *seen).max(0.0) < dedupe.as_secs_f64()
                })
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

#[cfg(test)]
mod tests {
    use super::QrScannerState;

    #[test]
    fn configure_keeps_decode_throttle_when_unchanged() {
        let mut state = QrScannerState::new();
        state.configure(15.0, 5.0, 1500, false, None, None);
        if let Ok(mut runtime) = state.runtime.lock() {
            runtime.last_decode = Some(3600.0);
        }
        state.configure(15.0, 5.0, 1500, false, None, None);
        assert!(
            state
                .runtime
                .lock()
                .ok()
                .is_some_and(|runtime| runtime.last_decode.is_some()),
            "identical configure must preserve decode throttle"
        );
    }

    #[test]
    fn configure_resets_throttle_when_rate_changes() {
        let mut state = QrScannerState::new();
        state.configure(15.0, 5.0, 1500, false, None, None);
        if let Ok(mut runtime) = state.runtime.lock() {
            runtime.last_decode = Some(3600.0);
        }
        state.configure(15.0, 1.0, 1500, false, None, None);
        assert!(
            state
                .runtime
                .lock()
                .ok()
                .is_some_and(|runtime| runtime.last_decode.is_none()),
            "changed decode rate must restart throttle window"
        );
    }

    #[test]
    fn downscaled_luma_keeps_small_frames_as_is() {
        let data = vec![0xAB; 100 * 80];
        assert!(super::downscaled_luma(&data, 100, 80).is_none());
        let capture_sized = vec![0xAB; 240 * 180];
        assert!(super::downscaled_luma(&capture_sized, 240, 180).is_none());
        assert!(super::downscaled_luma(&[], 0, 0).is_none());
    }

    #[test]
    fn downscaled_luma_rejects_mismatched_buffer() {
        assert!(super::downscaled_luma(&[0; 10], 480, 360).is_none());
    }

    #[test]
    fn downscaled_luma_shrinks_large_frames() {
        let data = vec![0xAB; 480 * 360];
        let shrunk = super::downscaled_luma(&data, 480, 360);
        assert!(shrunk.is_some(), "large frame must shrink");
        if let Some((narrow, narrow_w, narrow_h)) = shrunk {
            assert!(narrow_w.max(narrow_h) <= 240);
            assert_eq!(
                narrow.len(),
                crate::utils::pixel_area_len(narrow_w, narrow_h)
            );
            assert!(narrow.iter().all(|px| *px == 0xAB));
        }
    }

    #[cfg(feature = "qr")]
    fn decodable_frame(content: &str, side: u32) -> Option<crate::camera::FrameData> {
        let (w, h, rgba) = crate::qr::qr_rgba(content, side)?;
        let luma = rgba
            .chunks_exact(4)
            .map(|px| if px[0] == 0 { 0 } else { 0xFF })
            .collect();
        Some(crate::camera::FrameData {
            data: luma,
            width: w,
            height: h,
            preview_rgba: None,
        })
    }

    #[cfg(feature = "qr")]
    fn scan_runtime() -> (
        std::sync::Arc<std::sync::Mutex<super::ScanRuntime>>,
        std::sync::Arc<std::sync::Mutex<Option<String>>>,
    ) {
        use std::time::Duration;
        (
            std::sync::Arc::new(std::sync::Mutex::new(super::ScanRuntime {
                config: super::ScanConfig {
                    decode_period: Duration::from_millis(200),
                    dedupe: Duration::from_millis(1500),
                    continuous: true,
                },
                on_scan: None,
                on_error: None,
                last_decode: None,
                last_hit: None,
            })),
            std::sync::Arc::new(std::sync::Mutex::new(None)),
        )
    }

    #[cfg(feature = "qr")]
    #[test]
    fn live_scan_skips_decode_inside_throttle_window() {
        use std::ops::ControlFlow;
        let fixture = decodable_frame("https://functora.github.io", 128);
        assert!(fixture.is_some(), "fixture QR must encode");
        if let Some(frame) = fixture {
            let (runtime, decoded) = scan_runtime();
            if let Ok(mut guard) = runtime.lock() {
                guard.last_decode = Some(3600.0);
            }
            let ctx = egui::Context::default();
            let outcome = super::handle_frame(&ctx, &frame, &runtime, &decoded);
            assert!(matches!(outcome, ControlFlow::Continue(())));
            assert!(
                decoded.lock().ok().is_some_and(|slot| slot.is_none()),
                "throttled scan must not decode"
            );
        }
    }

    #[cfg(feature = "qr")]
    #[test]
    fn live_scan_decodes_large_frame_after_downscale() {
        use std::ops::ControlFlow;
        let fixture = decodable_frame("https://functora.github.io", 480);
        assert!(fixture.is_some(), "fixture QR must encode");
        if let Some(frame) = fixture {
            assert!(
                frame.width.max(frame.height) > 240,
                "fixture must need downscaling"
            );
            let (runtime, decoded) = scan_runtime();
            let ctx = egui::Context::default();
            let outcome = super::handle_frame(&ctx, &frame, &runtime, &decoded);
            assert!(matches!(outcome, ControlFlow::Continue(())));
            assert_eq!(
                decoded.lock().ok().and_then(|slot| slot.clone()),
                Some("https://functora.github.io".to_owned())
            );
        }
    }

    #[cfg(feature = "qr")]
    #[test]
    fn live_scan_decodes_outside_throttle_window() {
        use std::ops::ControlFlow;
        let fixture = decodable_frame("https://functora.github.io", 128);
        assert!(fixture.is_some(), "fixture QR must encode");
        if let Some(frame) = fixture {
            let (runtime, decoded) = scan_runtime();
            let ctx = egui::Context::default();
            let outcome = super::handle_frame(&ctx, &frame, &runtime, &decoded);
            assert!(matches!(outcome, ControlFlow::Continue(())));
            assert_eq!(
                decoded.lock().ok().and_then(|slot| slot.clone()),
                Some("https://functora.github.io".to_owned())
            );
        }
    }
}
