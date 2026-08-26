use crate::camera::FrameData;
use crate::error::Error;
use crate::in_flight::InFlight;
use std::ops::ControlFlow;
use std::sync::{
    Arc, Mutex,
    atomic::{AtomicU64, Ordering},
};

pub static CAMERA_EPOCH: AtomicU64 = AtomicU64::new(0);

#[must_use]
pub fn camera_epoch() -> u64 {
    CAMERA_EPOCH.load(Ordering::SeqCst)
}

/// Decides whether the capture loop keeps running after one frame.
///
/// `Continue(())` keeps the live feed going; `Break(())` ends the session
/// (for example after a decoded QR code when the scanner is not continuous).
pub type FrameHandler = Box<dyn FnMut(&egui::Context, &FrameData) -> ControlFlow<(), ()> + Send>;

/// Shared mutable state driven by the platform frame pump.
#[derive(Debug, Default)]
pub struct FeedInner {
    pub(crate) running: bool,
    pub(crate) error: Option<Arc<Error>>,
    pub(crate) latest_rgba: Option<(Vec<u8>, u32, u32)>,
    pub(crate) epoch: u64,
    pub(crate) interval_ms: u64,
}

pub struct CameraViewState {
    inner: Arc<Mutex<FeedInner>>,
    texture: Option<egui::TextureHandle>,
    in_flight: InFlight,
    handler: Option<FrameHandler>,
}

impl Default for CameraViewState {
    fn default() -> Self {
        Self {
            inner: Arc::new(Mutex::new(FeedInner {
                interval_ms: 66,
                ..FeedInner::default()
            })),
            texture: None,
            in_flight: InFlight::default(),
            handler: None,
        }
    }
}

impl CameraViewState {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Sets the live-feed capture rate (clamped to 1..=60 fps).
    #[must_use]
    pub fn with_fps(self, fps: f32) -> Self {
        let interval = crate::utils::fps_to_interval_ms(fps);
        if let Ok(mut feed) = self.inner.lock() {
            feed.interval_ms = interval;
        }
        self
    }

    /// Installs a pass-through handler when none was provided, so a bare
    /// `CameraView` can run the feed without custom frame processing.
    pub fn ensure_default_handler(&mut self) {
        if self.handler.is_none() {
            self.handler = Some(Box::new(
                |_ctx: &egui::Context, _frame: &crate::camera::FrameData| {
                    std::ops::ControlFlow::Continue(())
                },
            ));
        }
    }

    pub fn set_handler(&mut self, handler: Option<FrameHandler>) {
        self.handler = handler;
    }

    pub fn set_fps(&self, fps: f32) {
        let interval = crate::utils::fps_to_interval_ms(fps);
        if let Ok(mut feed) = self.inner.lock() {
            feed.interval_ms = interval;
        }
    }

    #[must_use]
    pub fn interval_ms(&self) -> u64 {
        self.inner.lock().ok().map_or(66, |feed| feed.interval_ms)
    }

    /// Starts the capture pump unless it is already running or another
    /// session holds the in-flight slot. Returns true when running.
    pub fn start(&mut self, ctx: &egui::Context) -> bool {
        if self.is_running() {
            return true;
        }
        let Some(guard) = self.in_flight.claim() else {
            return false;
        };
        let Some(handler) = self.handler.take() else {
            drop(guard);
            return false;
        };
        let epoch = CAMERA_EPOCH.fetch_add(1, Ordering::SeqCst) + 1;
        let interval_ms = self.inner.lock().ok().map_or(66, |feed| feed.interval_ms);
        if let Ok(mut feed) = self.inner.lock() {
            feed.epoch = epoch;
            feed.running = true;
            feed.error = None;
        }
        super::pump::spawn_pump(
            (*ctx).clone(),
            Arc::clone(&self.inner),
            epoch,
            interval_ms,
            handler,
            guard,
        );
        true
    }

    /// Signals the pump to stop and releases the camera; freezes the last
    /// preview frame.
    pub fn stop(&mut self) {
        let epoch = CAMERA_EPOCH.fetch_add(1, Ordering::SeqCst) + 1;
        if let Ok(mut feed) = self.inner.lock() {
            feed.running = false;
            feed.epoch = epoch;
        }
        crate::camera::stop_capture_worker();
        #[cfg(target_os = "android")]
        crate::platform::android::stop_camera_blocking();
    }

    #[must_use]
    pub fn is_running(&self) -> bool {
        self.inner.lock().ok().is_some_and(|feed| feed.running)
    }

    #[must_use]
    pub fn error(&self) -> Option<Arc<Error>> {
        self.inner.lock().ok().and_then(|feed| feed.error.clone())
    }

    pub fn clear_error(&mut self) {
        if let Ok(mut feed) = self.inner.lock() {
            feed.error = None;
        }
    }

    pub(crate) fn drain_rgba(&self) -> Option<(Vec<u8>, u32, u32)> {
        self.inner
            .lock()
            .ok()
            .and_then(|mut feed| feed.latest_rgba.take())
    }

    pub(crate) fn take_texture(&mut self) -> &mut Option<egui::TextureHandle> {
        &mut self.texture
    }

    /// Uploads a frame into the preview texture, reusing the existing handle.
    pub(crate) fn store_texture(
        &mut self,
        ctx: &egui::Context,
        rgba: &[u8],
        width: u32,
        height: u32,
    ) {
        let image =
            egui::ColorImage::from_rgba_unmultiplied([width as usize, height as usize], rgba);
        match &self.texture {
            Some(tex) => {
                let mut updated = egui::TextureHandle::clone(tex);
                updated.set(image, egui::TextureOptions::LINEAR);
            }
            None => {
                self.texture =
                    Some(ctx.load_texture("functora-camera", image, egui::TextureOptions::LINEAR));
            }
        }
    }
}

/// Maps low-level camera failures onto precise public error variants.
#[cfg(any(all(target_arch = "wasm32", feature = "web"), target_os = "android"))]
pub(crate) fn map_camera_error(e: &Error) -> Error {
    match e {
        Error::JS(msg) => {
            if msg.contains("Permission") || msg.contains("denied") || msg.contains("NotAllowed") {
                Error::CameraPermissionDenied(msg.clone())
            } else {
                Error::CameraNotAvailable(msg.clone())
            }
        }
        other => Error::CameraNotAvailable(other.to_string()),
    }
}
