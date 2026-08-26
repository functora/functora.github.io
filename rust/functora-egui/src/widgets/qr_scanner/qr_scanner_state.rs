use crate::error::Error;
use crate::in_flight::InFlight;
use std::sync::{
    Arc, Mutex,
    atomic::{AtomicU64, Ordering},
};

static CAMERA_EPOCH: AtomicU64 = AtomicU64::new(0);

#[derive(Debug, Default)]
pub struct QrInner {
    pub scanning: bool,
    pub error: Option<Arc<Error>>,
    pub decoded: Option<String>,
    pub latest_rgba: Option<(Vec<u8>, u32, u32)>,
    pub epoch: u64,
}

pub struct QrScannerState {
    inner: Arc<Mutex<QrInner>>,
    texture: Option<egui::TextureHandle>,
    in_flight: InFlight,
}

impl Default for QrScannerState {
    fn default() -> Self {
        Self {
            inner: Arc::new(Mutex::new(QrInner::default())),
            texture: None,
            in_flight: InFlight::default(),
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
        self.inner.lock().ok().is_some_and(|g| g.scanning)
    }

    #[must_use]
    pub fn error(&self) -> Option<Arc<Error>> {
        self.inner.lock().ok().and_then(|g| g.error.clone())
    }

    #[must_use]
    pub fn decoded(&self) -> Option<String> {
        self.inner.lock().ok().and_then(|g| g.decoded.clone())
    }

    pub fn take_decoded(&mut self) -> Option<String> {
        self.inner.lock().ok().and_then(|mut g| g.decoded.take())
    }

    pub fn clear_error(&mut self) {
        if let Ok(mut g) = self.inner.lock() {
            g.error = None;
        }
    }

    pub fn clear_decoded(&mut self) {
        if let Ok(mut g) = self.inner.lock() {
            g.decoded = None;
        }
    }

    pub fn stop(&mut self) {
        if let Ok(mut g) = self.inner.lock() {
            g.scanning = false;
        }
        let epoch = CAMERA_EPOCH.fetch_add(1, Ordering::SeqCst) + 1;
        if let Ok(mut g) = self.inner.lock() {
            g.epoch = epoch;
        }
        self.texture = None;
        crate::camera::stop_capture_worker();
    }

    #[must_use]
    pub fn inner_arc(&self) -> Arc<Mutex<QrInner>> {
        Arc::clone(&self.inner)
    }

    pub(crate) fn texture_mut(&mut self) -> &mut Option<egui::TextureHandle> {
        &mut self.texture
    }

    pub(crate) fn in_flight(&self) -> &InFlight {
        &self.in_flight
    }

    pub(crate) fn set_scanning(&self, scanning: bool) {
        if let Ok(mut g) = self.inner.lock() {
            g.scanning = scanning;
            if scanning {
                g.error = None;
                g.decoded = None;
            }
        }
    }

    pub(crate) fn take_latest_rgba(&self) -> Option<(Vec<u8>, u32, u32)> {
        self.inner
            .lock()
            .ok()
            .and_then(|mut g| g.latest_rgba.take())
    }

    pub(crate) fn bump_epoch(&self) -> u64 {
        let epoch = CAMERA_EPOCH.fetch_add(1, Ordering::SeqCst) + 1;
        if let Ok(mut g) = self.inner.lock() {
            g.epoch = epoch;
        }
        epoch
    }
}

#[cfg(any(
    all(target_arch = "wasm32", feature = "web"),
    all(target_os = "android", feature = "qr")
))]
pub(crate) fn camera_epoch() -> u64 {
    CAMERA_EPOCH.load(Ordering::SeqCst)
}
