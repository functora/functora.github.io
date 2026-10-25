//! Fallback backend for `wasm32` without the `web` feature: every op reports
//! its honest unavailability instead of each caller repeating the
//! feature-disabled stub. This module is only compiled for that target, where
//! it backs the `backend` alias in `super`.

use crate::camera::FrameData;
use crate::error::{Capability, Error};

#[cfg(feature = "clipboard")]
pub use super::ShareData;

#[cfg(feature = "clipboard")]
pub async fn clipboard_read() -> Result<String, Error> {
    std::future::ready(()).await;
    Err(Error::CapabilityDisabled(Capability::Clipboard))
}

#[cfg(feature = "clipboard")]
pub async fn clipboard_write(text: String) -> Result<(), Error> {
    std::future::ready(()).await;
    let _ = text;
    Err(Error::CapabilityDisabled(Capability::Clipboard))
}

#[cfg(feature = "clipboard")]
pub async fn share(data: ShareData) -> Result<(), Error> {
    std::future::ready(()).await;
    let _ = data;
    Err(Error::CapabilityDisabled(Capability::Share))
}

#[cfg(feature = "files")]
pub async fn download(data: Vec<u8>, filename: &str) -> Result<String, Error> {
    std::future::ready(()).await;
    let _ = (data, filename);
    Err(Error::CapabilityDisabled(Capability::Save))
}

pub async fn sleep(millis: u64) {
    std::future::ready(()).await;
    let _ = millis;
}

pub async fn check_camera() -> Result<(), Error> {
    std::future::ready(()).await;
    Err(Error::CameraNotAvailable(
        "Camera not available (web feature disabled)".into(),
    ))
}

pub async fn start_camera() -> Result<(), Error> {
    std::future::ready(()).await;
    Err(Error::CameraNotAvailable(
        "Camera not available (web feature disabled)".into(),
    ))
}

pub async fn capture_frame() -> Result<FrameData, Error> {
    std::future::ready(()).await;
    Err(Error::CameraNotAvailable(
        "Camera not available (web feature disabled)".into(),
    ))
}

pub fn begin_capture_session() {}

pub fn stop_capture_worker() {}

pub async fn stop_camera() -> Result<(), Error> {
    std::future::ready(()).await;
    Ok(())
}
