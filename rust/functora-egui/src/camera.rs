use crate::error::Error;

#[derive(Debug, Clone)]
pub struct FrameData {
    pub data: Vec<u8>,
    pub width: u32,
    pub height: u32,
    pub preview_rgba: Option<Vec<u8>>,
}

impl FrameData {
    #[must_use]
    pub fn upright(self, rotation: crate::utils::FrameRotation) -> Self {
        let Self {
            data,
            width,
            height,
            preview_rgba,
        } = self;
        let (upright_data, upright_width, upright_height) =
            crate::utils::rotate_luma(&data, width, height, rotation);
        let upright_preview =
            preview_rgba.map(|rgba| crate::utils::rotate_rgba(&rgba, width, height, rotation).0);
        Self {
            data: upright_data,
            width: upright_width,
            height: upright_height,
            preview_rgba: upright_preview,
        }
    }
}

fn camera_error(msg: String) -> Error {
    if msg.contains("Permission") || msg.contains("denied") || msg.contains("NotAllowed") {
        Error::CameraPermissionDenied(msg)
    } else {
        Error::CameraNotAvailable(msg)
    }
}

/// Maps backend JS failures to camera errors. Backends that already report
/// `CameraNotAvailable`/`CameraPermissionDenied` pass through unchanged.
fn map_camera_error(error: Error) -> Error {
    if let Error::JS(msg) = error {
        camera_error(msg)
    } else {
        error
    }
}

pub async fn check_camera() -> Result<(), Error> {
    crate::platform::backend::check_camera().await
}

pub async fn start_camera() -> Result<(), Error> {
    crate::platform::backend::start_camera()
        .await
        .map_err(map_camera_error)
}

pub async fn capture_frame() -> Result<FrameData, Error> {
    crate::platform::backend::capture_frame().await
}

pub fn begin_capture_session() {
    crate::platform::backend::begin_capture_session();
}

pub fn stop_capture_worker() {
    crate::platform::backend::stop_capture_worker();
}

pub async fn stop_camera() -> Result<(), Error> {
    crate::platform::backend::stop_camera().await
}

pub async fn sleep(millis: u64) -> Result<(), Error> {
    crate::platform::backend::sleep(millis).await;
    Ok(())
}
