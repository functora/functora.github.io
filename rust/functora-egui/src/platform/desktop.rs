use crate::camera::FrameData;
use crate::error::Error;

pub async fn clipboard_read() -> Result<String, Error> {
    std::future::ready(()).await;
    let mut clipboard =
        arboard::Clipboard::new().map_err(|e| Error::JS(format!("Clipboard init: {e}")))?;
    clipboard
        .get_text()
        .map_err(|e| Error::JS(format!("Clipboard read: {e}")))
}

pub async fn clipboard_write(text: String) -> Result<(), Error> {
    std::future::ready(()).await;
    let mut clipboard =
        arboard::Clipboard::new().map_err(|e| Error::JS(format!("Clipboard init: {e}")))?;
    clipboard
        .set_text(text)
        .map_err(|e| Error::JS(format!("Clipboard write: {e}")))
}

#[derive(Debug, Clone)]
pub struct ShareData {
    pub title: String,
    pub text: String,
    pub url: String,
}

pub async fn share(data: ShareData) -> Result<(), Error> {
    let full = format!("{}\n{}\n{}", data.title, data.text, data.url);
    if let Err(e) = clipboard_write(full).await {
        tracing::warn!("Share fallback clipboard failed: {e}");
    }
    Ok(())
}

pub async fn download(data: Vec<u8>, filename: &str) -> Result<String, Error> {
    let handle = rfd::AsyncFileDialog::new()
        .set_file_name(filename)
        .save_file()
        .await
        .ok_or_else(|| Error::JS("Save cancelled".into()))?;
    handle
        .write(&data)
        .await
        .map_err(|e| Error::JS(format!("{e}")))?;
    Ok(filename.to_string())
}

pub async fn sleep(millis: u64) {
    std::future::ready(()).await;
    std::thread::sleep(std::time::Duration::from_millis(millis));
}

#[must_use]
pub fn storage_get(_key: &str) -> Option<String> {
    None
}

pub fn storage_set(_key: &str, _value: &str) -> Result<(), Error> {
    Ok(())
}

pub async fn check_camera() -> Result<(), Error> {
    std::future::ready(()).await;
    Err(Error::CameraNotAvailable(
        "Camera not available on desktop – use file picker".into(),
    ))
}

pub async fn start_camera() -> Result<(), Error> {
    std::future::ready(()).await;
    Err(Error::CameraNotAvailable(
        "Camera not available on desktop – use file picker".into(),
    ))
}

pub async fn capture_frame() -> Result<FrameData, Error> {
    std::future::ready(()).await;
    Err(Error::CameraNotAvailable(
        "Camera not available on desktop – use file picker".into(),
    ))
}

pub fn begin_capture_session() {}

pub fn stop_capture_worker() {}

pub async fn stop_camera() -> Result<(), Error> {
    std::future::ready(()).await;
    Ok(())
}
