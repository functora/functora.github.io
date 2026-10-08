use crate::camera::FrameData;
use crate::error::Error;

#[cfg(feature = "clipboard")]
pub async fn clipboard_read() -> Result<String, Error> {
    std::future::ready(()).await;
    let mut clipboard =
        arboard::Clipboard::new().map_err(|e| Error::JS(format!("Clipboard init: {e}")))?;
    clipboard
        .get_text()
        .map_err(|e| Error::JS(format!("Clipboard read: {e}")))
}

#[cfg(feature = "clipboard")]
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

const OPEN_URL_POLL_INTERVAL_MS: u64 = 50;
const OPEN_URL_POLL_COUNT: u32 = 200;

fn has_desktop_session() -> bool {
    std::env::var_os("DISPLAY").is_some() || std::env::var_os("WAYLAND_DISPLAY").is_some()
}

pub async fn open_url(url: String) -> Result<(), Error> {
    std::future::ready(()).await;
    if !has_desktop_session() {
        return Err(Error::JS(format!("no desktop session to open {url}")));
    }
    open_url_wait(&url)
}

fn await_exit(child: &mut std::process::Child) -> Result<Option<std::process::ExitStatus>, Error> {
    (0..OPEN_URL_POLL_COUNT)
        .find_map(|poll| {
            if poll > 0 {
                std::thread::sleep(std::time::Duration::from_millis(OPEN_URL_POLL_INTERVAL_MS));
            }
            child.try_wait().map_err(Error::from).transpose()
        })
        .transpose()
}

fn open_url_wait(url: &str) -> Result<(), Error> {
    let mut child = std::process::Command::new("xdg-open")
        .arg(url)
        .stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .spawn()
        .map_err(Error::from)?;
    if let Some(status) = await_exit(&mut child)? {
        status
            .success()
            .then_some(())
            .ok_or_else(|| Error::JS(format!("xdg-open failed for {url}")))
    } else {
        let _kill_outcome = child.kill();
        let _wait_outcome = child.wait();
        Err(Error::JS(format!("xdg-open timed out for {url}")))
    }
}

#[cfg(feature = "clipboard")]
pub async fn share(data: ShareData) -> Result<(), Error> {
    let full = format!("{}\n{}\n{}", data.title, data.text, data.url);
    if let Err(write_error) = clipboard_write(full).await {
        tracing::warn!("Share fallback clipboard failed: {write_error}");
    }
    if data.url.starts_with("http")
        && let Err(open_error) = open_url(data.url).await
    {
        tracing::warn!("Share open_url failed: {open_error}");
    }
    Ok(())
}

#[cfg(feature = "files")]
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

#[cfg(feature = "storage")]
#[must_use]
pub fn storage_get(app: &'static str, key: &str) -> Option<String> {
    crate::storage::Storage::new(app).load(key)
}

#[cfg(not(feature = "storage"))]
#[must_use]
pub fn storage_get(_app: &'static str, _key: &str) -> Option<String> {
    None
}

#[cfg(feature = "storage")]
pub fn storage_set(app: &'static str, key: &str, value: &str) -> Result<(), Error> {
    crate::storage::Storage::new(app).persist(key, &value.to_owned());
    Ok(())
}

#[cfg(not(feature = "storage"))]
pub fn storage_set(_app: &'static str, _key: &str, _value: &str) -> Result<(), Error> {
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
