use crate::error::Error;

pub async fn download(data: Vec<u8>, filename: &str) -> Result<String, Error> {
    std::future::ready(()).await;
    download_with_progress(data, filename, |_, _| {}).await
}

#[allow(clippy::unused_async)]
pub async fn download_with_progress(
    data: Vec<u8>,
    filename: &str,
    on_progress: impl FnMut(u64, u64),
) -> Result<String, Error> {
    #[cfg(target_os = "android")]
    {
        let name = filename.to_string();
        crate::platform::android::save_to_downloads(&data, &name, on_progress)?;
        return Ok(name);
    }
    #[cfg(not(target_os = "android"))]
    {
        let _ = on_progress;
        return crate::platform::backend::download(data, filename).await;
    }
    #[allow(unreachable_code)]
    {
        let _ = (data, filename, on_progress);
        Err(Error::JS("Download not available".into()))
    }
}
