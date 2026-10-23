use crate::error::Error;

pub async fn download(data: Vec<u8>, filename: &str) -> Result<String, Error> {
    std::future::ready(()).await;
    download_with_progress(data, filename, |_, _| {}).await
}

pub async fn download_with_progress(
    data: Vec<u8>,
    filename: &str,
    on_progress: impl FnMut(u64, u64),
) -> Result<String, Error> {
    std::future::ready(()).await;
    #[cfg(target_os = "android")]
    {
        let name = filename.to_string();
        crate::platform::android::save_to_downloads(&data, &name, on_progress)?;
        Ok(name)
    }
    #[cfg(not(target_os = "android"))]
    {
        let _ = on_progress;
        crate::platform::backend::download(data, filename).await
    }
}
