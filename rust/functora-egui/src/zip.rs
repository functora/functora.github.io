pub use functora_core::files::Attachment;
pub use functora_core::zip::{create_zip_report, unzip_report};

use crate::error::Error;
use crate::progress::Job;
use crate::worker;

pub async fn create_zip_async<S, P>(
    files: &[Attachment],
    progress: P,
    stage: S,
    token: std::sync::Arc<std::sync::atomic::AtomicBool>,
) -> Result<Vec<u8>, Error>
where
    S: Copy + Send + Sync + 'static,
    P: FnMut(Option<Job<S>>) + Send + 'static,
{
    let file_entries = files
        .iter()
        .map(|f| (f.name.clone(), std::sync::Arc::clone(&f.data)))
        .collect::<Vec<_>>();
    worker::run(
        file_entries,
        progress,
        Some(token),
        move |entries, mut report, cancel| async move {
            create_zip_report(entries, stage, &mut report, cancel.as_ref()).await
        },
    )
    .await
}

pub async fn unzip_async<S, P>(
    inner: Vec<u8>,
    progress: P,
    stage: S,
    token: std::sync::Arc<std::sync::atomic::AtomicBool>,
) -> Result<Vec<(String, Vec<u8>)>, Error>
where
    S: Copy + Send + Sync + 'static,
    P: FnMut(Option<Job<S>>) + Send + 'static,
{
    worker::run(
        inner,
        progress,
        Some(token),
        move |bytes, mut report, cancel| async move {
            unzip_report(bytes, stage, &mut report, cancel.as_ref()).await
        },
    )
    .await
}
