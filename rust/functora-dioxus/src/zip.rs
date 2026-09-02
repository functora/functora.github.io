pub use functora_core::zip::{create_zip_report, unzip_report};

use crate::Error;
use crate::files::Attachment;
use crate::progress::Job;
use crate::worker::run;
use dioxus::prelude::Writable;

pub async fn create_zip_async<P, S>(files: &[Attachment], progress: P, stage: S) -> Result<Vec<u8>, Error>
where
    P: Writable<Target = Option<Job<S>>> + 'static,
    S: Copy + Send + Sync + 'static,
{
    let file_entries = files
        .iter()
        .map(|f| (f.name.clone(), f.data.to_vec()))
        .collect::<Vec<_>>();
    run(file_entries, progress, move |entries, mut report| async move {
        create_zip_report(entries, stage, &mut report).await
    })
    .await
}

pub async fn unzip_async<P, S>(inner: Vec<u8>, progress: P, stage: S) -> Result<Vec<(String, Vec<u8>)>, Error>
where
    P: Writable<Target = Option<Job<S>>> + 'static,
    S: Copy + Send + Sync + 'static,
{
    run(inner, progress, move |bytes, mut report| async move {
        unzip_report(bytes, stage, &mut report).await
    })
    .await
}
