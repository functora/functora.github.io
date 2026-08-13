use crate::Error;
use crate::encoding::download_script;
use crate::progress::{Job, report_progress};
use base64::Engine;
use base64::engine::general_purpose::STANDARD as BASE64;
use dioxus::prelude::Writable;
use serde::Serialize;

#[derive(Serialize)]
struct DownloadMsg {
    t: &'static str,
    data: String,
}

pub async fn download_package<P, S, D>(data: D, filename: &str, progress: P, stage: S) -> Result<String, Error>
where
    P: Writable<Target = Option<Job<S>>> + Copy + 'static,
    S: Copy + 'static,
    D: AsRef<[u8]>,
{
    const SEND_CHUNK: usize = 3 * 1024 * 1024;
    let eval = dioxus::document::eval(&download_script(filename));
    let bytes = data.as_ref();
    let total = bytes.len() as u64;
    let mut done = 0u64;
    for chunk in bytes.chunks(SEND_CHUNK) {
        eval.send(DownloadMsg {
            t: "chunk",
            data: BASE64.encode(chunk),
        })?;
        done += chunk.len() as u64;
        report_progress(progress, stage, done, total).await;
    }
    eval.send(DownloadMsg {
        t: "done",
        data: String::new(),
    })?;
    Ok(filename.to_string())
}
