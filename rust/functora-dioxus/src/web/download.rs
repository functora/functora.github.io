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

pub async fn download_package<P, S>(data: Vec<u8>, filename: &str, progress: P, stage: S) -> Result<String, Error>
where
    P: Writable<Target = Option<Job<S>>> + Copy + 'static,
    S: Copy + 'static,
{
    const SEND_CHUNK: usize = 3 * 1024 * 1024;
    let eval = dioxus::document::eval(&download_script(filename));
    let total = data.len() as u64;
    let mut done = 0u64;
    for chunk in data.chunks(SEND_CHUNK) {
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
