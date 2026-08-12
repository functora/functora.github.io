use crate::Error;
use crate::progress::{Job, report_progress, report_progress_named};
use base64::Engine;
use dioxus::prelude::Writable;
use serde::Deserialize;
use std::collections::HashMap;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::sync::{LazyLock, Mutex};

#[cfg(target_os = "android")]
use crate::android::extract;
#[cfg(not(target_os = "android"))]
use crate::web::extract;

#[cfg(not(target_os = "android"))]
pub use crate::web::{download_package, video_thumbnail_script};

static PREVIEW_MEMO: LazyLock<Mutex<HashMap<(String, u64), Preview>>> = LazyLock::new(|| Mutex::new(HashMap::new()));

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Attachment {
    pub name: String,
    pub data: Vec<u8>,
}

#[must_use]
pub fn format_size(size: u64) -> String {
    const KB: u64 = 1024;
    const MB: u64 = KB * 1024;
    if size < KB {
        format!("{size} B")
    } else {
        let (unit, label) = if size >= MB { (MB, "MB") } else { (KB, "KB") };
        let tenths = (u128::from(size) * 10 + u128::from(unit) / 2) / u128::from(unit);
        format!("{}.{} {}", tenths / 10, tenths % 10, label)
    }
}

#[must_use]
pub fn mime_for(name: &str) -> Option<&'static str> {
    let ext = name
        .rsplit_once('.')
        .map(|(_, e)| e.to_ascii_lowercase())
        .unwrap_or_default();
    match ext.as_str() {
        "jpg" | "jpeg" => Some("image/jpeg"),
        "png" => Some("image/png"),
        "gif" => Some("image/gif"),
        "webp" => Some("image/webp"),
        "bmp" => Some("image/bmp"),
        "svg" => Some("image/svg+xml"),
        "avif" => Some("image/avif"),
        "ico" => Some("image/x-icon"),
        "mp4" => Some("video/mp4"),
        "webm" => Some("video/webm"),
        "mov" => Some("video/quicktime"),
        "ogv" => Some("video/ogg"),
        "m4v" => Some("video/x-m4v"),
        "mp3" => Some("audio/mpeg"),
        "wav" => Some("audio/wav"),
        "ogg" | "oga" => Some("audio/ogg"),
        "m4a" => Some("audio/mp4"),
        "flac" => Some("audio/flac"),
        "aac" => Some("audio/aac"),
        "opus" => Some("audio/opus"),
        "pdf" => Some("application/pdf"),
        "txt" | "log" => Some("text/plain"),
        "md" | "markdown" => Some("text/markdown"),
        "html" | "htm" => Some("text/html"),
        "css" => Some("text/css"),
        "csv" => Some("text/csv"),
        "json" => Some("application/json"),
        "xml" => Some("application/xml"),
        "toml" => Some("application/toml"),
        "yaml" | "yml" => Some("application/yaml"),
        _ => None,
    }
}

#[must_use]
pub fn is_text(mime: &str) -> bool {
    mime.starts_with("text/")
        || matches!(
            mime,
            "application/json" | "application/xml" | "application/toml" | "application/yaml"
        )
}

#[derive(Debug, Clone, PartialEq)]
pub enum Preview {
    Image(String),
    Video(String),
    Audio(String),
    Pdf(String),
    Markdown(String),
    Text(String),
    Download,
    Missing,
}

#[must_use]
pub fn preview(name: &str, data: &[u8]) -> Preview {
    let url = |mime: &str| {
        format!(
            "data:{mime};base64,{}",
            base64::engine::general_purpose::STANDARD.encode(data)
        )
    };
    match mime_for(name) {
        Some(mime) if mime.starts_with("image/") => Preview::Image(url(mime)),
        Some(mime) if mime.starts_with("video/") => Preview::Video(url(mime)),
        Some(mime) if mime.starts_with("audio/") => Preview::Audio(url(mime)),
        Some("application/pdf") => Preview::Pdf(url("application/pdf")),
        Some(mime) if is_text(mime) => match String::from_utf8(data.to_vec()) {
            Ok(text) if mime == "text/markdown" => Preview::Markdown(text),
            Ok(text) => Preview::Text(text),
            Err(e) => {
                tracing::warn!("Preview text decode failed: {e}");
                Preview::Download
            }
        },
        _ => Preview::Download,
    }
}

#[must_use]
pub fn preview_cached(name: &str, data: &[u8]) -> Preview {
    let key = (name.to_string(), preview_key(data));
    if let Some(preview) = PREVIEW_MEMO.lock().ok().and_then(|guard| guard.get(&key).cloned()) {
        return preview;
    }
    let preview = preview(name, data);
    if let Ok(mut guard) = PREVIEW_MEMO.lock() {
        _ = guard.insert(key, preview.clone());
    }
    preview
}

fn preview_key(data: &[u8]) -> u64 {
    let mut hasher = DefaultHasher::new();
    data.hash(&mut hasher);
    hasher.finish()
}

#[must_use]
pub fn pick_script(multiple: bool) -> String {
    format!(
        r"
        (async function() {{
            const CHUNK = 2 * 1024 * 1024;
            const input = document.createElement('input');
            input.type = 'file';
            input.multiple = {multiple};
            input.style.display = 'none';
            document.body.appendChild(input);
            const toBase64 = (uint8) => {{
                let bin = '';
                for (let i = 0; i < uint8.length; i += 0x8000) {{
                    bin += String.fromCharCode.apply(null, uint8.subarray(i, i + 0x8000));
                }}
                return btoa(bin);
            }};
            const done = () => dioxus.send({{ t: 'done' }});
            try {{
                const files = await new Promise((resolve) => {{
                    const timer = setTimeout(() => {{ input.remove(); resolve([]); }}, 120000);
                    input.addEventListener('change', () => {{
                        clearTimeout(timer);
                        resolve(Array.from(input.files));
                    }}, {{ once: true }});
                    input.click();
                }});
                for (const f of files) {{
                    dioxus.send({{ t: 'begin', name: f.name, size: f.size }});
                    for (let off = 0; off < f.size; off += CHUNK) {{
                        const buf = await f.slice(off, Math.min(off + CHUNK, f.size)).arrayBuffer();
                        dioxus.send({{ t: 'chunk', data: toBase64(new Uint8Array(buf)) }});
                    }}
                }}
                input.remove();
                done();
            }} catch(e) {{
                input.remove();
                done();
            }}
        }})()
        ",
        multiple = if multiple { "true" } else { "false" }
    )
}

pub async fn pick_files<P, S>(multiple: bool, progress: P, stage: S) -> Result<Vec<(String, Vec<u8>)>, Error>
where
    P: Writable<Target = Option<Job<S>>> + Copy + 'static,
    S: Copy + 'static,
{
    use base64::engine::general_purpose::STANDARD as BASE64;

    #[derive(Deserialize)]
    #[serde(tag = "t", rename_all = "lowercase")]
    enum PickMsg {
        Begin { name: String, size: u64 },
        Chunk { data: String },
        Done,
    }

    let mut eval = dioxus::document::eval(&pick_script(multiple));
    let mut files: Vec<(String, Vec<u8>)> = Vec::new();
    let mut done = 0u64;
    let mut total = 0u64;
    loop {
        let msg = eval.recv::<PickMsg>().await?;
        match msg {
            PickMsg::Begin { name, size } => {
                total += size;
                files.push((name, Vec::new()));
                done = files.iter().map(|(_, b)| b.len() as u64).sum();
            }
            PickMsg::Chunk { data } => {
                if let Some((_, buf)) = files.last_mut() {
                    match BASE64.decode(&data) {
                        Ok(bytes) => {
                            done += bytes.len() as u64;
                            buf.extend(bytes);
                        }
                        Err(e) => {
                            tracing::warn!("File decode error: {e}");
                            _ = files.pop();
                        }
                    }
                }
            }
            PickMsg::Done => break,
        }
        match files.last() {
            Some((name, _)) => report_progress_named(progress, stage, done, total, name).await,
            None => report_progress(progress, stage, done, total).await,
        }
    }
    Ok(files)
}
pub async fn video_thumbnail(url: &str) -> Option<String> {
    let src = match crate::thumbnail::cached_thumbnail(url) {
        Some(src) => src,
        None => extract(url).await,
    };
    crate::thumbnail::cache_thumbnail(url, src.clone());
    src
}
