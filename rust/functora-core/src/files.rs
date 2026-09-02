use base64::Engine;
use std::collections::HashMap;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::sync::{Arc, LazyLock, Mutex};

static PREVIEW_MEMO: LazyLock<Mutex<HashMap<(String, u64), Preview>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Attachment {
    pub name: String,
    pub data: Arc<[u8]>,
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
    const PREVIEW_LIMIT: usize = 5 * 1024 * 1024;
    if data.len() > PREVIEW_LIMIT {
        return Preview::Download;
    }
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
    PREVIEW_MEMO
        .lock()
        .ok()
        .and_then(|guard| guard.get(&key).cloned())
        .unwrap_or_else(|| {
            let preview = preview(name, data);
            if let Ok(mut guard) = PREVIEW_MEMO.lock() {
                _ = guard.insert(key, preview.clone());
            }
            preview
        })
}

#[must_use]
pub fn preview_key(data: &[u8]) -> u64 {
    let mut hasher = DefaultHasher::new();
    data.hash(&mut hasher);
    hasher.finish()
}

/// Returns the blob object URL held by a renderable media `Preview`, if any.
///
/// Text and download `Preview` variants never hold a blob URL.
#[must_use]
pub fn preview_blob_url(preview: &Preview) -> Option<&str> {
    match preview {
        Preview::Image(url) | Preview::Video(url) | Preview::Audio(url) | Preview::Pdf(url) => {
            url.strip_prefix("blob:").map(|_| url.as_str())
        }
        _ => None,
    }
}

/// Synchronous initial preview used for immediate, delay-free rendering of
/// non-streaming content (text, markdown, unsupported types). Media that must
/// be streamed into a blob object URL (images, video, audio, pdf) returns
/// `None` here; those are prepared asynchronously by `preview_blob` and the
/// caller should show a loading state until the blob URL is ready.
#[must_use]
pub fn preview_initial(name: &str, data: &[u8]) -> Option<Preview> {
    if mime_for(name).is_some_and(|m| {
        m.starts_with("image/")
            || m.starts_with("video/")
            || m.starts_with("audio/")
            || m == "application/pdf"
    }) {
        None
    } else {
        Some(preview(name, data))
    }
}
