use crate::Error;
use crate::progress::{Job, Stage, report_progress, report_progress_named};
use base64::Engine;
use base64::engine::general_purpose::STANDARD as BASE64;
use dioxus::prelude::Writable;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::sync::{Arc, LazyLock, Mutex};

#[cfg(target_os = "android")]
use crate::android::extract;
#[cfg(not(target_os = "android"))]
use crate::web::extract;

#[cfg(not(target_os = "android"))]
pub use crate::web::{download_package, video_thumbnail_script};

static PREVIEW_MEMO: LazyLock<Mutex<HashMap<(String, u64), Preview>>> = LazyLock::new(|| Mutex::new(HashMap::new()));

/// Session cache of live blob object URLs, keyed by attachment identity. A blob URL
/// keeps a reference to its underlying `Blob` in the `WebView` until revoked, so the
/// cache is bounded by the number of distinct attachments the user opens.
#[derive(Debug, Default)]
pub struct BlobMemo {
    entries: HashMap<(String, u64), String>,
}

impl BlobMemo {
    #[must_use]
    pub fn get(&self, name: &str, data_key: u64) -> Option<&str> {
        self.entries.get(&(name.to_string(), data_key)).map(String::as_str)
    }

    pub fn insert(&mut self, name: &str, data_key: u64, url: String) {
        _ = self.entries.insert((name.to_string(), data_key), url);
    }

    /// Removes every entry holding `url`; returns how many were removed.
    #[must_use]
    pub fn forget(&mut self, url: &str) -> usize {
        let before = self.entries.len();
        self.entries.retain(|_, cached| cached.as_str() != url);
        before - self.entries.len()
    }
}

static BLOB_URL_MEMO: LazyLock<Mutex<BlobMemo>> = LazyLock::new(|| Mutex::new(BlobMemo::default()));

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
        m.starts_with("image/") || m.starts_with("video/") || m.starts_with("audio/") || m == "application/pdf"
    }) {
        None
    } else {
        Some(preview(name, data))
    }
}

/// JavaScript that reassembles streamed base64 chunks into a `Blob` and sends back an
/// object URL. Streaming avoids embedding the whole file as a `data:` URI (which is
/// slow and fails on Android for media), and using a local blob URL instead of an HTTP
/// endpoint keeps the app serverless and offline. The script sends `{ ok: true, url }`
/// or `{ ok: false, error }` so the underlying cause is never lost. After every chunk it
/// sends `{ t: 'ack' }`, which lets the Rust side apply backpressure and keeps at most one
/// base64 chunk in flight instead of queueing the whole file in the eval channel. The script
/// must be a plain async function body, not an IIFE: the desktop evaluator wraps it in an
/// `AsyncFunction` whose promise resolution triggers the channel `close()`, and an IIFE
/// resolves that promise immediately, after which garbage collection tears the eval
/// down mid-stream, surfacing as `EvalError::Finished` on Android.
#[must_use]
pub fn blob_url_script() -> String {
    r"
    let mime = '';
    const parts = [];
    try {
        for (;;) {
            const m = await dioxus.recv();
            if (m && m.t === 'done') break;
            if (m.t === 'mime') {
                mime = m.data;
            } else if (m.t === 'chunk') {
                const bin = atob(m.data);
                const bytes = new Uint8Array(bin.length);
                for (let i = 0; i < bin.length; i++) {
                    bytes[i] = bin.charCodeAt(i);
                }
                parts.push(bytes);
                dioxus.send({ t: 'ack' });
            }
        }
        const blob = new Blob(parts, { type: mime });
        const url = URL.createObjectURL(blob);
        dioxus.send({ ok: true, url });
    } catch (e) {
        const msg = (e && e.message) ? e.message : String(e);
        dioxus.send({ ok: false, error: msg });
    }
    "
    .to_string()
}

#[derive(Serialize, Deserialize)]
struct BlobMsg {
    t: &'static str,
    data: String,
}

#[derive(Deserialize)]
#[serde(tag = "t", rename_all = "lowercase")]
enum BlobAck {
    Ack,
}

#[derive(Deserialize)]
struct BlobResult {
    ok: bool,
    #[serde(default)]
    url: Option<String>,
    #[serde(default)]
    error: Option<String>,
}

const BLOB_CHUNK: usize = 2 * 1024 * 1024;

/// Streams `data` into the `WebView`, reassembles it as a `Blob`, and returns a `blob:`
/// object URL. Streaming chunks keeps large media out of the DOM as a single huge
/// base64 `data:` URI, which is the fix for video/audio not playing on Android and for
/// slow attachment handling everywhere.
pub async fn create_blob_url<P>(mime: &str, data: &[u8], progress: P) -> Result<String, Error>
where
    P: Writable<Target = Option<Job<Stage>>> + Copy + 'static,
{
    let mut eval = dioxus::document::eval(&blob_url_script());
    let total = data.len() as u64;
    if let Err(e) = eval.send(BlobMsg {
        t: "mime",
        data: mime.to_string(),
    }) {
        tracing::warn!("Blob URL mime send failed: {e}");
        return Err(Error::from(e));
    }
    let mut done = 0u64;
    for chunk in data.chunks(BLOB_CHUNK) {
        if let Err(e) = eval.send(BlobMsg {
            t: "chunk",
            data: BASE64.encode(chunk),
        }) {
            tracing::warn!("Blob URL chunk send failed: {e}");
            return Err(Error::from(e));
        }
        done += chunk.len() as u64;
        report_progress(progress, Stage::Preview, done, total).await;
        if let Err(e) = eval.recv::<BlobAck>().await {
            tracing::warn!("Blob URL chunk ack failed: {e}");
            return Err(Error::from(e));
        }
    }
    if let Err(e) = eval.send(BlobMsg {
        t: "done",
        data: String::new(),
    }) {
        tracing::warn!("Blob URL done send failed: {e}");
        return Err(Error::from(e));
    }
    match eval.recv::<BlobResult>().await {
        Ok(r) if r.ok => r.url.ok_or_else(|| Error::JS("Blob URL result missing url".into())),
        Ok(r) => Err(Error::JS(format!(
            "Blob URL creation failed: {}",
            r.error.unwrap_or_else(|| "unknown js error".into())
        ))),
        Err(e) => Err(Error::from(e)),
    }
}

/// Releases a blob object URL previously created by `create_blob_url`, freeing the
/// underlying `Blob` held by the `WebView`. Also evicts the URL from the memo so a
/// revoked URL is never served again. Always call this when the preview is no longer
/// shown. This is fire-and-forget: the revoke script is dispatched but not awaited,
/// mirroring `download`, so it never races the eval teardown.
pub fn revoke_blob_url(url: &str) -> Result<(), Error> {
    if let Ok(mut guard) = BLOB_URL_MEMO.lock() {
        _ = guard.forget(url);
    }
    let eval = dioxus::document::eval("const u = await dioxus.recv(); URL.revokeObjectURL(u);");
    if let Err(e) = eval.send(url) {
        tracing::warn!("Blob URL revoke send failed: {e}");
        return Err(Error::from(e));
    }
    Ok(())
}

/// Builds a `Preview` for an attachment, preferring streaming `blob:` object URLs for
/// image, audio, video, and PDF media. Text and markdown are still rendered inline.
/// The blob URL is memoized so re-opening the same attachment reuses it; `revoke_blob_url`
/// evicts the memo entry, so a revoked URL is never served on a later open.
pub async fn preview_blob<P>(name: &str, data: &[u8], progress: P) -> Preview
where
    P: Writable<Target = Option<Job<Stage>>> + Copy + 'static,
{
    let Some(mime) = mime_for(name) else {
        return Preview::Download;
    };
    let is_blob = mime.starts_with("image/")
        || mime.starts_with("video/")
        || mime.starts_with("audio/")
        || mime == "application/pdf";
    if !is_blob {
        return preview(name, data);
    }
    let data_key = preview_key(data);
    if let Some(url) = BLOB_URL_MEMO
        .lock()
        .ok()
        .and_then(|guard| guard.get(name, data_key).map(str::to_string))
    {
        return match mime {
            m if m.starts_with("image/") => Preview::Image(url),
            m if m.starts_with("video/") => Preview::Video(url),
            m if m.starts_with("audio/") => Preview::Audio(url),
            _ => Preview::Pdf(url),
        };
    }
    match create_blob_url(mime, data, progress).await {
        Ok(url) => {
            if let Ok(mut guard) = BLOB_URL_MEMO.lock() {
                guard.insert(name, data_key, url.clone());
            }
            match mime {
                m if m.starts_with("image/") => Preview::Image(url),
                m if m.starts_with("video/") => Preview::Video(url),
                m if m.starts_with("audio/") => Preview::Audio(url),
                _ => Preview::Pdf(url),
            }
        }
        Err(e) => {
            tracing::warn!("Blob preview failed, falling back to download: {e}");
            Preview::Download
        }
    }
}

#[must_use]
pub fn pick_script(multiple: bool) -> String {
    format!(
        r"
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
