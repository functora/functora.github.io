use crate::Error;
#[cfg(not(target_os = "android"))]
use crate::encoding::download_script;
use crate::progress::{Job, report_progress, report_progress_named};
use base64::Engine;
use dioxus::prelude::Writable;
use serde::Deserialize;
#[cfg(not(target_os = "android"))]
use serde::Serialize;

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

#[cfg(not(target_os = "android"))]
#[must_use]
pub fn video_thumbnail_script() -> String {
    r"
    (async function() {
        const MAX_W = 360;
        const MAX_H = 240;
        try {
            const begin = await dioxus.recv();
            const parts = [];
            for (;;) {
                const m = await dioxus.recv();
                if (m && m.t === 'done') {
                    break;
                }
                const bin = atob(m.data);
                const bytes = new Uint8Array(bin.length);
                for (let i = 0; i < bin.length; i++) {
                    bytes[i] = bin.charCodeAt(i);
                }
                parts.push(bytes);
            }
            const url = URL.createObjectURL(new Blob(parts, {type: begin.data}));
            const video = document.createElement('video');
            video.preload = 'metadata';
            video.muted = true;
            video.setAttribute('playsinline', '');
            video.src = url;
            await new Promise((resolve, reject) => {
                video.onloadeddata = resolve;
                video.onerror = () => reject(new Error('video load failed'));
                setTimeout(() => reject(new Error('video load timeout')), 15000);
            });
            video.currentTime = 0;
            await Promise.race([
                new Promise((resolve) => { video.onseeked = resolve; }),
                new Promise((resolve) => setTimeout(resolve, 1500)),
            ]);
            if (!video.videoWidth || !video.videoHeight) {
                throw new Error('video has no dimensions');
            }
            const canvas = document.createElement('canvas');
            const scale = Math.min(MAX_W / video.videoWidth, MAX_H / video.videoHeight, 1);
            canvas.width = Math.max(1, Math.round(video.videoWidth * scale));
            canvas.height = Math.max(1, Math.round(video.videoHeight * scale));
            canvas.getContext('2d').drawImage(video, 0, 0, canvas.width, canvas.height);
            const data = canvas.toDataURL('image/jpeg', 0.7);
            video.removeAttribute('src');
            video.load();
            URL.revokeObjectURL(url);
            dioxus.send({t: 'ok', data: data});
        } catch (e) {
            dioxus.send({t: 'fail'});
        }
    })()
    "
    .to_string()
}

#[cfg(not(target_os = "android"))]
#[derive(Serialize)]
struct ThumbnailMsg {
    t: &'static str,
    data: String,
}

#[cfg(not(target_os = "android"))]
#[derive(Deserialize)]
#[serde(tag = "t", rename_all = "lowercase")]
enum ThumbnailReply {
    Ok { data: String },
    Fail,
}

#[cfg(not(target_os = "android"))]
pub async fn video_thumbnail(url: &str) -> Option<String> {
    use base64::engine::general_purpose::STANDARD as BASE64;
    const SEND_CHUNK: usize = 2 * 1024 * 1024;
    let (prefix, payload) = url.split_once(',').unwrap_or(("", ""));
    let Some(mime) = prefix.strip_prefix("data:") else {
        tracing::warn!("Video preview URL has no data MIME prefix");
        return None;
    };
    let bytes = match BASE64.decode(payload) {
        Ok(bytes) => bytes,
        Err(e) => {
            tracing::warn!("Video preview base64 decode failed: {e}");
            return None;
        }
    };
    let mut eval = dioxus::document::eval(&video_thumbnail_script());
    if let Err(e) = eval.send(ThumbnailMsg {
        t: "begin",
        data: mime.to_string(),
    }) {
        tracing::warn!("Video preview send failed: {e}");
        return None;
    }
    for chunk in bytes.chunks(SEND_CHUNK) {
        if let Err(e) = eval.send(ThumbnailMsg {
            t: "chunk",
            data: BASE64.encode(chunk),
        }) {
            tracing::warn!("Video preview chunk send failed: {e}");
            return None;
        }
    }
    if let Err(e) = eval.send(ThumbnailMsg {
        t: "done",
        data: String::new(),
    }) {
        tracing::warn!("Video preview done send failed: {e}");
        return None;
    }
    match eval.recv::<ThumbnailReply>().await {
        Ok(ThumbnailReply::Ok { data }) => Some(data),
        Ok(ThumbnailReply::Fail) => {
            tracing::warn!("Video preview extraction failed");
            None
        }
        Err(e) => {
            tracing::warn!("Video preview receive failed: {e}");
            None
        }
    }
}

#[cfg(target_os = "android")]
pub async fn video_thumbnail(url: &str) -> Option<String> {
    use base64::engine::general_purpose::STANDARD as BASE64;
    let (prefix, payload) = url.split_once(',').unwrap_or(("", ""));
    if !prefix.starts_with("data:") {
        tracing::warn!("Video preview URL has no data MIME prefix");
        return None;
    }
    let bytes = match BASE64.decode(payload) {
        Ok(bytes) => bytes,
        Err(e) => {
            tracing::warn!("Video preview base64 decode failed: {e}");
            return None;
        }
    };
    match crate::thumbnail::video_thumbnail(&bytes) {
        Some(jpeg) => Some(crate::thumbnail::jpeg_data_url(jpeg)),
        None => {
            tracing::warn!("Video preview extraction produced no frame");
            None
        }
    }
}

#[cfg(not(target_os = "android"))]
#[derive(Serialize)]
struct DownloadMsg {
    t: &'static str,
    data: String,
}

#[cfg(not(target_os = "android"))]
pub async fn download_package<P, S>(data: Vec<u8>, filename: &str, progress: P, stage: S) -> Result<String, Error>
where
    P: Writable<Target = Option<Job<S>>> + Copy + 'static,
    S: Copy + 'static,
{
    use base64::engine::general_purpose::STANDARD as BASE64;
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
