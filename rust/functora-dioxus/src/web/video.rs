use base64::Engine;
use base64::engine::general_purpose::STANDARD as BASE64;
use serde::{Deserialize, Serialize};

#[derive(Serialize)]
struct ThumbnailMsg {
    t: &'static str,
    data: String,
}

#[derive(Deserialize)]
#[serde(tag = "t", rename_all = "lowercase")]
enum ThumbnailReply {
    Ok { data: String },
    Fail,
}

pub async fn extract(url: &str) -> Option<String> {
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
