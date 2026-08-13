use crate::abort::EvalAbort;
use base64::Engine;
use base64::engine::general_purpose::STANDARD as BASE64;
use serde::{Deserialize, Serialize};

/// Extracts the MIME type from a `data:` URL prefix, dropping the `;base64`
/// parameter (and any other parameters) so the payload can be used as a `Blob`
/// type, which rejects parameters it does not know.
#[must_use]
pub fn data_url_mime(prefix: &str) -> Option<&str> {
    prefix
        .strip_prefix("data:")
        .and_then(|rest| rest.split(';').next())
        .filter(|mime| !mime.is_empty())
}

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
    let mime = data_url_mime(prefix)?;
    let bytes = BASE64
        .decode(payload)
        .inspect_err(|e| {
            tracing::warn!("Video preview base64 decode failed: {e}");
        })
        .ok()?;
    let mut eval = dioxus::document::eval(&video_thumbnail_script());
    let abort = EvalAbort::new(eval, serde_json::json!({ "t": "abort" }));
    let send = |msg: ThumbnailMsg| {
        eval.send(msg)
            .inspect_err(|e| {
                tracing::warn!("Video preview send failed: {e}");
            })
            .ok()
    };
    send(ThumbnailMsg {
        t: "begin",
        data: mime.to_string(),
    })?;
    bytes.chunks(SEND_CHUNK).try_for_each(|chunk| {
        send(ThumbnailMsg {
            t: "chunk",
            data: BASE64.encode(chunk),
        })
    })?;
    send(ThumbnailMsg {
        t: "done",
        data: String::new(),
    })?;
    let reply = eval.recv::<ThumbnailReply>().await;
    abort.disarm();
    match reply {
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
    const MAX_W = 360;
    const MAX_H = 240;
    let url = null;
    try {
        const begin = await dioxus.recv();
        const parts = [];
        for (;;) {
            const m = await dioxus.recv();
            if (m && m.t === 'done') {
                break;
            }
            if (m && m.t === 'abort') {
                return;
            }
            const bin = atob(m.data);
            const bytes = new Uint8Array(bin.length);
            for (let i = 0; i < bin.length; i++) {
                bytes[i] = bin.charCodeAt(i);
            }
            parts.push(bytes);
        }
        url = URL.createObjectURL(new Blob(parts, {type: begin.data}));
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
        if (url) URL.revokeObjectURL(url);
        dioxus.send({t: 'fail'});
    }
    "
    .to_string()
}
