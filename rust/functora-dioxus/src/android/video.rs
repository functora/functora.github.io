use base64::Engine;
use base64::engine::general_purpose::STANDARD as BASE64;

pub async fn extract(url: &str) -> Option<String> {
    let (prefix, payload) = url.split_once(',').unwrap_or(("", ""));
    let _ = prefix.strip_prefix("data:").or_else(|| {
        tracing::warn!("Video preview URL has no data MIME prefix");
        None
    })?;
    let bytes = BASE64
        .decode(payload)
        .inspect_err(|e| {
            tracing::warn!("Video preview base64 decode failed: {e}");
        })
        .ok()?;
    crate::thumbnail::video_thumbnail(&bytes)
        .map(crate::thumbnail::jpeg_data_url)
        .or_else(|| {
            tracing::warn!("Video preview extraction produced no frame");
            None
        })
}
