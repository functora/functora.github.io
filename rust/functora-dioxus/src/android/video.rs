use base64::Engine;
use base64::engine::general_purpose::STANDARD as BASE64;

pub async fn extract(url: &str) -> Option<String> {
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
    let src = crate::thumbnail::video_thumbnail(&bytes).map(crate::thumbnail::jpeg_data_url);
    if src.is_none() {
        tracing::warn!("Video preview extraction produced no frame");
    }
    src
}
