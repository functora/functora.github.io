//! Thumbnail strip for attachment lists: display-constrained image
//! previews (`egui::Image` capped at [`THUMB_SIZE`]) with click-to-open.

use functora_egui::files::{Preview, preview_cached, preview_key};

/// Display size cap for list thumbnails. Full bytes are reused; no
/// downscaling happens here.
pub const THUMB_SIZE: f32 = 64.0;

/// Cache key covering both file name and content, so replacing a file
/// with same-named bytes never reuses a stale preview.
pub type PreviewCacheKey = (String, u64);

#[must_use]
pub fn preview_cache_key(name: &str, data: &[u8]) -> PreviewCacheKey {
    (name.to_string(), preview_key(data))
}

/// Stable `bytes://` URI for an attachment. The filename extension is
/// kept so image loaders route correctly.
#[must_use]
pub fn attachment_uri(name: &str) -> String {
    format!("{}{name}", crate::app::BYTES_URI_PREFIX)
}

/// Renders a clickable thumbnail for image attachments, `None` otherwise.
pub fn show_attachment_thumb(ui: &mut egui::Ui, name: &str, data: &[u8]) -> Option<egui::Response> {
    matches!(preview_cached(name, data), Preview::Image(_)).then(|| {
        ui.add(
            egui::Image::from_bytes(attachment_uri(name), data.to_vec())
                .maintain_aspect_ratio(true)
                .max_width(THUMB_SIZE)
                .max_height(THUMB_SIZE)
                .sense(egui::Sense::click()),
        )
    })
}
