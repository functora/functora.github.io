//! Thumbnail strip for attachment lists: display-constrained image
//! previews (`egui::Image` capped at [`THUMB_SIZE`]) with click-to-open.

use functora_egui::files::{Attachment, Preview, preview_cached};

/// Display size cap for list thumbnails. Full bytes are reused; no
/// downscaling happens here.
pub const THUMB_SIZE: f32 = 64.0;

/// Stable `bytes://` URI for an attachment. The filename extension is
/// kept so image loaders route correctly.
#[must_use]
pub fn attachment_uri(name: &str) -> String {
    format!("{}{name}", crate::app::BYTES_URI_PREFIX)
}

/// Renders a clickable thumbnail for image attachments, `None` otherwise.
pub fn show_attachment_thumb(ui: &mut egui::Ui, att: &Attachment) -> Option<egui::Response> {
    matches!(preview_cached(&att.name, &att.data), Preview::Image(_)).then(|| {
        ui.add(
            egui::Image::from_bytes(attachment_uri(&att.name), std::sync::Arc::clone(&att.data))
                .maintain_aspect_ratio(true)
                .max_width(THUMB_SIZE)
                .max_height(THUMB_SIZE)
                .sense(egui::Sense::click()),
        )
    })
}
