use functora_egui::snippet;
use functora_egui::{Button, Flex, Input, Typography, spawn_async};

impl crate::state::ShowcaseApp {
    pub fn demo_thumbnail(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted(
            "Thumbnail: files::video_thumbnail (mp4 data URL -> jpeg data URL) + cache. Native decodes via mp4+rust_h264; web reports unavailable.",
        )
        .show(ui);
        ui.add_space(12.0);
        _ = ui.add(
            Input::new(&mut self.platform.thumbnail_input)
                .placeholder("data:video/mp4;base64,... or data:image/..."),
        );
        ui.add_space(8.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            let busy = self.platform.thumbnail_rx.is_some();
            if f.add(
                Button::new(if busy {
                    "Generating..."
                } else {
                    "Generate thumbnail"
                })
                .icon(functora_egui::LucideIcon::Image)
                .enabled(!busy),
            )
            .inner
            .clicked()
            {
                let url = self.platform.thumbnail_input.clone();
                self.platform.thumbnail_rx =
                    Some(spawn_async(async move { Self::make_thumbnail(&url) }));
            }
        });
        ui.add_space(8.0);
        if let Some((uri, jpeg)) = self.platform.thumbnail_image.clone() {
            _ = Typography::small(format!("Thumbnail: {} bytes", jpeg.len())).show(ui);
            ui.add_space(4.0);
            _ = ui.add(
                egui::Image::from_bytes(uri, jpeg)
                    .maintain_aspect_ratio(true)
                    .max_height(240.0),
            );
            ui.add_space(4.0);
        }
        _ = Typography::small(
            "Tip: pick a video file in Files demo, then paste its data URL here. Non-video input reports an honest error.",
        )
        .show(ui);

        snippet(
            ui,
            "// Thumbnail: files::video_thumbnail (mp4 data URL -> jpeg) + from_bytes display\nuse functora_egui::{spawn_async, files::video_thumbnail};\n\n// Pure helper (runs inside spawn_async so mp4 decode never blocks paint)\nfn make_thumbnail(url: &str) -> Result<(String, Vec<u8>), String> {\n    let data_url = video_thumbnail(url)\n        .ok_or_else(|| \"No thumbnail available\".to_owned())?;\n    let payload = data_url.split_once(',').map(|(_, rest)| rest).unwrap_or(\"\");\n    let jpeg = base64_decode(payload)?;\n    Ok((\"bytes://thumb.jpg\".to_owned(), jpeg))\n}\n\nlet url = thumbnail_input.clone();\nthumbnail_rx = Some(spawn_async(async move { make_thumbnail(&url) }));\n\n// Render the stored bytes (bytes:// keeps the extension for loader routing)\nif let Some((uri, jpeg)) = &thumbnail_image {\n    ui.add(\n        egui::Image::from_bytes(uri.clone(), jpeg.clone())\n            .maintain_aspect_ratio(true)\n            .max_height(240.0),\n    );\n}",
        );
    }
}
