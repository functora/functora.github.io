use functora_egui::snippet;
use functora_egui::{Button, Flex, Input, ToastVariant, Typography, spawn_async};

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
            crate::screens::section(ui, format!("Thumbnail: {} bytes", jpeg.len()));
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
        ui.add_space(12.0);
        crate::screens::section(ui, "Core thumbnail helpers (functora_core::thumbnail)");
        _ = Flex::column().gap(4.0).align_start().show(ui, |f| {
            _ = f.ui(|ui2| {
                _ = Typography::small(format!(
                    "jpeg_data_url(3 bytes): {}",
                    functora_egui::thumbnail::jpeg_data_url(vec![1, 2, 3])
                ))
                .show(ui2);
            });
            _ = f.ui(|ui2| {
                _ = Typography::small(format!(
                    "cached_thumbnail(\"demo-url\"): {:?}",
                    functora_egui::thumbnail::cached_thumbnail("demo-url")
                ))
                .show(ui2);
            });
        });
        #[cfg(not(target_arch = "wasm32"))]
        {
            _ = Flex::column().gap(4.0).align_start().show(ui, |f| {
                _ = f.ui(|ui2| {
                    _ = Typography::small(format!(
                        "video_thumbnail(non-mp4 bytes): {:?}",
                        functora_egui::thumbnail::video_thumbnail(b"not-an-mp4")
                    ))
                    .show(ui2);
                });
            });
        }
        ui.add_space(4.0);
        if ui.add(Button::new("Cache thumbnail result")).clicked() {
            functora_egui::thumbnail::cache_thumbnail(
                "demo-url",
                Some("data:image/jpeg;base64,AA==".to_owned()),
            );
            self.toast.add(
                "Cached thumbnail for demo-url",
                ToastVariant::Success,
                ui.ctx().input(|i| i.time),
            );
        }
        #[cfg(not(target_arch = "wasm32"))]
        {
            _ = Typography::small(format!(
                "planar_yuv_to_rgb(2x2): {:?}",
                functora_egui::thumbnail::planar_yuv_to_rgb(
                    2,
                    2,
                    &[16, 235, 16, 235],
                    &[128, 128],
                    &[128, 128]
                )
                .map(|rgb| rgb.len())
            ))
            .show(ui);
        }

        snippet(
            ui,
            "// Thumbnail: files::video_thumbnail (mp4 data URL -> jpeg) + from_bytes display\nuse functora_egui::{spawn_async, files::video_thumbnail};\nuse base64::Engine as _;\n\n// Pure helper (runs inside spawn_async so mp4 decode never blocks paint)\nfn make_thumbnail(url: &str) -> Result<(String, Vec<u8>), String> {\n    let data_url = video_thumbnail(url)\n        .ok_or_else(|| \"No thumbnail available\".to_owned())?;\n    let payload = data_url.split_once(',').map_or(\"\", |(_, rest)| rest);\n    let jpeg = base64::engine::general_purpose::STANDARD\n        .decode(payload)\n        .map_err(|_| \"Thumbnail decode failed\".to_owned())?;\n    Ok((\"bytes://thumb.jpg\".to_owned(), jpeg))\n}\n\nlet url = thumbnail_input.clone();\nthumbnail_rx = Some(spawn_async(async move { make_thumbnail(&url) }));\n\n// Render the stored bytes (bytes:// keeps the extension for loader routing)\nif let Some((uri, jpeg)) = &thumbnail_image {\n    ui.add(\n        egui::Image::from_bytes(uri.clone(), jpeg.clone())\n            .maintain_aspect_ratio(true)\n            .max_height(240.0),\n    );\n}\n\n// Core thumbnail API: decode an mp4 to JPEG, wrap it as a data URL and memoize it\nuse functora_egui::thumbnail::{cache_thumbnail, cached_thumbnail, jpeg_data_url, planar_yuv_to_rgb, video_thumbnail};\n\nlet jpeg = video_thumbnail(&mp4_bytes);        // Option<Vec<u8>> (native only)\nlet data_url = jpeg.map(jpeg_data_url);        // \"data:image/jpeg;base64,...\"\n\ncache_thumbnail(\"demo-url\", data_url.clone());\nlet memoized = cached_thumbnail(\"demo-url\");   // Option<Option<String>>\n\n// Planar YUV -> packed RGB (native only)\nlet rgb = planar_yuv_to_rgb(width, height, &y_plane, &u_plane, &v_plane);",
        );
    }
}
