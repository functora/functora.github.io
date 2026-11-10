use functora_egui::snippet;
use functora_egui::{Input, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_qr_image(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted(
            "QrImage widget: renders encoded content as a centered QR card (white frame, cached texture, 480 max side). Share-card pattern from cryptonote: QrImage + readonly URL + share/download.",
        )
        .show(ui);
        ui.add_space(12.0);
        _ = ui
            .add(Input::new(&mut self.platform.qr_image_input).placeholder("https://example.com"));
        ui.add_space(8.0);
        let content = self.platform.qr_image_input.clone();
        _ = functora_egui::QrImage::new(&content).show(ui);
        ui.add_space(8.0);
        match functora_egui::qr::qr_rgba(&content, 256) {
            Some((wide, high, rgba)) => {
                _ = Typography::small(format!(
                    "QR payload: {wide}x{high}, content {} chars",
                    content.len()
                ))
                .show(ui);
                let luma = Self::rgba_to_luma(&rgba);
                _ = Typography::small(format!(
                    "decode_qr_rgba: {:?}",
                    functora_egui::qr::decode_qr_rgba(&rgba, wide, high)
                ))
                .show(ui);
                _ = Typography::small(format!(
                    "decode_qr_luma: {:?}",
                    functora_egui::qr::decode_qr_luma(&luma, wide, high)
                ))
                .show(ui);
                _ = Typography::small(format!(
                    "decode_qr_luma_fast: {:?}",
                    functora_egui::qr::decode_qr_luma_fast(&luma, wide, high)
                ))
                .show(ui);
            }
            None => {
                _ = Typography::small("QR unavailable: content is empty or encoding failed.")
                    .show(ui);
            }
        }
        _ = Typography::small(format!(
            "decode_hints().TryHarder: {:?}",
            functora_egui::qr::decode_hints().TryHarder
        ))
        .show(ui);

        snippet(
            ui,
            "// QrImage: encoded content as a centered QR card\nuse functora_egui::QrImage;\n\n// Render every frame (cached texture keyed by content hash)\nQrImage::new(&url).show(ui);\n\n// Share-card pattern (cryptonote share screen)\nQrImage::new(&url).show(ui);\n// + readonly URL row + share/download buttons\n\n// Payload size without rendering\nif let Some((w, h, rgba)) = functora_egui::qr::qr_rgba(&url, 256) {\n    eprintln!(\"QR {w}x{h} {} bytes\", rgba.len());\n}\n\n// Decode it back (the scanner widgets use the same core decoders)\nuse functora_egui::qr::{decode_hints, decode_qr_luma, decode_qr_luma_fast, decode_qr_rgba};\n\nlet decoded = decode_qr_rgba(&rgba, w, h);   // RGBA input\nlet luma: Vec<u8> = rgba.chunks_exact(4)\n    .map(|px| u8::try_from((306 * u64::from(px[0]) + 601 * u64::from(px[1]) + 117 * u64::from(px[2]) + 0x200) >> 10).unwrap_or(u8::MAX))\n    .collect();\nlet decoded = decode_qr_luma(&luma, w, h);       // thorough decode\nlet decoded = decode_qr_luma_fast(&luma, w, h);  // default hints\nlet hints = decode_hints();                      // TryHarder = true",
        );
    }
}
