use functora_egui::snippet;
use functora_egui::{Button, ButtonVariant, Card, Flex, Input, Typography};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_encoding(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted(
            "Encoding: encode_payload/decode_payload (base64url JSON), append/extract_query_param, generate_qr_code (svg). Crypto via functora_core.",
        )
        .show(ui);
        ui.add_space(12.0);
        _ = ui.add(
            Input::new(&mut self.platform.encode_input)
                .placeholder("text to encode")
                .desired_width(ui.available_width()),
        );
        ui.add_space(8.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            if f.add(Button::new("Encode").icon(functora_egui::LucideIcon::Code))
                .inner
                .clicked()
            {
                #[derive(serde::Serialize)]
                struct Payload {
                    msg: String,
                }
                let v = Payload {
                    msg: self.platform.encode_input.clone(),
                };
                match functora_egui::encoding::encode_payload(&v) {
                    Ok(s) => self.platform.encode_output = s,
                    Err(e) => self.platform.encode_output = format!("encode err: {e}"),
                }
            }
            if f.add(Button::new("Decode").variant(ButtonVariant::Outline))
                .inner
                .clicked()
            {
                match functora_egui::encoding::decode_payload::<serde_json::Value>(
                    &self.platform.encode_output,
                ) {
                    Ok(v) => self.platform.encode_output = format!("decoded: {v}"),
                    Err(e) => self.platform.encode_output = format!("decode err: {e}"),
                }
            }
            if f.add(Button::new("QR SVG").variant(ButtonVariant::Outline))
                .inner
                .clicked()
            {
                match functora_egui::encoding::generate_qr_code(&self.platform.encode_input) {
                    Ok(svg) => {
                        self.platform.encode_output =
                            svg.chars().take(300).collect::<String>() + "...";
                    }
                    Err(e) => self.platform.encode_output = format!("qr err: {e}"),
                }
            }
        });
        if !self.platform.encode_output.is_empty() {
            ui.add_space(8.0);
            _ = Card::new().show(ui, |ui2| {
                _ = Typography::small(&self.platform.encode_output).show(ui2);
            });
        }
        ui.add_space(12.0);
        _ = Typography::small(format!(
            "append_query_param: {}",
            functora_egui::encoding::append_query_param("https://example.com", "k", "v")
        ))
        .show(ui);

        snippet(
            ui,
            "// Encoding: base64url JSON + query params + QR SVG\nuse functora_egui::encoding::{encode_payload, decode_payload, generate_qr_code, append_query_param};\nuse serde::{Serialize, Deserialize};\n\n#[derive(Serialize, Deserialize)]\nstruct Payload { msg: String }\n\nlet payload = Payload { msg: \"hello\".to_owned() };\n\n// Encode to base64url JSON\nlet encoded = encode_payload(&payload)?;\n// \"eyJtc2ciOiJoZWxsbyJ9\"\n\n// Decode back\nlet decoded: Payload = decode_payload(&encoded)?;\nassert_eq!(decoded.msg, \"hello\");\n\n// Generate QR code SVG\nlet svg = generate_qr_code(\"https://example.com\")?;\n\n// Append query param\nlet url = append_query_param(\"https://example.com\", \"k\", \"v\");\n// \"https://example.com?k=v\"",
        );
    }
}
