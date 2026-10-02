use functora_egui::snippet;
use functora_egui::{
    Badge, Button, ButtonVariant, Card, Flex, Input, ShadcnThemeExt, Switch, ToastVariant,
    Typography,
};

impl crate::state::ShowcaseApp {
    pub fn demo_qr_scanner(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted(
            "QrScanner widget: stateful live preview (TextureHandle) + decode_qr_luma/rgba (rxing). Web live via canvas, Android Camera2, desktop file-picker fallback. Opt-in features `camera` + `qr`.",
        )
        .show(ui);
        ui.add_space(12.0);
        _ = ui.add(Input::new(&mut self.platform.qr_input).placeholder("https://example.com"));
        ui.add_space(4.0);
        if ui
            .add(Button::new("Clear").variant(ButtonVariant::Outline))
            .clicked()
        {
            self.platform.qr_state.clear_decoded();
            self.platform.qr_state.clear_error();
            self.platform.qr_last_scan.clear();
            self.platform.qr_error_notified = None;
        }
        ui.add_space(8.0);
        _ = Typography::small("Scan target (test fixture): point the scanner below at this code.")
            .show(ui);
        ui.add_space(4.0);
        let generated = self.platform.qr_input.clone();
        _ = functora_egui::QrImage::new(&generated).show(ui);
        ui.add_space(4.0);
        _ = Typography::small(format!(
            "Preview content (scan target): {} chars",
            generated.len()
        ))
        .show(ui);
        ui.add_space(12.0);
        _ = Card::new().show(ui, |ui2| {
            _ = Typography::small(
                "Auto-starts and scans automatically (10 fps preview, 5 fps decode).",
            )
            .show(ui2);
            ui2.add_space(4.0);
            _ = Flex::row().gap(8.0).show(ui2, |f2| {
                _ = f2.add(Switch::new(&mut self.platform.qr_continuous).label("Continuous"));
            });
            ui2.add_space(4.0);
            if ui2
                .add(Button::new("Restart scanner").variant(ButtonVariant::Outline))
                .clicked()
            {
                self.platform.qr_state.stop();
                self.platform.qr_state.clear_decoded();
                self.platform.qr_state.clear_error();
                self.platform.qr_last_scan.clear();
                self.platform.qr_error_notified = None;
                let ctx = ui2.ctx().clone();
                let _ = self.platform.qr_state.start(&ctx);
            }
            ui2.add_space(8.0);
            let _ = functora_egui::QrScanner::new()
                .continuous(self.platform.qr_continuous)
                .on_scan(|text| log::info!("QR scanned: {text}"))
                .show(ui2, &mut self.platform.qr_state);
            if let Some(text) = self.platform.qr_state.take_decoded() {
                self.platform.qr_last_scan.clone_from(&text);
                self.toast.add(
                    format!("Scanned: {text}"),
                    ToastVariant::Success,
                    ui2.ctx().input(|i| i.time),
                );
            }
            if let Some(err) = self.platform.qr_state.error() {
                let msg = err.to_string();
                if self.platform.qr_error_notified.as_ref() != Some(&msg) {
                    self.platform.qr_error_notified = Some(msg.clone());
                    self.toast.add(
                        format!("Scan error: {msg}"),
                        ToastVariant::Error,
                        ui2.ctx().input(|i| i.time),
                    );
                }
                ui2.add_space(8.0);
                _ = ui2.label(
                    egui::RichText::new(format!("Error: {err}"))
                        .color(ui2.ctx().shadcn_theme().destructive)
                        .size(12.0),
                );
            } else {
                self.platform.qr_error_notified = None;
            }
            if !self.platform.qr_last_scan.is_empty() {
                ui2.add_space(8.0);
                _ = ui2.add(Badge::new(format!(
                    "Scan action: {}",
                    self.platform.qr_last_scan.clone()
                )));
            }
        });
        ui.add_space(8.0);
        _ = Typography::small("Tip: Use Pick Image inside the scanner for file fallback (desktop) or Start Camera for live (web/android).").show(ui);

        snippet(
            ui,
            "// QrScanner: stateful live preview + auto-scan\nuse functora_egui::{QrImage, QrScanner, QrScannerState};\n\n// State (persist across frames)\nlet mut qr_state = QrScannerState::new();\nlet mut last_scan = String::new();\nlet mut error_notified: Option<String> = None;\n\n// Generated QR preview (call every frame)\nQrImage::new(&qr_input).show(ui);\n\n// Start scanner (call once or on button)\nqr_state.start(&ctx)?;\n\n// Render widget (call every frame)\nQrScanner::new()\n    .continuous(true)           // keep scanning after first decode\n    .on_scan(|text| {           // callback on decode\n        log::info!(\"QR: {}\", text);\n    })\n    .show(ui, &mut qr_state);\n\n// Scan action: take + act once (cryptonote home_scan pattern)\nif let Some(text) = qr_state.take_decoded() {\n    last_scan = text.clone();\n}\n\n// Error toast once per distinct message\nif let Some(err) = qr_state.error() {\n    let msg = err.to_string();\n    if error_notified.as_ref() != Some(&msg) {\n        error_notified = Some(msg);\n    }\n} else {\n    error_notified = None;\n}\n\n// Stop when done\nqr_state.stop();",
        );
    }
}
