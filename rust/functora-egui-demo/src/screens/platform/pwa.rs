use functora_egui::snippet;
use functora_egui::{Button, ButtonVariant, Card, Flex, Label, Typography, spawn_async};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_pwa(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted(
            "PWA: pwa_init_js, pwa_sw_js, trigger_pwa_install, install_hint. Manifest/theme_color derived from Cargo.toml.",
        )
        .show(ui);
        ui.add_space(12.0);
        _ = Card::new().show(ui, |ui2| {
            _ = Typography::small("Generated pwa_init_js:").show(ui2);
            ui2.add_space(4.0);
            _ = Label::new(functora_egui::pwa::pwa_init_js("/sw.js", "demo-v1")).show(ui2);
        });
        ui.add_space(8.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            let pending = self.platform.pwa_rx.is_some();
            if f.add(
                Button::new("Trigger PWA install")
                    .icon(functora_egui::LucideIcon::Download)
                    .enabled(!pending),
            )
            .inner
            .clicked()
            {
                self.platform.pwa_rx = Some(spawn_async(async move {
                    let res = functora_egui::pwa::trigger_pwa_install()
                        .await
                        .map_err(|e| e.to_string())?;
                    Ok(format!("Install: {res:?}"))
                }));
            }
            if f.add(
                Button::new("Install hint")
                    .variant(ButtonVariant::Outline)
                    .enabled(!pending),
            )
            .inner
            .clicked()
            {
                self.platform.pwa_rx = Some(spawn_async(async move {
                    let hint = functora_egui::pwa::install_hint()
                        .await
                        .map_err(|e| e.to_string())?;
                    Ok(format!("Hint: {hint:?}"))
                }));
            }
        });
        ui.add_space(8.0);
        _ = Typography::small(
            "On desktop this will be NotAvailable - expected. On web with beforeinstallprompt it may be Accepted/Rejected.",
        )
        .show(ui);

        snippet(
            ui,
            "// PWA: install_hint + trigger_pwa_install\nuse functora_egui::{pwa::install_hint, pwa::trigger_pwa_install};\n\n// Check if install is available\nlet hint = install_hint().await?;\nmatch hint {\n    functora_egui::pwa::InstallHint::Ios => {\n        // Show install button\n    }\n    functora_egui::pwa::InstallHint::Mac => {\n        // Hide install button\n    }\n    functora_egui::pwa::InstallHint::Unavailable => {}\n}\n\n// Trigger install prompt\nlet res = trigger_pwa_install().await?;\n// res = Accepted | Rejected | NotAvailable",
        );
    }
}
