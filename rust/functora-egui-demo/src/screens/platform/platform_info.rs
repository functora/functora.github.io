use functora_egui::snippet;
use functora_egui::{Button, Card, Input, ResponsiveExt, ToastVariant, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_platform_info(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted(
            "Platform info: is_mobile_hint (web innerWidth), location_href/hash, storage files_dir, theme, breakpoint.",
        )
        .show(ui);
        ui.add_space(12.0);
        let is_mobile = ui.ctx().on_mobile();
        let spacing = ui.ctx().responsive_spacing();
        _ = Card::new().show(ui, |ui2| {
            _ = Typography::small(format!("on_mobile: {is_mobile}")).show(ui2);
            _ = Typography::small(format!("breakpoint: {:?}", ui2.ctx().breakpoint())).show(ui2);
            _ = Typography::small(format!(
                "spacing content_max_width: {}",
                spacing.content_max_width
            ))
            .show(ui2);
            _ = Typography::small(format!("spacing page_padding: {}", spacing.page_padding))
                .show(ui2);
            _ = Typography::small(format!(
                "current_theme: {}",
                functora_egui::current_theme(ui2.ctx())
            ))
            .show(ui2);
            #[cfg(target_arch = "wasm32")]
            {
                if let Some(hint) = functora_egui::platform::web::is_mobile_hint() {
                    _ = Typography::small(format!("is_mobile_hint: {hint}")).show(ui2);
                }
                if let Some(href) = functora_egui::platform::web::location_href() {
                    _ = Typography::small(format!("location_href: {href}")).show(ui2);
                }
                if let Some(hash) = functora_egui::platform::web::location_hash() {
                    _ = Typography::small(format!("location_hash: {hash}")).show(ui2);
                }
            }
            #[cfg(not(target_arch = "wasm32"))]
            {
                _ = Typography::small("location_href/hash only on web").show(ui2);
            }
            match functora_egui::storage::files_dir() {
                Ok(p) => _ = Typography::small(format!("files_dir: {}", p.display())).show(ui2),
                Err(e) => _ = Typography::small(format!("files_dir err: {e}")).show(ui2),
            }
            if let Some(v) = functora_egui::storage::load_state::<String>("platform_info") {
                _ = Typography::small(format!("platform_info: {v}")).show(ui2);
            }
        });
        ui.add_space(8.0);
        _ = ui.add(Input::new(&mut self.platform.platform_info).placeholder("info note"));
        ui.add_space(4.0);
        if ui.add(Button::new("Save to platform_info")).clicked() {
            functora_egui::storage::persist_value("platform_info", &self.platform.platform_info);
            self.toast
                .add("Saved", ToastVariant::Success, ui.ctx().input(|i| i.time));
        }
        ui.add_space(12.0);
        snippet(
            ui,
            "// Platform info: responsive context + persistent storage\nuse functora_egui::{ResponsiveExt, storage};\n\n// Breakpoint + spacing from the current viewport (800px mobile)\nlet is_mobile = ui.on_mobile();\nlet spacing = ui.ctx().responsive_spacing();\neprintln!(\"{} px content, {} px padding\", spacing.content_max_width, spacing.page_padding);\n\n// Current theme\nlet theme = functora_egui::current_theme(ui.ctx());\n\n// Persistent key/value storage (localStorage / storage.json)\nstorage::persist_value(\"platform_info\", &note)?;\nlet saved = storage::load_state::<String>(\"platform_info\");\n\n// Web-only location helpers\n#[cfg(target_arch = \"wasm32\")]\nif let Some(href) = functora_egui::platform::web::location_href() {\n    eprintln!(\"{href}\");\n}",
        );
    }
}
