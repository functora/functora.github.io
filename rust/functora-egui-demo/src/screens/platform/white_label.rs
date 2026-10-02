use functora_egui::snippet;
use functora_egui::{Card, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_white_label(ui: &mut egui::Ui) {
        _ = Typography::muted(
            "WhiteLabel: functora_core::white_label - AppAttrs branding URLs, donate blocks, WhiteLabelContent defaults.",
        )
        .show(ui);
        ui.add_space(12.0);
        let attrs = Self::DEMO_ATTRS;
        let content =
            functora_egui::white_label::WhiteLabelContent::<functora_egui::messages::Msg>::default(
            );
        let custom_branding = content.license_text.is_some() || content.privacy_text.is_some();
        let donate_labels = content
            .donate_blocks
            .iter()
            .map(|block| block.label.as_str())
            .collect::<Vec<_>>()
            .join(", ");
        _ = Card::new().show(ui, |ui2| {
            _ = Typography::small(format!(
                "app: {} v{} ({})",
                attrs.app, attrs.vsn, attrs.description
            ))
            .show(ui2);
            _ = Typography::small(format!("app_url: {}", attrs.app_url())).show(ui2);
            _ = Typography::small(format!("source_url: {}", attrs.source_url())).show(ui2);
            _ = Typography::small(format!("custom branding: {custom_branding}")).show(ui2);
            _ = Typography::small(format!("donate_blocks: {donate_labels}")).show(ui2);
        });
        ui.add_space(12.0);
        snippet(
            ui,
            "// WhiteLabel: per-app branding derived from AppAttrs\nuse functora_egui::white_label::{AppAttrs, WhiteLabelContent};\n\nconst ATTRS: AppAttrs = AppAttrs {\n    app: \"functora-egui-demo\",\n    vsn: env!(\"CARGO_PKG_VERSION\"),\n    org: \"functora\",\n    src: Some(\"rust\"),\n    dst: \"apps\",\n    description: env!(\"CARGO_PKG_DESCRIPTION\"),\n};\n\n// Derived branding URLs\nlet app_url = ATTRS.app_url();        // https://functora.github.io/apps/functora-egui-demo\nlet source_url = ATTRS.source_url();  // repo tree link\n\n// Default content (license / privacy / donate blocks)\nlet content = WhiteLabelContent::<functora_egui::messages::Msg>::default();\nlet donate = content.donate_blocks.len();",
        );
    }
}
