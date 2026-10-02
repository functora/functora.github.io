use functora_egui::snippet;
use functora_egui::{Textarea, Typography};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_textarea(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Multi-line text area.").show(ui);
        ui.add_space(12.0);
        _ = Textarea::new(&mut self.textarea_text)
            .placeholder("Write a message...")
            .desired_width(ui.available_width().min(420.0))
            .show(ui);
        ui.add_space(12.0);
        _ = Typography::small("Compact override").show(ui);
        ui.add_space(4.0);
        _ = Textarea::new(&mut self.textarea_compact)
            .placeholder("Short note...")
            .min_height(80.0)
            .show(ui);

        snippet(
            ui,
            "// Textarea: multi-line text area (default 192px, 8 rows)\nuse functora_egui::Textarea;\n\nlet mut msg = String::new();\nTextarea::new(&mut msg)\n    .placeholder(\"Write a message...\")\n    .desired_width(ui.available_width().min(420.0))\n    .show(ui);\n\n// Compact override\nTextarea::new(&mut msg)\n    .min_height(80.0)\n    .show(ui);",
        );
    }
}
