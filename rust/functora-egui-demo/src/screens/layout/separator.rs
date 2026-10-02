use functora_egui::snippet;
use functora_egui::{Separator, Typography};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_separator(ui: &mut egui::Ui) {
        _ = Typography::muted("Visual divider between content sections.").show(ui);
        ui.add_space(12.0);
        _ = ui.label("Content above");
        _ = Separator::horizontal().show(ui);
        _ = ui.label("Content below");
        ui.add_space(12.0);
        _ = Separator::horizontal().text("With Label").show(ui);
        _ = ui.label("Content after labeled separator");
        ui.add_space(12.0);
        _ = ui.horizontal(|ui61| {
            _ = ui61.label("Left");
            _ = Separator::vertical().show(ui61);
            _ = ui61.label("Right");
        });

        snippet(
            ui,
            "// Separator: visual dividers\nuse functora_egui::Separator;\n\nSeparator::horizontal().show(ui);\nSeparator::horizontal().text(\"With Label\").show(ui);\nui.horizontal(|ui| {\n    ui.label(\"Left\");\n    Separator::vertical().show(ui);\n    ui.label(\"Right\");\n});",
        );
    }
}
