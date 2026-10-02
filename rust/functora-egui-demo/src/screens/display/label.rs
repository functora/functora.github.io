use functora_egui::snippet;
use functora_egui::{Label, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_label(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Labels pair with inputs in forms and settings.").show(ui);
        ui.add_space(12.0);
        _ = Label::new("Your email address").show(ui);
        ui.add_space(8.0);
        _ = functora_egui::Input::new(&mut self.label_email)
            .placeholder("you@example.com")
            .show(ui);
        ui.add_space(8.0);
        _ = Label::new("Sizes").show(ui);
        ui.add_space(8.0);
        _ = Label::new("Small label")
            .size(functora_egui::ComponentSize::Sm)
            .show(ui);
        ui.add_space(8.0);
        _ = Label::new("Muted label").muted().show(ui);

        snippet(
            ui,
            "// Label: text labels for forms\nuse functora_egui::{Label, Input, ComponentSize};\n\nLabel::new(\"Your email address\").show(ui);\n\nInput::new(&mut email).placeholder(\"you@example.com\").show(ui);\n\nLabel::new(\"Sizes\").show(ui);\nLabel::new(\"Small label\").size(ComponentSize::Sm).show(ui);\nLabel::new(\"Muted label\").muted().show(ui);",
        );
    }
}
