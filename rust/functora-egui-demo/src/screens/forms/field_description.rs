use functora_egui::snippet;
use functora_egui::{FieldDescription, Input, Label};

impl crate::state::ShowcaseApp {
    pub fn demo_field_description(&mut self, ui: &mut egui::Ui) {
        crate::screens::intro(ui, "Supporting helper text under a field.");
        _ = Label::new("Password").show(ui);
        ui.add_space(8.0);
        _ = Input::new(&mut self.field_description_password)
            .password()
            .show(ui);
        ui.add_space(8.0);
        FieldDescription::show(ui, "Use at least 8 characters with numbers and symbols.");

        snippet(
            ui,
            "// FieldDescription: helper text under a field\nuse functora_egui::{FieldDescription, Label, Input};\n\nLabel::new(\"Password\").show(ui);\nInput::new(&mut password).password().show(ui);\nFieldDescription::show(ui, \"Use at least 8 characters with numbers and symbols.\");",
        );
    }
}
