use functora_egui::snippet;
use functora_egui::{FieldSet, Input, Label, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_field_set(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("A bordered fieldset container for grouped controls.").show(ui);
        ui.add_space(12.0);
        _ = FieldSet::show(ui, "Shipping address", |ui31| {
            _ = Label::new("Full name").show(ui31);
            ui31.add_space(8.0);
            _ = Input::new(&mut self.form.form_name)
                .placeholder("Ada Lovelace")
                .show(ui31);
            ui31.add_space(8.0);
            _ = Label::new("Email").show(ui31);
            ui31.add_space(8.0);
            _ = Input::new(&mut self.field_set_email)
                .placeholder("ada@example.com")
                .show(ui31);
        });

        snippet(
            ui,
            "// FieldSet: bordered container for grouped controls\nuse functora_egui::{FieldSet, Label, Input};\n\nFieldSet::show(ui, \"Shipping address\", |body| {\n    Label::new(\"Full name\").show(body);\n    Input::new(&mut name).placeholder(\"Ada Lovelace\").show(body);\n    Label::new(\"Email\").show(body);\n    Input::new(&mut email).placeholder(\"ada@example.com\").show(body);\n});",
        );
    }
}
