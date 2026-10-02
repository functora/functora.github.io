use functora_egui::snippet;
use functora_egui::{Input, Typography};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_input(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Single-line text input field.").show(ui);
        ui.add_space(12.0);
        _ = Input::new(&mut self.input_text)
            .placeholder("Type something...")
            .desired_width(ui.available_width())
            .show(ui);
        ui.add_space(4.0);
        _ = Typography::small(format!("Value: \"{}\"", self.input_text)).show(ui);

        ui.add_space(12.0);
        _ = Typography::small("Password").show(ui);
        ui.add_space(4.0);
        _ = Input::new(&mut self.input_password)
            .password()
            .placeholder("secret")
            .desired_width(ui.available_width())
            .show(ui);

        snippet(
            ui,
            "// Input: single-line text field\nuse functora_egui::Input;\n\nlet mut text = String::new();\nInput::new(&mut text)\n    .placeholder(\"Type something...\")\n    .desired_width(ui.available_width())\n    .show(ui);\n\n// Password\nlet mut secret = String::new();\nInput::new(&mut secret)\n    .password()\n    .placeholder(\"secret\")\n    .desired_width(ui.available_width())\n    .show(ui);",
        );
    }
}
