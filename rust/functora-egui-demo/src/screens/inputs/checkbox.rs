use functora_egui::snippet;
use functora_egui::{Checkbox, Typography};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_checkbox(&mut self, ui: &mut egui::Ui) {
        crate::screens::intro(ui, "Toggle boolean values with a checkbox.");
        _ = ui
            .add(Checkbox::new(&mut self.checks.checkbox_val).label("Accept terms and conditions"));
        ui.add_space(4.0);
        _ = Typography::small(format!("Checked: {}", self.checks.checkbox_val)).show(ui);

        snippet(
            ui,
            "// Checkbox: bound to boolean\nuse functora_egui::Checkbox;\n\nlet mut checked = false;\nui.add(Checkbox::new(&mut checked).label(\"Accept terms and conditions\"));\n\n// checked is now true/false based on user interaction",
        );
    }
}
