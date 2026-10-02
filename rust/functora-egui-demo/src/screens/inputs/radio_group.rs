use crate::catalog::RadioOption;
use functora_egui::snippet;
use functora_egui::{RadioGroupLabeled, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_radio_group(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("A group of radio buttons managed together.").show(ui);
        ui.add_space(12.0);
        let entries = RadioOption::ALL.map(|(value, label)| (value, label.to_owned()));
        _ = RadioGroupLabeled::new(&mut self.radio_group_val, &entries).show(ui);
        ui.add_space(4.0);
        _ = Typography::small(format!("Selected: {:?}", self.radio_group_val)).show(ui);

        snippet(
            ui,
            "// RadioGroupLabeled: managed group bound to an enum\nuse functora_egui::RadioGroupLabeled;\n\n#[derive(Clone, Copy, PartialEq)]\nenum RadioOption { A, B, C }\n\nlet entries = [(RadioOption::A, \"Option A\".to_owned()), (RadioOption::B, \"Option B\".to_owned()), (RadioOption::C, \"Option C\".to_owned())];\nlet mut selected = RadioOption::A;\n\nRadioGroupLabeled::new(&mut selected, &entries).show(ui);\n\n// selected now holds the chosen variant",
        );
    }
}
