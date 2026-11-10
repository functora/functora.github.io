use crate::catalog::RadioOption;
use functora_egui::snippet;
use functora_egui::{RadioGroup, RadioGroupLabeled, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_radio_group(&mut self, ui: &mut egui::Ui) {
        crate::screens::intro(ui, "A group of radio buttons managed together.");
        let entries = RadioOption::ALL.map(|(value, label)| (value, label.to_owned()));
        _ = RadioGroupLabeled::new(&mut self.radio_group_val, &entries).show(ui);
        ui.add_space(4.0);
        _ = Typography::small(format!("Selected: {:?}", self.radio_group_val)).show(ui);

        ui.add_space(12.0);
        crate::screens::section(ui, "Display-based group");
        let options = [RadioOption::A, RadioOption::B, RadioOption::C];
        _ = RadioGroup::new(&mut self.radio_group_display, &options).show(ui);
        ui.add_space(4.0);
        _ = Typography::small(format!("Display selected: {}", self.radio_group_display)).show(ui);

        snippet(
            ui,
            "// RadioGroupLabeled: managed group bound to an enum\nuse functora_egui::RadioGroupLabeled;\n\n#[derive(Clone, Copy, PartialEq)]\nenum RadioOption { A, B, C }\n\nlet entries = [(RadioOption::A, \"Option A\".to_owned()), (RadioOption::B, \"Option B\".to_owned()), (RadioOption::C, \"Option C\".to_owned())];\nlet mut selected = RadioOption::A;\n\nRadioGroupLabeled::new(&mut selected, &entries).show(ui);\n\n// selected now holds the chosen variant\n\n// RadioGroup: the Display-based variant labels the options with Display\nuse functora_egui::RadioGroup;\n\nlet options = [RadioOption::A, RadioOption::B, RadioOption::C];\nRadioGroup::new(&mut selected, &options).show(ui);",
        );
    }
}
