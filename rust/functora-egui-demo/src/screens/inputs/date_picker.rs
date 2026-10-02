use functora_egui::snippet;
use functora_egui::{DatePicker, Typography};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_date_picker(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Date picker with a popover calendar.").show(ui);
        ui.add_space(12.0);
        _ = DatePicker::new()
            .placeholder("Pick a date")
            .show(ui, &mut self.date_picker);
        ui.add_space(4.0);
        if self.date_picker.is_set() {
            _ = Typography::small(format!("Selected: {}", self.date_picker.format())).show(ui);
        } else {
            _ = Typography::small("No date selected.").show(ui);
        }

        snippet(
            ui,
            "// DatePicker: popover calendar\nuse functora_egui::{DatePicker, DatePickerState};\n\nlet mut state = DatePickerState::default();\n\nDatePicker::new()\n    .placeholder(\"Pick a date\")\n    .show(ui, &mut state);\n\nif state.is_set() {\n    let date = state.format(); // e.g. \"2026-08-27\"\n    eprintln!(\"Selected: {date}\");\n}",
        );
    }
}
