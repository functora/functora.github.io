use functora_egui::snippet;
use functora_egui::{Calendar, Typography};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_calendar(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("A month grid calendar with navigation.").show(ui);
        ui.add_space(12.0);
        let clicked = Calendar::new().show(
            ui,
            &mut self.calendar_year,
            &mut self.calendar_month,
            &mut self.calendar_day,
        );
        if let Some(day) = clicked {
            self.calendar_day = day;
            self.toast.add(
                format!("Selected {day}"),
                functora_egui::ToastVariant::Default,
                ui.ctx().input(|i| i.time),
            );
        }
        ui.add_space(4.0);
        _ = Typography::small(format!(
            "{:04}-{:02}-{:02}",
            self.calendar_year, self.calendar_month, self.calendar_day
        ))
        .show(ui);

        snippet(
            ui,
            "// Calendar: month grid with navigation\nuse functora_egui::Calendar;\n\nlet mut year = 2026;\nlet mut month = 8;\nlet mut day = 20;\n\nif let Some(clicked_day) = Calendar::new().show(ui, &mut year, &mut month, &mut day) {\n    day = clicked_day;\n    eprintln!(\"Selected: {:04}-{:02}-{:02}\", year, month, day);\n}",
        );
    }
}
