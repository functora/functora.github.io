use functora_egui::snippet;
use functora_egui::{Progress, Typography};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_progress(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("A progress indicator with a value 0.0..=1.0.").show(ui);
        ui.add_space(12.0);
        _ = Progress::new(self.progress_val).show(ui);
        ui.add_space(4.0);
        _ = Typography::small(format!("{:.0}%", self.progress_val * 100.0)).show(ui);

        snippet(
            ui,
            "// Progress: indicator with value 0.0..=1.0\nuse functora_egui::Progress;\n\nlet mut progress = 0.66;\nProgress::new(progress).show(ui);\n\n// progress is a f32 between 0.0 and 1.0",
        );
    }
}
