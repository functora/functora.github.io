use functora_egui::snippet;
use functora_egui::{Radio, Typography};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_radio(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Individual radio buttons for exclusive selection.").show(ui);
        ui.add_space(12.0);
        if ui
            .add(Radio::new(&mut self.radios.radio_a).label("Option A"))
            .clicked()
        {
            self.radios.radio_b = false;
            self.radios.radio_c = false;
        }
        if ui
            .add(Radio::new(&mut self.radios.radio_b).label("Option B"))
            .clicked()
        {
            self.radios.radio_a = false;
            self.radios.radio_c = false;
        }
        if ui
            .add(Radio::new(&mut self.radios.radio_c).label("Option C"))
            .clicked()
        {
            self.radios.radio_a = false;
            self.radios.radio_b = false;
        }

        snippet(
            ui,
            "// Radio: individual buttons for exclusive selection\nuse functora_egui::Radio;\n\nlet mut option_a = true;\nlet mut option_b = false;\nlet mut option_c = false;\n\nif ui.add(Radio::new(&mut option_a).label(\"Option A\")).clicked() {\n    option_b = false;\n    option_c = false;\n}\nif ui.add(Radio::new(&mut option_b).label(\"Option B\")).clicked() {\n    option_a = false;\n    option_c = false;\n}\nif ui.add(Radio::new(&mut option_c).label(\"Option C\")).clicked() {\n    option_a = false;\n    option_b = false;\n}",
        );
    }
}
