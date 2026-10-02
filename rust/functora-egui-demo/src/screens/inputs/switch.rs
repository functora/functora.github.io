use functora_egui::snippet;
use functora_egui::{Switch, Typography};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_switch(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("A toggle switch for on/off states.").show(ui);
        ui.add_space(12.0);
        _ = ui.add(Switch::new(&mut self.checks.switch_val).label("Airplane mode"));
        ui.add_space(4.0);
        _ = Typography::small(format!("Enabled: {}", self.checks.switch_val)).show(ui);

        snippet(
            ui,
            "// Switch: toggle on/off with label\nuse functora_egui::Switch;\n\nlet mut enabled = false;\nui.add(Switch::new(&mut enabled).label(\"Airplane mode\"));\n\n// enabled is now true/false",
        );
    }
}
