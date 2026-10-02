use crate::catalog::Align;
use functora_egui::snippet;
use functora_egui::{ToggleGroupValue, Typography};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_toggle_group(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Exclusive toggle group: only one active at a time.").show(ui);
        ui.add_space(12.0);
        let entries = [
            (Align::Left, "Left".to_owned()),
            (Align::Center, "Center".to_owned()),
            (Align::Right, "Right".to_owned()),
        ];
        _ = ToggleGroupValue::new(&entries).show(ui, &mut self.toggle_group_align);
        ui.add_space(4.0);
        _ = Typography::small(format!("Selected: {:?}", self.toggle_group_align)).show(ui);

        snippet(
            ui,
            "// ToggleGroupValue: exclusive selection bound to an enum\nuse functora_egui::ToggleGroupValue;\n\n#[derive(Clone, Copy, PartialEq)]\nenum Align { Left, Center, Right }\n\nlet entries = [(Align::Left, \"Left\".to_owned()), (Align::Center, \"Center\".to_owned()), (Align::Right, \"Right\".to_owned())];\nlet mut align = Align::Left;\n\nToggleGroupValue::new(&entries).show(ui, &mut align);\n\n// align now holds the chosen variant",
        );
    }
}
