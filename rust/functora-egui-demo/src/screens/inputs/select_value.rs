use crate::catalog::BlendMode;
use functora_egui::snippet;
use functora_egui::{SelectValue, SelectValueLabeled, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_select_value(&mut self, ui: &mut egui::Ui) {
        crate::screens::intro(ui, "Select bound to a non-Option enum value.");
        let entries = BlendMode::ALL.map(|(value, label)| (value, label.to_owned()));
        _ = SelectValueLabeled::new(&mut self.select_blend, &entries)
            .width(220.0)
            .show(ui);
        ui.add_space(4.0);
        _ = Typography::small(format!("Blend mode: {:?}", self.select_blend)).show(ui);
        ui.add_space(12.0);
        crate::screens::section(ui, "Display-based variant with an overridden trigger text");
        let modes = BlendMode::ALL.map(|(value, _)| value);
        _ = SelectValue::new(&mut self.select_display_blend, &modes)
            .width(220.0)
            .selected_text("Custom trigger label")
            .show(ui);
        ui.add_space(4.0);
        _ = Typography::small(format!(
            "Display blend mode: {:?}",
            self.select_display_blend
        ))
        .show(ui);

        snippet(
            ui,
            "// SelectValueLabeled: dropdown bound to a non-Option enum\nuse functora_egui::SelectValueLabeled;\n\n#[derive(Clone, Copy, PartialEq)]\nenum BlendMode { Normal, Multiply, Screen, Overlay }\n\nlet entries = [(BlendMode::Normal, \"Normal\".to_owned()), (BlendMode::Multiply, \"Multiply\".to_owned()), (BlendMode::Screen, \"Screen\".to_owned()), (BlendMode::Overlay, \"Overlay\".to_owned())];\nlet mut blend_mode = BlendMode::Normal;\n\nSelectValueLabeled::new(&mut blend_mode, &entries)\n    .width(220.0)\n    .show(ui);\n\n// blend_mode always holds a valid variant (never None)\n\n// SelectValue: uses Display for the trigger text, which selected_text can override\nuse functora_egui::SelectValue;\n\nlet modes = [BlendMode::Normal, BlendMode::Multiply];\nlet mut mode = BlendMode::Normal;\nSelectValue::new(&mut mode, &modes)\n    .width(220.0)\n    .selected_text(\"Custom trigger label\")\n    .show(ui);",
        );
    }
}
