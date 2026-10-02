use crate::catalog::BlendMode;
use functora_egui::snippet;
use functora_egui::{SelectValueLabeled, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_select_value(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Select bound to a non-Option enum value.").show(ui);
        ui.add_space(12.0);
        let entries = BlendMode::ALL.map(|(value, label)| (value, label.to_owned()));
        _ = SelectValueLabeled::new(&mut self.select_blend, &entries).show(ui);
        ui.add_space(4.0);
        _ = Typography::small(format!("Blend mode: {:?}", self.select_blend)).show(ui);

        snippet(
            ui,
            "// SelectValueLabeled: dropdown bound to a non-Option enum\nuse functora_egui::SelectValueLabeled;\n\n#[derive(Clone, Copy, PartialEq)]\nenum BlendMode { Normal, Multiply, Screen, Overlay }\n\nlet entries = [(BlendMode::Normal, \"Normal\".to_owned()), (BlendMode::Multiply, \"Multiply\".to_owned()), (BlendMode::Screen, \"Screen\".to_owned()), (BlendMode::Overlay, \"Overlay\".to_owned())];\nlet mut blend_mode = BlendMode::Normal;\n\nSelectValueLabeled::new(&mut blend_mode, &entries).show(ui);\n\n// blend_mode always holds a valid variant (never None)",
        );
    }
}
