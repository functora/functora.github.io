use functora_egui::snippet;
use functora_egui::{Flex, Toggle, ToggleVariant, Typography};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_toggle(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Toggle buttons with outline variant.").show(ui);
        ui.add_space(12.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            _ = f.add(
                Toggle::new(&mut self.text_style.toggle_bold, "B").variant(ToggleVariant::Outline),
            );
            _ = f.add(
                Toggle::new(&mut self.text_style.toggle_italic, "I")
                    .variant(ToggleVariant::Outline),
            );
            _ = f.add(
                Toggle::new(&mut self.text_style.toggle_underline, "U")
                    .variant(ToggleVariant::Outline),
            );
        });
        ui.add_space(4.0);
        _ = Typography::small(format!(
            "Bold: {}, Italic: {}, Underline: {}",
            self.text_style.toggle_bold,
            self.text_style.toggle_italic,
            self.text_style.toggle_underline
        ))
        .show(ui);

        snippet(
            ui,
            "// Toggle: pressable button for boolean state\nuse functora_egui::{Toggle, ToggleVariant};\n\nlet mut bold = false;\nlet mut italic = false;\nlet mut underline = false;\n\nToggle::new(&mut bold, \"B\").variant(ToggleVariant::Outline).show(ui);\nToggle::new(&mut italic, \"I\").variant(ToggleVariant::Outline).show(ui);\nToggle::new(&mut underline, \"U\").variant(ToggleVariant::Outline).show(ui);",
        );
    }
}
