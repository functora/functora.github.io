use crate::catalog::Align;
use functora_egui::snippet;
use functora_egui::{LucideIcon, ToggleGroup, ToggleGroupValue, ToggleVariant, Typography};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_toggle_group(&mut self, ui: &mut egui::Ui) {
        crate::screens::intro(ui, "Exclusive toggle group: only one active at a time.");
        let entries = [
            (Align::Left, "Left".to_owned()),
            (Align::Center, "Center".to_owned()),
            (Align::Right, "Right".to_owned()),
        ];
        _ = ToggleGroupValue::new(&entries).show(ui, &mut self.toggle_group_align);
        ui.add_space(4.0);
        _ = Typography::small(format!("Selected: {:?}", self.toggle_group_align)).show(ui);
        ui.add_space(12.0);
        crate::screens::section(ui, "Value group with icons and an outline variant");
        let icons = [
            (Align::Left, LucideIcon::TextAlignStart),
            (Align::Center, LucideIcon::TextAlignCenter),
            (Align::Right, LucideIcon::TextAlignEnd),
        ];
        _ = ToggleGroupValue::new(&entries)
            .icons(&icons)
            .variant(ToggleVariant::Outline)
            .show(ui, &mut self.toggle_group_align);
        ui.add_space(12.0);
        crate::screens::section(ui, "Label-only group");
        let mut label_selected = 0_usize;
        _ = ToggleGroup::new(vec![
            "Bold".to_owned(),
            "Italic".to_owned(),
            "Underline".to_owned(),
        ])
        .icons(vec![
            LucideIcon::Bold,
            LucideIcon::Italic,
            LucideIcon::Underline,
        ])
        .variant(ToggleVariant::Default)
        .show(ui, &mut label_selected);
        ui.add_space(4.0);
        _ = Typography::small(format!("Label group index: {label_selected}")).show(ui);

        snippet(
            ui,
            "// ToggleGroupValue: exclusive selection bound to an enum\nuse functora_egui::ToggleGroupValue;\n\n#[derive(Clone, Copy, PartialEq)]\nenum Align { Left, Center, Right }\n\nlet entries = [(Align::Left, \"Left\".to_owned()), (Align::Center, \"Center\".to_owned()), (Align::Right, \"Right\".to_owned())];\nlet mut align = Align::Left;\n\nToggleGroupValue::new(&entries).show(ui, &mut align);\n\n// align now holds the chosen variant\n\n// Icons map by value; values without an icon stay icon-less\nuse functora_egui::{LucideIcon, ToggleVariant};\n\nlet icons = [(Align::Left, LucideIcon::TextAlignStart), (Align::Center, LucideIcon::TextAlignCenter), (Align::Right, LucideIcon::TextAlignEnd)];\nToggleGroupValue::new(&entries)\n    .icons(&icons)\n    .variant(ToggleVariant::Outline)\n    .show(ui, &mut align);\n\n// Label-only group (no bound value)\nuse functora_egui::ToggleGroup;\n\nlet mut selected = 0usize;\nToggleGroup::new(vec![\"Bold\".to_owned(), \"Italic\".to_owned()])\n    .icons(vec![LucideIcon::Bold, LucideIcon::Italic])\n    .show(ui, &mut selected);",
        );
    }
}
