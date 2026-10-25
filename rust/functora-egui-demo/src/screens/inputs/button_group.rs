use crate::catalog::ButtonGroupOption;
use functora_egui::snippet;
use functora_egui::{Button, ButtonGroup, ButtonVariant, LucideIcon, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_button_group(&mut self, ui: &mut egui::Ui) {
        crate::screens::intro(ui, "Connected button strip with merged borders.");
        crate::screens::section(ui, "Selectable group");
        _ = ButtonGroup::show(ui, |g| {
            for (choice, label) in ButtonGroupOption::ALL {
                if Button::new(label)
                    .variant(ButtonVariant::Outline)
                    .selected(self.demo.button_group_selected == choice)
                    .show(g)
                    .clicked()
                {
                    self.demo.button_group_selected = choice;
                }
            }
        });
        ui.add_space(4.0);
        let selected = ButtonGroupOption::ALL
            .iter()
            .find(|(choice, _)| *choice == self.demo.button_group_selected)
            .map_or("", |(_, label)| *label);
        _ = Typography::small(format!("Selected: {selected}")).show(ui);
        ui.add_space(12.0);
        crate::screens::section(ui, "Icon group");
        _ = ButtonGroup::show(ui, |g| {
            _ = Button::icon_only(LucideIcon::Undo2)
                .variant(ButtonVariant::Ghost)
                .show(g);
            _ = Button::icon_only(LucideIcon::Redo2)
                .variant(ButtonVariant::Ghost)
                .show(g);
        });
        ui.add_space(12.0);
        crate::screens::section(ui, "Disabled group");
        _ = ButtonGroup::show(ui, |g| {
            _ = Button::new("Cut").enabled(false).show(g);
            _ = Button::new("Copy").enabled(false).show(g);
            _ = Button::new("Paste").enabled(false).show(g);
        });

        snippet(
            ui,
            "// ButtonGroup: connected button strip\nuse functora_egui::{ButtonGroup, Button, ButtonVariant, LucideIcon};\n\n// Selectable group bound to an enum\n#[derive(Clone, Copy, PartialEq)]\nenum ButtonGroupOption { Left, Center, Right }\n\nlet entries = [\n    (ButtonGroupOption::Left, \"Left\"),\n    (ButtonGroupOption::Center, \"Center\"),\n    (ButtonGroupOption::Right, \"Right\"),\n];\nlet mut selected = ButtonGroupOption::Left;\nButtonGroup::show(ui, |g| {\n    for (choice, label) in entries {\n        if Button::new(label)\n            .variant(ButtonVariant::Outline)\n            .selected(selected == choice)\n            .show(g)\n            .clicked()\n        {\n            selected = choice;\n        }\n    }\n});\n\n// Icon group\nButtonGroup::show(ui, |g| {\n    Button::icon_only(LucideIcon::Undo2).variant(ButtonVariant::Ghost).show(g);\n    Button::icon_only(LucideIcon::Redo2).variant(ButtonVariant::Ghost).show(g);\n});\n\n// Disabled group\nButtonGroup::show(ui, |g| {\n    Button::new(\"Cut\").enabled(false).show(g);\n    Button::new(\"Copy\").enabled(false).show(g);\n    Button::new(\"Paste\").enabled(false).show(g);\n});",
        );
    }
}
