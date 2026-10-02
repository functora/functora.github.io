use functora_egui::snippet;
use functora_egui::{Button, ButtonGroup, ButtonVariant, LucideIcon, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_button_group(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Connected button strip with merged borders.").show(ui);
        ui.add_space(12.0);
        _ = Typography::small("Selectable group").show(ui);
        ui.add_space(4.0);
        let options = ["Left", "Center", "Right"];
        _ = ButtonGroup::show(ui, |g| {
            for (idx, label) in options.iter().enumerate() {
                if Button::new(*label)
                    .variant(ButtonVariant::Outline)
                    .selected(self.demo.button_group_selected == idx)
                    .show(g)
                    .clicked()
                {
                    self.demo.button_group_selected = idx;
                }
            }
        });
        ui.add_space(4.0);
        _ = Typography::small(format!(
            "Selected: {}",
            options[self.demo.button_group_selected]
        ))
        .show(ui);
        ui.add_space(12.0);
        _ = Typography::small("Icon group").show(ui);
        ui.add_space(4.0);
        _ = ButtonGroup::show(ui, |g| {
            _ = Button::icon_only(LucideIcon::Undo2)
                .variant(ButtonVariant::Ghost)
                .show(g);
            _ = Button::icon_only(LucideIcon::Redo2)
                .variant(ButtonVariant::Ghost)
                .show(g);
        });
        ui.add_space(12.0);
        _ = Typography::small("Disabled group").show(ui);
        ui.add_space(4.0);
        _ = ButtonGroup::show(ui, |g| {
            _ = Button::new("Cut").enabled(false).show(g);
            _ = Button::new("Copy").enabled(false).show(g);
            _ = Button::new("Paste").enabled(false).show(g);
        });

        snippet(
            ui,
            "// ButtonGroup: connected button strip\nuse functora_egui::{ButtonGroup, Button, ButtonVariant, LucideIcon};\n\n// Selectable group bound to an index\nlet options = [\"Left\", \"Center\", \"Right\"];\nlet mut selected = 0;\nButtonGroup::show(ui, |g| {\n    for (idx, label) in options.iter().enumerate() {\n        if Button::new(*label)\n            .variant(ButtonVariant::Outline)\n            .selected(selected == idx)\n            .show(g)\n            .clicked()\n        {\n            selected = idx;\n        }\n    }\n});\n\n// Icon group\nButtonGroup::show(ui, |g| {\n    Button::icon_only(LucideIcon::Undo2).variant(ButtonVariant::Ghost).show(g);\n    Button::icon_only(LucideIcon::Redo2).variant(ButtonVariant::Ghost).show(g);\n});\n\n// Disabled group\nButtonGroup::show(ui, |g| {\n    Button::new(\"Cut\").enabled(false).show(g);\n    Button::new(\"Copy\").enabled(false).show(g);\n    Button::new(\"Paste\").enabled(false).show(g);\n});",
        );
    }
}
