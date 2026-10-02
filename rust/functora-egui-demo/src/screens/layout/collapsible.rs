use functora_egui::snippet;
use functora_egui::{Button, ButtonVariant, Collapsible, ComponentSize, Typography};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_collapsible(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("A section that can be toggled open or closed.").show(ui);
        ui.add_space(12.0);
        _ = Collapsible::new("Click to toggle").show(
            ui,
            &mut self.checks.collapsible_open,
            |ui55| {
                _ = ui55.label("This content is hidden when the collapsible is closed.");
                _ = ui55.label("You can put any widgets inside here.");
                ui55.add_space(4.0);
                _ = Button::new("Nested Action")
                    .variant(ButtonVariant::Outline)
                    .size(ComponentSize::Sm)
                    .show(ui55);
            },
        );

        snippet(
            ui,
            "// Collapsible: toggleable content section\nuse functora_egui::Collapsible;\n\nlet mut open = true;\nCollapsible::new(\"Click to toggle\").show(ui, &mut open, |body| {\n    body.label(\"This content is hidden when the collapsible is closed.\");\n    body.label(\"You can put any widgets inside here.\");\n    Button::new(\"Nested Action\")\n        .variant(ButtonVariant::Outline)\n        .size(ComponentSize::Sm)\n        .show(body);\n});",
        );
    }
}
