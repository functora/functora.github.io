use functora_egui::snippet;
use functora_egui::{Button, ButtonVariant, ComponentSize, InputGroup, LucideIcon};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_input_group(&mut self, ui: &mut egui::Ui) {
        crate::screens::intro(ui, "Input with prefix text and suffix addons.");

        crate::screens::section(ui, "With prefix");
        _ = InputGroup::show(
            ui,
            &mut self.input_group_url,
            "example.com",
            Some("https://"),
            None::<fn(&mut egui::Ui)>,
        );

        ui.add_space(12.0);
        crate::screens::section(ui, "With prefix and suffix button");
        _ = InputGroup::show(
            ui,
            &mut self.input_group_search,
            "Search...",
            None,
            Some(|ui68: &mut egui::Ui| {
                _ = Button::icon_only(LucideIcon::Search)
                    .variant(ButtonVariant::Ghost)
                    .size(ComponentSize::Sm)
                    .show(ui68);
            }),
        );

        snippet(
            ui,
            "// InputGroup: input with prefix text and/or suffix addon\nuse functora_egui::{InputGroup, Button, ButtonVariant, LucideIcon, ComponentSize};\n\n// With prefix\nlet mut url = String::new();\nInputGroup::show(\n    ui,\n    &mut url,\n    \"example.com\",\n    Some(\"https://\"),\n    None::<fn(&mut egui::Ui)>,\n);\n\n// With prefix and suffix button\nlet mut search = String::new();\nInputGroup::show(\n    ui,\n    &mut search,\n    \"Search...\",\n    None,\n    Some(|ui| {\n        Button::icon_only(LucideIcon::Search)\n            .variant(ButtonVariant::Ghost)\n            .size(ComponentSize::Sm)\n            .show(ui);\n    }),\n);",
        );
    }
}
