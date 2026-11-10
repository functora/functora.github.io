use crate::catalog::Fruit;
use functora_egui::snippet;
use functora_egui::{Select, SelectLabeled, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_select(&mut self, ui: &mut egui::Ui) {
        crate::screens::intro(ui, "Dropdown selection from a list.");
        let entries = Fruit::ALL.map(|(value, label)| (value, label.to_owned()));
        _ = SelectLabeled::new(&mut self.select_val, &entries)
            .placeholder("Pick a fruit...")
            .width(220.0)
            .show(ui);
        ui.add_space(4.0);
        _ = Typography::small(format!("Selected: {:?}", self.select_val)).show(ui);
        ui.add_space(12.0);
        crate::screens::section(ui, "Display-based select with an overridden trigger text");
        let fruits = Fruit::ALL.map(|(value, _)| value);
        _ = Select::new(&mut self.select_display, &fruits)
            .placeholder("Pick a fruit...")
            .width(220.0)
            .selected_text("Custom trigger label")
            .show(ui);
        ui.add_space(4.0);
        _ = Typography::small(format!("Display select: {:?}", self.select_display)).show(ui);

        snippet(
            ui,
            "// SelectLabeled: dropdown bound to an enum (Option value)\nuse functora_egui::SelectLabeled;\n\n#[derive(Clone, Copy, PartialEq)]\nenum Fruit { Apple, Banana, Cherry, Grape, Mango }\n\nlet entries = [(Fruit::Apple, \"Apple\".to_owned()), (Fruit::Banana, \"Banana\".to_owned()), (Fruit::Cherry, \"Cherry\".to_owned()), (Fruit::Grape, \"Grape\".to_owned()), (Fruit::Mango, \"Mango\".to_owned())];\nlet mut fruit: Option<Fruit> = None;\n\nSelectLabeled::new(&mut fruit, &entries)\n    .placeholder(\"Pick a fruit...\")\n    .width(220.0)\n    .show(ui);\n\n// fruit is now Some(Fruit) or None\n\n// Select: uses Display for the trigger text, which selected_text can override\nuse functora_egui::Select;\n\nlet options = [Fruit::Apple, Fruit::Banana, Fruit::Cherry];\nlet mut choice: Option<Fruit> = None;\nSelect::new(&mut choice, &options)\n    .placeholder(\"Pick a fruit...\")\n    .width(220.0)\n    .selected_text(\"Custom trigger label\")\n    .show(ui);",
        );
    }
}
