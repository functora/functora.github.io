use crate::catalog::Fruit;
use functora_egui::snippet;
use functora_egui::{SelectLabeled, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_select(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Dropdown selection from a list.").show(ui);
        ui.add_space(12.0);
        let entries = Fruit::ALL.map(|(value, label)| (value, label.to_owned()));
        _ = SelectLabeled::new(&mut self.select_val, &entries)
            .placeholder("Pick a fruit...")
            .show(ui);
        ui.add_space(4.0);
        _ = Typography::small(format!("Selected: {:?}", self.select_val)).show(ui);

        snippet(
            ui,
            "// SelectLabeled: dropdown bound to an enum (Option value)\nuse functora_egui::SelectLabeled;\n\n#[derive(Clone, Copy, PartialEq)]\nenum Fruit { Apple, Banana, Cherry, Grape, Mango }\n\nlet entries = [(Fruit::Apple, \"Apple\".to_owned()), (Fruit::Banana, \"Banana\".to_owned()), (Fruit::Cherry, \"Cherry\".to_owned()), (Fruit::Grape, \"Grape\".to_owned()), (Fruit::Mango, \"Mango\".to_owned())];\nlet mut fruit: Option<Fruit> = None;\n\nSelectLabeled::new(&mut fruit, &entries)\n    .placeholder(\"Pick a fruit...\")\n    .show(ui);\n\n// fruit is now Some(Fruit) or None",
        );
    }
}
