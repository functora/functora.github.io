use crate::catalog::Framework;
use functora_egui::snippet;
use functora_egui::{ComboboxValue, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_combobox(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Searchable dropdown with type-ahead filtering.").show(ui);
        ui.add_space(12.0);
        let entries = [
            (Framework::React, "React".to_owned()),
            (Framework::Vue, "Vue".to_owned()),
            (Framework::Angular, "Angular".to_owned()),
            (Framework::Svelte, "Svelte".to_owned()),
            (Framework::Solid, "Solid".to_owned()),
        ];
        _ = ComboboxValue::new(&entries)
            .placeholder("Select framework...")
            .show(ui, &mut self.combobox_framework, &mut self.combobox_search);
        ui.add_space(4.0);
        _ = Typography::small(format!("Selected: {:?}", self.combobox_framework)).show(ui);

        snippet(
            ui,
            "// ComboboxValue: searchable dropdown bound to an enum\nuse functora_egui::ComboboxValue;\n\n#[derive(Clone, Copy, PartialEq)]\nenum Framework { React, Vue, Angular, Svelte, Solid }\n\nlet entries = [(Framework::React, \"React\".to_owned()), (Framework::Vue, \"Vue\".to_owned()), (Framework::Angular, \"Angular\".to_owned()), (Framework::Svelte, \"Svelte\".to_owned()), (Framework::Solid, \"Solid\".to_owned())];\nlet mut selected: Option<Framework> = None;\nlet mut search = String::new();\n\nComboboxValue::new(&entries)\n    .placeholder(\"Select framework...\")\n    .show(ui, &mut selected, &mut search);\n\n// selected is Some(Framework) or None",
        );
    }
}
