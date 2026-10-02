use functora_egui::snippet;
use functora_egui::{Accordion, Typography};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_accordion(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Expandable sections: click to toggle.").show(ui);
        ui.add_space(12.0);
        _ = Accordion::new(vec![
            (
                "Is it accessible?".to_owned(),
                "Yes. It adheres to the WAI-ARIA design pattern.".to_owned(),
            ),
            (
                "Is it styled?".to_owned(),
                "Yes. It comes with default styles matching shadcn/ui.".to_owned(),
            ),
            (
                "Is it animated?".to_owned(),
                "Yes. It has smooth open/close transitions.".to_owned(),
            ),
        ])
        .multiple()
        .show(ui, &mut self.accordion_open);
        ui.add_space(12.0);
        _ = Typography::small("Single mode (only one open at a time)").show(ui);
        ui.add_space(4.0);
        let mut single_open = vec![0];
        _ = Accordion::new(vec![
            (
                "What is Rust?".to_owned(),
                "A systems programming language focused on safety and performance.".to_owned(),
            ),
            (
                "What is egui?".to_owned(),
                "An immediate-mode GUI library for Rust.".to_owned(),
            ),
            (
                "What is shadcn/ui?".to_owned(),
                "A component library design system.".to_owned(),
            ),
        ])
        .show(ui, &mut single_open);

        snippet(
            ui,
            "// Accordion: expandable sections\nuse functora_egui::Accordion;\n\n// Multiple mode (default)\nlet items = vec![\n    (\"Is it accessible?\", \"Yes. It adheres to the WAI-ARIA design pattern.\"),\n    (\"Is it styled?\", \"Yes. It comes with default styles matching shadcn/ui.\"),\n    (\"Is it animated?\", \"Yes. It has smooth open/close transitions.\"),\n];\nlet mut open_indices = vec![0];\n\nAccordion::new(items.clone())\n    .multiple()\n    .show(ui, &mut open_indices);\n\n// Single mode (only one open at a time)\nlet mut single_open = vec![0];\nAccordion::new(items)\n    .show(ui, &mut single_open);",
        );
    }
}
