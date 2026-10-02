use functora_egui::snippet;
use functora_egui::{Card, Resizable, Typography};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_resizable(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Draggable split pane with an adjustable divider.").show(ui);
        ui.add_space(12.0);
        _ = Resizable::new().height(160.0).show(
            ui,
            &mut self.resizable_fraction,
            |ui56| {
                _ = Card::new().show(ui56, |ui57| {
                    _ = ui57.label("Left Panel");
                });
            },
            |ui58| {
                _ = Card::new().show(ui58, |ui59| {
                    _ = ui59.label("Right Panel");
                });
            },
        );
        ui.add_space(4.0);
        _ = Typography::small(format!("Fraction: {:.2}", self.resizable_fraction)).show(ui);

        snippet(
            ui,
            "// Resizable: draggable split pane\nuse functora_egui::Resizable;\n\nlet mut fraction = 0.5;\nResizable::new()\n    .height(160.0)\n    .show(ui, &mut fraction, |left| {\n        Card::new().show(left, |l| l.label(\"Left Panel\"));\n    }, |right| {\n        Card::new().show(right, |r| r.label(\"Right Panel\"));\n    });\n\n// fraction is now the split ratio (0.0 to 1.0)",
        );
    }
}
