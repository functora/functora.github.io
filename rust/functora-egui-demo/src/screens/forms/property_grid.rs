use functora_egui::snippet;
use functora_egui::{NumberInput, PropertyGrid, PropertyRow, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_property_grid(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("A label/value grid for inspectors and settings.").show(ui);
        ui.add_space(12.0);
        _ = PropertyGrid::new()
            .label_width(96.0)
            .row_gap(4.0)
            .show(ui, |ui34| {
                _ = PropertyRow::new("X").show(ui34, |ui35| {
                    _ = ui35.add(
                        NumberInput::new(&mut self.prop_x)
                            .range(-500.0..=500.0)
                            .width(110.0),
                    );
                });
                _ = PropertyRow::new("Y").show(ui34, |ui36| {
                    _ = ui36.add(
                        NumberInput::new(&mut self.prop_y)
                            .range(-500.0..=500.0)
                            .width(110.0),
                    );
                });
                _ = PropertyRow::new("Width").show(ui34, |ui37| {
                    _ = ui37.add(
                        NumberInput::new(&mut self.prop_width)
                            .range(0.0..=2000.0)
                            .width(110.0),
                    );
                });
                _ = PropertyRow::new("Height").show(ui34, |ui38| {
                    _ = ui38.add(
                        NumberInput::new(&mut self.prop_height)
                            .range(0.0..=2000.0)
                            .width(110.0),
                    );
                });
                _ = PropertyRow::new("Rotation").show(ui34, |ui39| {
                    _ = ui39.add(
                        NumberInput::new(&mut self.prop_rotation)
                            .range(-180.0..=180.0)
                            .suffix(" deg")
                            .width(110.0),
                    );
                });
                _ = PropertyRow::new("Opacity").show(ui34, |ui40| {
                    _ = ui40.add(
                        NumberInput::new(&mut self.prop_opacity)
                            .range(0.0..=100.0)
                            .suffix("%")
                            .width(110.0),
                    );
                });
            });

        snippet(
            ui,
            "// PropertyGrid: label/value grid for inspectors\nuse functora_egui::{PropertyGrid, PropertyRow, NumberInput};\n\nPropertyGrid::new()\n    .label_width(96.0)\n    .row_gap(4.0)\n    .show(ui, |grid| {\n        PropertyRow::new(\"X\").show(grid, |row| {\n            row.add(NumberInput::new(&mut x).range(-500.0..=500.0).width(110.0));\n        });\n        PropertyRow::new(\"Y\").show(grid, |row| {\n            row.add(NumberInput::new(&mut y).range(-500.0..=500.0).width(110.0));\n        });\n        PropertyRow::new(\"Width\").show(grid, |row| {\n            row.add(NumberInput::new(&mut w).range(0.0..=2000.0).width(110.0));\n        });\n        PropertyRow::new(\"Height\").show(grid, |row| {\n            row.add(NumberInput::new(&mut h).range(0.0..=2000.0).width(110.0));\n        });\n        PropertyRow::new(\"Rotation\").show(grid, |row| {\n            row.add(NumberInput::new(&mut rot).range(-180.0..=180.0).suffix(\" deg\").width(110.0));\n        });\n        PropertyRow::new(\"Opacity\").show(grid, |row| {\n            row.add(NumberInput::new(&mut opacity).range(0.0..=100.0).suffix(\"%\").width(110.0));\n        });\n    });",
        );
    }
}
