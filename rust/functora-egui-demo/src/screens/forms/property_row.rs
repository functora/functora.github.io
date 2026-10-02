use crate::catalog::BlendMode;
use functora_egui::snippet;
use functora_egui::{
    Badge, Button, ButtonVariant, Flex, PropertyGrid, PropertyRow, SelectValueLabeled, Typography,
};

impl crate::state::ShowcaseApp {
    pub fn demo_property_row(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("A single labeled row: text, badges, or inputs.").show(ui);
        ui.add_space(12.0);
        _ = PropertyGrid::new().label_width(96.0).show(ui, |ui41| {
            _ = PropertyRow::new("Mode").show(ui41, |ui42| {
                _ = Badge::new("Auto")
                    .variant(functora_egui::BadgeVariant::Secondary)
                    .show(ui42);
            });
            _ = PropertyRow::new("Blend").show(ui41, |ui43| {
                let entries = BlendMode::ALL.map(|(value, label)| (value, label.to_owned()));
                _ = ui43.add(SelectValueLabeled::new(&mut self.property_blend, &entries));
            });
            _ = PropertyRow::new("Visible").show(ui41, |ui44| {
                _ = ui44.add(functora_egui::Switch::new(&mut self.form.form_billing).label("Show"));
            });
            _ = PropertyRow::new("Actions").show(ui41, |ui45| {
                _ = Flex::row().gap(8.0).show(ui45, |f| {
                    _ = f.add(
                        Button::new("Reset")
                            .variant(ButtonVariant::Outline)
                            .size(functora_egui::ComponentSize::Sm),
                    );
                    _ = f.add(
                        Button::new("Apply")
                            .size(functora_egui::ComponentSize::Sm)
                            .icon(functora_egui::LucideIcon::Check),
                    );
                });
            });
        });

        snippet(
            ui,
            "// PropertyRow: single labeled row (text, badges, inputs, switches)\nuse functora_egui::{PropertyGrid, PropertyRow, Badge, BadgeVariant, SelectValueLabeled, Switch, Flex, Button, ButtonVariant, LucideIcon, ComponentSize};\n\n#[derive(Clone, Copy, PartialEq)]\nenum BlendMode { Normal, Multiply, Screen, Overlay }\n\nPropertyGrid::new().label_width(96.0).show(ui, |grid| {\n    PropertyRow::new(\"Mode\").show(grid, |row| {\n        Badge::new(\"Auto\").variant(BadgeVariant::Secondary).show(row);\n    });\n    PropertyRow::new(\"Blend\").show(grid, |row| {\n        let entries = [(BlendMode::Normal, \"Normal\".to_owned()), (BlendMode::Multiply, \"Multiply\".to_owned()), (BlendMode::Screen, \"Screen\".to_owned()), (BlendMode::Overlay, \"Overlay\".to_owned())];\n        SelectValueLabeled::new(&mut blend, &entries).show(row);\n    });\n    PropertyRow::new(\"Visible\").show(grid, |row| {\n        Switch::new(&mut show).label(\"Show\").show(row);\n    });\n    PropertyRow::new(\"Actions\").show(grid, |row| {\n        Flex::row().gap(8.0).show(row, |f| {\n            f.add(Button::new(\"Reset\").variant(ButtonVariant::Outline).size(ComponentSize::Sm));\n            f.add(Button::new(\"Apply\").size(ComponentSize::Sm).icon(LucideIcon::Check));\n        });\n    });\n});",
        );
    }
}
