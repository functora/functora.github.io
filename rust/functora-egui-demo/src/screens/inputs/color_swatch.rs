use crate::catalog::Swatch;
use functora_egui::snippet;
use functora_egui::{ColorSwatch, Flex, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_color_swatch(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("Clickable color swatches for palettes and style controls.").show(ui);
        ui.add_space(12.0);

        _ = Typography::small("Palette").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(8.0).wrap().show(ui, |f| {
            for (swatch, label, color) in Swatch::ALL {
                if f.add(
                    ColorSwatch::new(color)
                        .label(label)
                        .selected(self.color_swatch == swatch)
                        .show_hex(),
                )
                .inner
                .clicked()
                {
                    self.color_swatch = swatch;
                }
            }
        });

        ui.add_space(12.0);
        _ = Typography::small("Compact states").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            _ = f.add(ColorSwatch::new(egui::Color32::from_rgb(25, 113, 194)).selected(true));
            _ = f.add(ColorSwatch::new(egui::Color32::from_rgba_unmultiplied(
                25, 113, 194, 120,
            )));
            _ = f.add(
                ColorSwatch::new(egui::Color32::TRANSPARENT)
                    .label("Transparent")
                    .show_hex(),
            );
        });

        snippet(
            ui,
            "// ColorSwatch: clickable color swatches bound to an enum\nuse functora_egui::ColorSwatch;\nuse egui::Color32;\n\n#[derive(Clone, Copy, PartialEq)]\nenum Swatch { Signal, Mint, Amber, Rose, Ink }\n\nlet palette = [(Swatch::Signal, \"Signal\", Color32::from_rgb(25, 113, 194)), (Swatch::Mint, \"Mint\", Color32::from_rgb(18, 184, 134)), (Swatch::Amber, \"Amber\", Color32::from_rgb(245, 159, 0)), (Swatch::Rose, \"Rose\", Color32::from_rgb(224, 49, 49)), (Swatch::Ink, \"Ink\", Color32::from_rgb(33, 37, 41))];\nlet mut swatch = Swatch::Signal;\n\nfor (value, label, color) in palette {\n    if ColorSwatch::new(color)\n        .label(label)\n        .selected(swatch == value)\n        .show_hex()\n        .show(ui)\n        .clicked()\n    {\n        swatch = value;\n    }\n}\n\n// Compact states\nColorSwatch::new(Color32::from_rgb(25, 113, 194)).selected(true).show(ui);\nColorSwatch::new(Color32::from_rgba_unmultiplied(25, 113, 194, 120)).show(ui);\nColorSwatch::new(Color32::TRANSPARENT).label(\"Transparent\").show_hex().show(ui);",
        );
    }
}
