use functora_egui::snippet;
use functora_egui::{Badge, BadgeVariant, Kbd, Separator, StatusBar, Typography};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_status_bar(ui: &mut egui::Ui) {
        _ = Typography::muted("Compact container for workspace state and metadata.").show(ui);
        ui.add_space(12.0);
        _ = StatusBar::new().show(ui, |ui62| {
            _ = Badge::new("Saved")
                .variant(BadgeVariant::Secondary)
                .show(ui62);
            _ = Separator::vertical().show(ui62);
            _ = Typography::small("Canvas 1920 x 1080").show(ui62);
            _ = Separator::vertical().show(ui62);
            _ = Typography::small("2 objects selected").show(ui62);
            _ = Separator::vertical().show(ui62);
            _ = Kbd::new("Cmd").show(ui62);
            _ = ui62.label("+");
            _ = Kbd::new("S").show(ui62);
        });
        ui.add_space(12.0);
        _ = StatusBar::new().dense().show(ui, |ui63| {
            _ = Typography::small("x: 124").show(ui63);
            _ = Typography::small("y: 88").show(ui63);
            _ = Typography::small("rotation: -8deg").show(ui63);
            _ = Badge::new("Snapping")
                .variant(BadgeVariant::Outline)
                .show(ui63);
        });

        snippet(
            ui,
            "// StatusBar: compact container for workspace state\nuse functora_egui::{StatusBar, Badge, BadgeVariant, Separator, Kbd, Typography};\n\nStatusBar::new().show(ui, |bar| {\n    Badge::new(\"Saved\").variant(BadgeVariant::Secondary).show(bar);\n    Separator::vertical().show(bar);\n    Typography::small(\"Canvas 1920 x 1080\").show(bar);\n    Separator::vertical().show(bar);\n    Typography::small(\"2 objects selected\").show(bar);\n    Separator::vertical().show(bar);\n    Kbd::new(\"Cmd\").show(bar);\n    bar.label(\"+\");\n    Kbd::new(\"S\").show(bar);\n});\n\n// Dense variant\nStatusBar::new().dense().show(ui, |bar| {\n    Typography::small(\"x: 124\").show(bar);\n    Typography::small(\"y: 88\").show(bar);\n    Typography::small(\"rotation: -8deg\").show(bar);\n    Badge::new(\"Snapping\").variant(BadgeVariant::Outline).show(bar);\n});",
        );
    }
}
