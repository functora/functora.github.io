use functora_egui::snippet;
use functora_egui::{Flex, Kbd, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_kbd(ui: &mut egui::Ui) {
        _ = Typography::muted("Keyboard hint chips for shortcuts.").show(ui);
        ui.add_space(12.0);
        _ = Flex::row().gap(6.0).align_center().show(ui, |f| {
            _ = f.add(Kbd::new("Ctrl"));
            _ = f.ui(|ui55| {
                _ = ui55.label("+");
            });
            _ = f.add(Kbd::new("K"));
            _ = f.ui(|ui56| {
                _ = ui56.label("opens the command palette");
            });
        });
        ui.add_space(12.0);
        _ = Flex::row().gap(6.0).align_center().show(ui, |f| {
            _ = f.add(Kbd::new("Shift"));
            _ = f.ui(|ui57| {
                _ = ui57.label("+");
            });
            _ = f.add(Kbd::new("Tab"));
            _ = f.ui(|ui58| {
                _ = ui58.label("cycles focus");
            });
        });

        snippet(
            ui,
            "// Kbd: keyboard hint chips\nuse functora_egui::{Kbd, Flex};\n\nFlex::row().gap(6.0).align_center().show(ui, |f| {\n    f.add(Kbd::new(\"Ctrl\"));\n    f.ui(|ui| { ui.label(\"+\"); });\n    f.add(Kbd::new(\"K\"));\n    f.ui(|ui| { ui.label(\"opens the command palette\"); });\n});\n\nFlex::row().gap(6.0).align_center().show(ui, |f| {\n    f.add(Kbd::new(\"Shift\"));\n    f.ui(|ui| { ui.label(\"+\"); });\n    f.add(Kbd::new(\"Tab\"));\n    f.ui(|ui| { ui.label(\"cycles focus\"); });\n});",
        );
    }
}
