use functora_egui::snippet;
use functora_egui::{Button, Flex, LucideIcon, Spinner, Typography};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_spinner(ui: &mut egui::Ui) {
        _ = Typography::muted("An animated loading indicator with sizes.").show(ui);
        ui.add_space(12.0);
        _ = Flex::row().gap(16.0).align_center().show(ui, |f| {
            _ = f.ui(|ui59| {
                _ = Spinner::new().size(16.0).show(ui59);
            });
            _ = f.ui(|ui60| {
                _ = Spinner::new().size(24.0).show(ui60);
            });
            _ = f.ui(|ui61| {
                _ = Spinner::new().size(32.0).show(ui61);
            });
            _ = f.ui(|ui62| {
                _ = Spinner::new().size(48.0).show(ui62);
            });
        });
        ui.add_space(12.0);
        _ = Typography::small("Inside a button").show(ui);
        ui.add_space(4.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            _ = f.add(
                Button::new("Loading")
                    .icon(LucideIcon::LoaderCircle)
                    .enabled(false),
            );
        });

        snippet(
            ui,
            "// Spinner: animated loading indicator\nuse functora_egui::{Spinner, Button, LucideIcon, Flex};\n\nFlex::row().gap(16.0).align_center().show(ui, |f| {\n    f.ui(|ui| { Spinner::new().size(16.0).show(ui); });\n    f.ui(|ui| { Spinner::new().size(24.0).show(ui); });\n    f.ui(|ui| { Spinner::new().size(32.0).show(ui); });\n    f.ui(|ui| { Spinner::new().size(48.0).show(ui); });\n});\n\n// Inside a button\nButton::new(\"Loading\")\n    .icon(LucideIcon::LoaderCircle)\n    .enabled(false)\n    .show(ui);",
        );
    }
}
