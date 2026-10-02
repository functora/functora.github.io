use functora_egui::snippet;
use functora_egui::{Flex, Hyperlink, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_hyperlink(ui: &mut egui::Ui) {
        _ = Typography::muted("A single inline link styled with the theme's primary color.")
            .show(ui);
        ui.add_space(12.0);
        _ = Flex::row().gap(12.0).wrap().show(ui, |f| {
            _ = f.add(
                Hyperlink::new("functora-egui").url("https://github.com/functora/functora-egui"),
            );
            _ = f.add(Hyperlink::new("Docs").url("https://docs.rs/functora-egui"));
            _ = f.add(
                Hyperlink::new("Same tab")
                    .url("https://functora.github.io/")
                    .open_in_new_tab(false),
            );
        });
        ui.add_space(4.0);
        _ = Typography::small("Links open in a new tab unless disabled.").show(ui);

        snippet(
            ui,
            "// Hyperlink: single inline link styled with the theme\nuse functora_egui::Hyperlink;\n\nHyperlink::new(\"functora-egui\")\n    .url(\"https://github.com/functora/functora-egui\")\n    .show(ui);\n\n// Same tab\nHyperlink::new(\"Docs\")\n    .url(\"https://docs.rs/functora-egui\")\n    .open_in_new_tab(false)\n    .show(ui);",
        );
    }
}
