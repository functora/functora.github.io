use functora_egui::snippet;
use functora_egui::{Footer, Hypertext};

impl crate::state::ShowcaseApp {
    pub fn demo_footer(ui: &mut egui::Ui) {
        crate::screens::intro(ui, "Centered, muted footer for page bottoms.");
        _ = Footer::new().show(ui, |inner| {
            _ = Hypertext::new()
                .text("© 2026 Functora. ")
                .link("Privacy", "https://functora.github.io/")
                .text(" · ")
                .link("Terms", "https://functora.github.io/")
                .show(inner);
        });

        snippet(
            ui,
            "// Footer: centered muted footer\nuse functora_egui::{Footer, Hypertext};\n\nFooter::new().show(ui, |inner| {\n    Hypertext::new()\n        .text(\"© 2026 Functora. \")\n        .link(\"Privacy\", \"https://functora.github.io/\")\n        .text(\" · \")\n        .link(\"Terms\", \"https://functora.github.io/\")\n        .show(inner);\n});",
        );
    }
}
