use functora_egui::snippet;
use functora_egui::{Hypertext, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_hypertext(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("A paragraph with inline links and internal action segments.")
            .show(ui);
        ui.add_space(12.0);
        let (_, action) = Hypertext::new()
            .text("Read the ")
            .link("docs", "https://docs.rs/functora-egui")
            .text(" or continue with the ")
            .action("onboarding guide", "onboarding")
            .text(" to get started.")
            .show_action(ui);
        if let Some(id) = action {
            self.toast.add(
                format!("Hypertext action: {id}"),
                functora_egui::ToastVariant::Default,
                ui.ctx().input(|i| i.time),
            );
        }
        ui.add_space(4.0);
        _ = Typography::small("Link segments open a URL; action segments report their id.")
            .show(ui);
        ui.add_space(12.0);
        _ = Hypertext::new()
            .text(format!(
                "\u{a9} {} Functora. ",
                functora_egui::FUNCTORA_CORE_YEAR
            ))
            .link("Functora", "https://functora.github.io/")
            .size(11.0)
            .centered()
            .show(ui);

        snippet(
            ui,
            "// Hypertext: paragraph with inline links and internal actions\nuse functora_egui::Hypertext;\n\nlet (_, action) = Hypertext::new()\n    .text(\"Read the \")\n    .link(\"docs\", \"https://docs.rs/functora-egui\")\n    .text(\" or continue with the \")\n    .action(\"onboarding guide\", \"onboarding\")\n    .text(\" to get started.\")\n    .show_action(ui);\n\nif let Some(id) = action {\n    eprintln!(\"action: {id}\");\n}\n\n// Uniform size for mixed paragraphs\nHypertext::new()\n    .text(format!(\"\u{a9} {} Functora. \", functora_egui::FUNCTORA_CORE_YEAR))\n    .link(\"Functora\", \"https://functora.github.io/\")\n    .size(11.0)\n    .centered()\n                .show(ui);",
        );
    }
}
