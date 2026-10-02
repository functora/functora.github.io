use functora_egui::snippet;
use functora_egui::{Card, Label, Textarea, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_markdown(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted(
            "Markdown: CommonMarkViewer + CommonMarkCache (egui_commonmark) renders raw source to native widgets. Opt-in feature `markdown`.",
        )
        .show(ui);
        ui.add_space(12.0);
        _ = Label::new("Source").show(ui);
        ui.add_space(8.0);
        _ = Textarea::new(&mut self.platform.md_source)
            .placeholder("# Hello")
            .desired_width(ui.available_width())
            .show(ui);
        ui.add_space(8.0);
        let preview_width = ui.available_width();
        _ = Card::new().show(ui, |ui2| {
            ui2.set_min_width((preview_width - 32.0).max(0.0));
            _ = functora_egui::markdown_view::show(
                ui2,
                &mut self.platform.md_cache,
                &self.platform.md_source,
            );
        });

        snippet(
            ui,
            "// Markdown: theme-aware rendered CommonMark\nuse functora_egui::CommonMarkCache;\nuse functora_egui::markdown_view;\n\n// Cache (persist across frames)\nlet mut cache = CommonMarkCache::default();\n\n// Render every frame (maps shadcn theme onto egui visuals)\nmarkdown_view::show(ui, &mut cache, \"# Hello\\n\\nThis is **bold**.\");",
        );
    }
}
