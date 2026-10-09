use functora_egui::progress::{Job, Stage};
use functora_egui::snippet;
use functora_egui::{BlockingOverlay, Button, ButtonVariant, LucideIcon, Typography};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

impl crate::state::ShowcaseApp {
    pub fn demo_blocking_overlay(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted("A modal overlay that blocks interaction during long operations.")
            .show(ui);
        ui.add_space(12.0);
        if Button::new("Show Blocking Overlay")
            .icon(LucideIcon::Hourglass)
            .variant(ButtonVariant::Outline)
            .show(ui)
            .clicked()
        {
            self.demo.blocking_overlay_open = true;
            self.blocking_overlay_cancel = Arc::new(AtomicBool::new(false));
            self.blocking_overlay_job = Some(Job {
                stage: Stage::Download,
                done: 45,
                total: 100,
                name: Some("archive.zip".to_owned()),
            });
        }
        ui.add_space(4.0);
        _ = Typography::small("Click Cancel inside the overlay to dismiss it.").show(ui);

        if self.demo.blocking_overlay_open {
            let mut open = self.demo.blocking_overlay_open;
            BlockingOverlay::new("Processing files...")
                .description("Reading and compressing files, please wait.")
                .show(
                    ui.ctx(),
                    &mut open,
                    self.blocking_overlay_job.as_ref(),
                    &self.blocking_overlay_cancel,
                    self.persistent.language,
                );
            if self.blocking_overlay_cancel.load(Ordering::Relaxed) {
                open = false;
                self.blocking_overlay_job = None;
            }
            self.demo.blocking_overlay_open = open;
        }

        snippet(
            ui,
            "// BlockingOverlay: modal overlay for long operations\nuse functora_egui::BlockingOverlay;\nuse functora_egui::progress::{Job, Stage};\nuse std::sync::Arc;\nuse std::sync::atomic::{AtomicBool, Ordering};\n\nlet mut open = false;\nlet cancel = Arc::new(AtomicBool::new(false));\nlet job = Job { stage: Stage::Download, done: 45, total: 100, name: Some(\"archive.zip\".to_owned()) };\n\nBlockingOverlay::new(\"Processing files...\")\n    .description(\"Reading and compressing files, please wait.\")\n    .show(ctx, &mut open, Some(&job), &cancel, lang);\n\n// Close when cancelled\nif cancel.load(Ordering::Relaxed) {\n    open = false;\n}",
        );
    }
}
