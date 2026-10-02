use functora_egui::snippet;
use functora_egui::{Button, ButtonVariant, Flex, Progress, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_progress_worker(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted(
            "Progress Job + Worker::run (thread on desktop, inline on wasm) with Stage enum.",
        )
        .show(ui);
        ui.add_space(12.0);
        if let Some(job) = &self.platform.progress_job {
            _ = ui.add(Progress::new(f32::from(job.percent()) / 100.0));
            ui.add_space(4.0);
            _ = Typography::small(format!(
                "Stage: {:?} {} / {} ({}%)",
                job.stage,
                job.done,
                job.total,
                job.percent()
            ))
            .show(ui);
            if let Some(name) = &job.name {
                _ = Typography::small(format!("file: {name}")).show(ui);
            }
        } else {
            _ = Typography::small("No job running").show(ui);
        }
        ui.add_space(8.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            let running = self.platform.progress_running;
            if f.add(
                Button::new("Start fake job")
                    .icon(functora_egui::LucideIcon::Play)
                    .enabled(!running),
            )
            .inner
            .clicked()
            {
                self.platform.progress_running = true;
                self.platform.progress_job = Some(functora_egui::progress::Job {
                    stage: functora_egui::progress::Stage::Zip,
                    done: 0,
                    total: 100,
                    name: None,
                });
            }
            if f.add(
                Button::new("Tick")
                    .variant(ButtonVariant::Outline)
                    .enabled(running),
            )
            .inner
            .clicked()
                && let Some(job) = &mut self.platform.progress_job
            {
                job.done = (job.done + 10).min(job.total);
                if job.done >= job.total {
                    self.platform.progress_running = false;
                }
            }
            if f.add(Button::new("Clear").variant(ButtonVariant::Outline))
                .inner
                .clicked()
            {
                self.platform.progress_job = None;
                self.platform.progress_running = false;
            }
        });
        ui.add_space(8.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            if f.add(Button::new("Claim guard demo")).inner.clicked() {
                let mut slot = self.platform.progress_job.clone();
                let is_claimed = functora_egui::progress::claim_job(
                    &mut slot,
                    functora_egui::progress::Stage::Download,
                )
                .is_some();
                if is_claimed {
                    self.platform.progress_job = Some(functora_egui::progress::Job {
                        stage: functora_egui::progress::Stage::Download,
                        done: 0,
                        total: 1,
                        name: None,
                    });
                }
            }
        });

        snippet(
            ui,
            "// Progress: Job<Stage> + claim_job for exclusive access\nuse functora_egui::Progress;\nuse functora_egui::progress::{Job, Stage, claim_job};\n\nlet mut slot: Option<Job<Stage>> = None;\n\n// Start a fake job\nslot = Some(Job { stage: Stage::Zip, done: 0, total: 100, name: None });\n\n// Tick\nif let Some(job) = &mut slot {\n    job.done = (job.done + 10).min(job.total);\n}\n\n// Claim guard demo: clone-then-claim for exclusive access\nlet mut other = slot.clone();\nif claim_job(&mut other, Stage::Download).is_some() {\n    other = Some(Job { stage: Stage::Download, done: 0, total: 1, name: None });\n}\n\n// Render progress bar\nif let Some(job) = &slot {\n    Progress::new(f32::from(job.percent()) / 100.0).show(ui);\n}\n\n// Clear\nslot = None;",
        );
    }
}
