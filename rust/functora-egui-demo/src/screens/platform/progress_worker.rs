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
        let ctx = ui.ctx().clone();
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
            if f.add(Button::new("Yield to paint").variant(ButtonVariant::Outline))
                .inner
                .clicked()
            {
                let repaint = ctx.clone();
                self.platform.progress_running = true;
                drop(functora_egui::spawn_async(async move {
                    functora_egui::progress::yield_to_paint().await;
                    functora_egui::progress::YieldOnce::new(false).await;
                    repaint.request_repaint();
                }));
            }
        });
        ui.add_space(4.0);
        _ = Typography::small(format!(
            "Stage enum: {:?}",
            [
                functora_egui::progress::Stage::Attach,
                functora_egui::progress::Stage::Zip,
                functora_egui::progress::Stage::Encrypt,
                functora_egui::progress::Stage::Decrypt,
                functora_egui::progress::Stage::Unzip,
                functora_egui::progress::Stage::Download,
                functora_egui::progress::Stage::Preview,
            ]
            .iter()
            .map(|stage| format!("{stage:?}"))
            .collect::<Vec<_>>()
            .join(", ")
        ))
        .show(ui);

        ui.add_space(12.0);
        crate::screens::section(ui, "Progress reporting helpers");
        let mut slot: Option<functora_egui::progress::Job<functora_egui::progress::Stage>> = None;
        functora_egui::progress::report(&mut slot, functora_egui::progress::Stage::Preview, 1, 4);
        _ = Typography::small(format!(
            "report -> {:?} ({} / {})",
            slot.as_ref().map(|job| job.stage),
            slot.as_ref().map_or(0, |job| job.done),
            slot.as_ref().map_or(0, |job| job.total)
        ))
        .show(ui);
        let repaint_ctx = ui.ctx().clone();
        drop(functora_egui::spawn_async(async move {
            functora_egui::progress::report_progress(
                &mut slot,
                functora_egui::progress::Stage::Preview,
                2,
                4,
            )
            .await;
            functora_egui::progress::report_progress_named(
                &mut slot,
                functora_egui::progress::Stage::Preview,
                3,
                4,
                "photo.png",
            )
            .await;
            functora_egui::progress::clear_progress(&mut slot);
            repaint_ctx.request_repaint();
        }));
        _ = Typography::small(
            "report_progress / report_progress_named / clear_progress run on the worker.",
        )
        .show(ui);

        ui.add_space(12.0);
        crate::screens::section(ui, "InFlight: single-owner guard");
        let in_flight = functora_egui::InFlight::new();
        let first = in_flight.claim();
        _ = Typography::small(format!(
            "claimed: {}, is_in_flight: {}",
            first.is_some(),
            in_flight.is_in_flight()
        ))
        .show(ui);
        let second = in_flight.claim();
        _ = Typography::small(format!(
            "second claim while the guard is alive: {}",
            second.is_some()
        ))
        .show(ui);
        drop(first);
        _ = Typography::small(format!(
            "after dropping the InFlightGuard, is_in_flight: {}",
            in_flight.is_in_flight()
        ))
        .show(ui);

        snippet(
            ui,
            "// Progress: Job<Stage> + claim_job for exclusive access\nuse functora_egui::Progress;\nuse functora_egui::progress::{Job, Stage, claim_job};\n\nlet mut slot: Option<Job<Stage>> = None;\n\n// Start a fake job\nslot = Some(Job { stage: Stage::Zip, done: 0, total: 100, name: None });\n\n// Tick\nif let Some(job) = &mut slot {\n    job.done = (job.done + 10).min(job.total);\n}\n\n// Claim guard demo: clone-then-claim for exclusive access\nlet mut other = slot.clone();\nif let Some(_guard) = claim_job(&mut other, Stage::Download) {\n    other = Some(Job { stage: Stage::Download, done: 0, total: 1, name: None });\n}\n\n// claim_job returns a JobGuard that owns the slot for as long as it lives\n\n// Render progress bar\nif let Some(job) = &slot {\n    Progress::new(f32::from(job.percent()) / 100.0).show(ui);\n}\n\n// Clear\nslot = None;\n\n// Report helpers: report writes synchronously, the async ones also yield to paint\nuse functora_egui::progress::{clear_progress, report, report_progress, report_progress_named};\n\nreport(&mut slot, Stage::Preview, 1, 4);\nreport_progress(&mut slot, Stage::Preview, 2, 4).await;\nreport_progress_named(&mut slot, Stage::Preview, 3, 4, \"photo.png\").await;\nclear_progress(&mut slot);\n\n// Long-running work yields to the paint loop between chunks\nuse functora_egui::progress::{YieldOnce, yield_to_paint};\n\nfor chunk in chunks {\n    process(chunk)?;\n    yield_to_paint().await;      // let the UI paint again\n    YieldOnce::new(false).await; // one extra scheduling point\n\n// Only one worker may run at a time\nuse functora_egui::{InFlight, InFlightGuard};\n\nlet in_flight = InFlight::new();\nif let Some(_guard) = in_flight.claim() {\n    // the InFlightGuard releases the slot when it drops\n}",
        );
    }
}
