use functora_egui::snippet;
use functora_egui::{Button, Flex, Typography, spawn_async};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_worker(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted(
            "Worker: worker::run – runs future on thread (desktop) or inline (wasm) with Reporter<Stage> progress.",
        )
        .show(ui);
        ui.add_space(12.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            let busy = self.platform.worker_rx.is_some();
            if f.add(Button::new(if busy { "Working..." } else { "Start worker" }).enabled(!busy))
                .inner
                .clicked()
            {
                self.platform.worker_rx = Some(spawn_async(async move {
                    functora_egui::worker::run(
                        42u32,
                        |_| {},
                        |val, mut reporter| async move {
                            reporter(functora_egui::progress::Job {
                                stage: functora_egui::progress::Stage::Download,
                                done: 1,
                                total: 1,
                                name: None,
                            });
                            Ok::<String, functora_egui::error::Error>(format!("Worker done: {val}"))
                        },
                    )
                    .await
                    .map_err(|e| e.to_string())
                }));
            }
        });
        ui.add_space(8.0);
        _ = Typography::small("Check ProgressWorker demo for Job<Stage> progress details.")
            .show(ui);

        snippet(
            ui,
            "// Worker: run async work on thread (desktop) or inline (wasm) with progress\nuse functora_egui::worker::run;\nuse functora_egui::progress::{Job, Stage};\n\nlet input = 42u32;\n\nlet result = run(\n    input,\n    |_job| { /* setup */ },\n    |val, mut reporter| async move {\n        // Report progress\n        reporter(Job {\n            stage: Stage::Download,\n            done: 1,\n            total: 1,\n            name: Some(\"task\".to_owned()),\n        });\n        \n        // Do async work\n        let output = format!(\"Worker done: {val}\");\n        Ok(output)\n    },\n).await?;\n\n// result: String",
        );
    }
}
