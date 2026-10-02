use functora_egui::snippet;
use functora_egui::{Button, Flex, ToastVariant, Typography, spawn_async};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_zip(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted(
            "Zip: zip::create_zip_async / unzip_async over the picked files from Files demo, then verify_zip_roundtrip compares names and bytes.",
        )
        .show(ui);
        ui.add_space(12.0);
        _ = Typography::small(format!(
            "Picked files for zip: {} (from Files)",
            self.platform.picked.len()
        ))
        .show(ui);
        ui.add_space(8.0);
        let ctx = ui.ctx().clone();
        _ = Flex::row().gap(8.0).show(ui, |f| {
            let busy = self.platform.zip_rx.is_some();
            if f.add(
                Button::new(if busy {
                    "Zipping..."
                } else {
                    "Create zip + verify"
                })
                .enabled(!busy),
            )
            .inner
            .clicked()
            {
                let files = self.platform.picked.clone();
                if files.is_empty() {
                    self.toast.add(
                        "No files picked (go to Files)",
                        ToastVariant::Error,
                        ctx.input(|i| i.time),
                    );
                } else {
                    self.platform.zip_rx = Some(spawn_async(async move {
                        Self::zip_roundtrip_async(files).await
                    }));
                }
            }
        });

        snippet(
            ui,
            "// Zip: create_zip_async + unzip_async + verify_zip_roundtrip\nuse functora_egui::zip::{create_zip_async, unzip_async};\nuse functora_egui::progress::Stage;\nuse functora_egui::files::Attachment;\n\nlet attachments = picked\n    .iter()\n    .map(|(name, data)| Attachment { name: name.clone(), data: data.clone().into() })\n    .collect::<Vec<_>>();\n\nlet zipped = create_zip_async(&attachments, |_| {}, Stage::Zip).await?;\nlet unzipped = unzip_async(zipped, |_| {}, Stage::Unzip).await?;\nlet summary = Self::verify_zip_roundtrip(&picked, unzipped)?;\n// \"Zip ok: 2 files, 42 bytes, round-trip verified\"",
        );
    }
}
