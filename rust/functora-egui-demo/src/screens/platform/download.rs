use functora_egui::snippet;
use functora_egui::{Button, Input, Textarea, Typography, spawn_async};

impl crate::state::ShowcaseApp {
    pub(crate) fn demo_download(&mut self, ui: &mut egui::Ui) {
        _ = Typography::muted(
            "Download via Blob+anchor (web), rfd save dialog (desktop), MediaStore Downloads (Android).",
        )
        .show(ui);
        ui.add_space(12.0);
        _ = ui.add(Input::new(&mut self.platform.download_name).placeholder("hello.txt"));
        ui.add_space(4.0);
        _ = ui.add(
            Textarea::new(&mut self.platform.download_text)
                .placeholder("file contents")
                .desired_width(ui.available_width()),
        );
        ui.add_space(8.0);
        let downloading = self.platform.download_rx.is_some();
        if ui
            .add_enabled(
                !downloading,
                Button::new(if downloading {
                    "Downloading..."
                } else {
                    "Download"
                })
                .icon(functora_egui::LucideIcon::Download),
            )
            .clicked()
        {
            let name = self.platform.download_name.clone();
            let data = self.platform.download_text.clone().into_bytes();
            self.platform.download_rx = Some(spawn_async(async move {
                functora_egui::download::download(data, &name)
                    .await
                    .map_err(|e| e.to_string())
            }));
        }
        snippet(
            ui,
            "// Download: Blob + anchor (web) / save dialog (desktop) / MediaStore (Android)\nuse functora_egui::download::download;\n\nlet data = b\"hello, world!\";\nlet filename = \"hello.txt\";\n\n// Simple one-liner\ndownload(data, filename).await?;\n\n// Or with bytes:\n// download(data.to_vec(), filename).await?;",
        );
    }
}
