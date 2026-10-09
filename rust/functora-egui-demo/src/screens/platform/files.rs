use functora_egui::snippet;
use functora_egui::{
    Badge, BlockingOverlay, Button, ButtonVariant, Card, Flex, Label, Separator, ShadcnThemeExt,
    ToastVariant, Typography,
};

impl crate::state::ShowcaseApp {
    pub fn demo_files(&mut self, ui: &mut egui::Ui) {
        let lang = self.persistent.language;
        if let Some(cancel) = self.platform.pick_cancel.clone() {
            let mut open = self.platform.pick_overlay_open;
            BlockingOverlay::new("Uploading...")
                .description("Reading files, please wait. You can cancel if needed.")
                .show(
                    ui.ctx(),
                    &mut open,
                    self.platform.pick_job.as_ref(),
                    &cancel,
                    lang,
                );
            self.platform.pick_overlay_open = open;
        }
        _ = Typography::muted(
            "Files: `pick_files` via rfd (desktop) / Intent (Android) / input (web). Preview via `preview`/`preview_blob`, mime via `mime_for`.",
        )
        .show(ui);
        ui.add_space(12.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            let picking = self.platform.pick_rx.is_some();
            if f.add(
                Button::new(if picking { "Picking..." } else { "Pick files" })
                    .icon(functora_egui::LucideIcon::Files)
                    .enabled(!picking),
            )
            .inner
            .clicked()
            {
                let cancel = functora_egui::files::new_cancel_token();
                let progress = std::sync::Arc::new(std::sync::Mutex::new(None));
                self.platform.pick_cancel = Some(std::sync::Arc::clone(&cancel));
                self.platform.pick_progress = Some(std::sync::Arc::clone(&progress));
                self.platform.pick_overlay_open = true;
                self.platform.pick_job = None;
                let rx = functora_egui::spawn_async(async move {
                    functora_egui::files::pick_files_with_shared_progress(
                        true,
                        Some(progress),
                        Some(&cancel),
                    )
                    .await
                });
                self.platform.pick_rx = Some(rx);
            }
            if f.add(Button::new("Clear").variant(ButtonVariant::Outline))
                .inner
                .clicked()
            {
                self.platform.picked.clear();
            }
        });
        if self.platform.picked.is_empty() {
            ui.add_space(8.0);
            _ = Typography::small("No files picked yet.").show(ui);
        } else {
            ui.add_space(12.0);
            _ = Typography::small(format!("{} file(s) picked", self.platform.picked.len()))
                .show(ui);
            ui.add_space(8.0);
            for (name, data) in &self.platform.picked {
                let preview = functora_egui::files::preview(name, data);
                let mime = functora_egui::files::mime_for_name(name).unwrap_or("unknown");
                let size = functora_egui::files::format_size(data.len() as u64);
                _ = Card::new().show(ui, |ui2| {
                    _ = Flex::column().gap(4.0).align_start().show(ui2, |f| {
                        _ = f.ui(|ui3| {
                            _ = Typography::small(format!("{name} ({mime}, {size})")).show(ui3);
                        });
                        _ = f.ui(|ui3| match preview {
                            functora_egui::files::Preview::Text(ref t) => {
                                _ = Label::new(t.chars().take(200).collect::<String>()).show(ui3);
                            }
                            functora_egui::files::Preview::Markdown(ref t) => {
                                _ = functora_egui::markdown_view::show(
                                    ui3,
                                    &mut self.platform.md_cache,
                                    t,
                                );
                            }
                            functora_egui::files::Preview::Image(_) => {
                                let theme = ui3.ctx().shadcn_theme();
                                let clicked = ui3
                                    .add(
                                        egui::Image::from_bytes(
                                            format!("bytes://{name}"),
                                            data.clone(),
                                        )
                                        .bg_fill(theme.secondary)
                                        .sense(egui::Sense::click())
                                        .max_width(220.0)
                                        .max_height(220.0),
                                    )
                                    .on_hover_text(if name.is_empty() {
                                        "Image preview".to_owned()
                                    } else {
                                        format!("Click to {name}")
                                    })
                                    .clicked();
                                if clicked {
                                    self.toast.add(
                                        format!("Image: {name}"),
                                        functora_egui::ToastVariant::Default,
                                        ui3.ctx().input(|i| i.time),
                                    );
                                }
                                _ = Typography::small(format!("Image: {name} ({size})")).show(ui3);
                            }
                            functora_egui::files::Preview::Video(ref url) => {
                                _ = Typography::small(format!(
                                    "Video: {}...",
                                    Self::truncate_preview_url(url, 60)
                                ))
                                .show(ui3);
                                _ = Typography::small(format!("Video file: {name} ({size})"))
                                    .show(ui3);
                            }
                            functora_egui::files::Preview::Audio(_)
                            | functora_egui::files::Preview::Pdf(_)
                            | functora_egui::files::Preview::Missing => {
                                _ = Typography::small(format!("{preview:?}")).show(ui3);
                            }
                            functora_egui::files::Preview::Download => {
                                _ = ui3.add(Badge::new("Download"));
                                _ = Typography::small(format!("Ready to download: {name}"))
                                    .show(ui3);
                            }
                        });
                    });
                });
                ui.add_space(8.0);
            }
        }
        ui.add_space(12.0);
        let _ = Separator::horizontal().show(ui);
        ui.add_space(8.0);
        _ = Typography::small("Blob memo cache demo").show(ui);
        ui.add_space(4.0);
        if ui.add(Button::new("Create revokable blob (txt)")).clicked() {
            let preview = functora_egui::files::preview_blob("hello.txt", b"hello blob");
            self.toast.add(
                format!("blob preview: {preview:?}"),
                ToastVariant::Default,
                ui.ctx().input(|i| i.time),
            );
        }

        snippet(
            ui,
            "// Files: pick + preview + mime\nuse functora_egui::files::{pick_files_with_shared_progress, new_cancel_token, preview, preview_blob, mime_for_name, format_size};\nuse std::sync::{Arc, Mutex};\n\n// Pick files with shared progress + cancel token (overlay reads progress)\nlet cancel = new_cancel_token();\nlet progress = Arc::new(Mutex::new(None));\nlet rx = functora_egui::spawn_async(async move {\n    pick_files_with_shared_progress(true, Some(progress), Some(&cancel)).await\n});\n\n// Cancel from the overlay close button\n// functora_egui::files::cancel(&cancel);\n\nfor (name, data) in files {\n    // Get mime type\n    let mime = mime_for_name(&name).unwrap_or(\"application/octet-stream\");\n    \n    // Preview (text/image/pdf)\n    let preview = preview(&name, &data);\n    \n    // Or create a revocable blob URL (web)\n    let blob_url = preview_blob(&name, &data);\n    \n    let size = format_size(data.len() as u64);\n    eprintln!(\"picked: {name} ({mime}, {size})\");\n}",
        );
    }
}
