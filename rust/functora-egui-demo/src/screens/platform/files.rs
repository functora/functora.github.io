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
            "Files: `pick_files` via rfd (desktop) / Intent (Android) / input (web). Preview via `preview`/`preview_cached`/`preview_blob`, mime via `mime_for`/`mime_for_name`.",
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
                                _ = ui3.add(Badge::new("Save"));
                                _ = Typography::small(format!("Ready to save: {name}")).show(ui3);
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
        crate::screens::section(ui, "Blob memo cache demo");
        if ui.add(Button::new("Create revokable blob (txt)")).clicked() {
            let preview = functora_egui::files::preview_blob("hello.txt", b"hello blob");
            self.toast.add(
                format!("blob preview: {preview:?}"),
                ToastVariant::Default,
                ui.ctx().input(|i| i.time),
            );
        }
        ui.add_space(4.0);
        if ui
            .add(Button::new("Revoke last blob").variant(ButtonVariant::Outline))
            .clicked()
        {
            let mut memo = functora_egui::files::BlobMemo::default();
            _ = memo.insert("hello.txt", 1, "blob:https://example.com/1".to_owned());
            let removed = memo.forget("blob:https://example.com/1");
            let revoked = functora_egui::files::revoke_blob_url("blob:https://example.com/1");
            self.toast.add(
                format!("BlobMemo::forget removed {removed}, revoke: {revoked:?}"),
                ToastVariant::Default,
                ui.ctx().input(|i| i.time),
            );
        }
        ui.add_space(8.0);
        crate::screens::section(ui, "Picker state and cancel token");
        let cancel = functora_egui::files::new_cancel_token();
        _ = Typography::small(format!(
            "is_cancelled before: {}, after cancel: {}",
            functora_egui::files::is_cancelled(&cancel),
            {
                functora_egui::files::cancel(&cancel);
                functora_egui::files::is_cancelled(&cancel)
            }
        ))
        .show(ui);
        _ = Typography::small(format!(
            "FilePickerState::from(0): {:?}, from(-1): {:?}, from(7): {:?}",
            functora_egui::files::FilePickerState::from(0),
            functora_egui::files::FilePickerState::from(-1),
            functora_egui::files::FilePickerState::from(7)
        ))
        .show(ui);
        _ = Typography::small(format!(
            "data_url_mime(\"data:image/png;base64,AAA\"): {:?}",
            functora_egui::files::data_url_mime("data:image/png;base64,AAA")
        ))
        .show(ui);
        let _: Option<functora_egui::files::PickResult> = None;
        ui.add_space(8.0);
        crate::screens::section(ui, "Core mime + preview helpers");
        let sample = b"key: value\n".as_slice();
        _ = Flex::column().gap(4.0).align_start().show(ui, |f| {
            _ = f.ui(|ui2| {
                _ = Typography::small(format!(
                    "mime_for(\"a.json\"): {:?}",
                    functora_egui::files::mime_for("a.json")
                ))
                .show(ui2);
            });
            _ = f.ui(|ui2| {
                _ = Typography::small(format!(
                    "is_text(\"application/json\"): {}",
                    functora_egui::files::is_text("application/json")
                ))
                .show(ui2);
            });
            _ = f.ui(|ui2| {
                _ = Typography::small(format!(
                    "preview_key(len {}): {}",
                    sample.len(),
                    functora_egui::files::preview_key(sample)
                ))
                .show(ui2);
            });
            _ = f.ui(|ui2| {
                _ = Typography::small(format!(
                    "preview_cached(\"a.txt\"): {:?}",
                    functora_egui::files::preview_cached("a.txt", sample)
                ))
                .show(ui2);
            });
            _ = f.ui(|ui2| {
                _ = Typography::small(format!(
                    "preview_initial(\"a.txt\"): {:?}",
                    functora_egui::files::preview_initial("a.txt", sample)
                ))
                .show(ui2);
            });
            _ = f.ui(|ui2| {
                _ = Typography::small(format!(
                    "preview_blob_url(text preview): {:?}",
                    functora_egui::files::preview_blob_url(&functora_egui::files::preview(
                        "a.txt", sample
                    ))
                ))
                .show(ui2);
            });
        });

        snippet(
            ui,
            "// Files: pick + preview + mime\nuse functora_egui::files::{pick_files_with_shared_progress, new_cancel_token, preview, preview_blob, mime_for_name, format_size};\nuse std::sync::{Arc, Mutex};\n\n// Pick files with shared progress + cancel token (overlay reads progress)\nlet cancel = new_cancel_token();\nlet progress = Arc::new(Mutex::new(None));\nlet rx = functora_egui::spawn_async(async move {\n    pick_files_with_shared_progress(true, Some(progress), Some(&cancel)).await\n});\n\n// Cancel from the overlay close button\n// functora_egui::files::cancel(&cancel);\n\nfor (name, data) in files {\n    // Get mime type (egui alias keeps the historical name)\n    let mime = mime_for_name(&name).unwrap_or(\"application/octet-stream\");\n    \n    // Preview (text/image/pdf)\n    let preview = preview(&name, &data);\n    \n    // Or create a revocable blob URL (web)\n    let blob_url = preview_blob(&name, &data);\n    \n    let size = format_size(data.len() as u64);\n    eprintln!(\"picked: {name} ({mime}, {size})\");\n}\n\n// Core helpers: mime lookup, text detection, memoized preview\nuse functora_egui::files::{is_text, mime_for, preview_cached, preview_initial, preview_key, preview_blob_url};\n\nassert_eq!(mime_for(\"a.json\"), Some(\"application/json\"));\nassert!(is_text(\"application/json\"));\nlet key = preview_key(&data);              // cheap buffer identity for the memo\nlet cached = preview_cached(&name, &data); // memoized preview (same buffer -> same result)\nlet initial = preview_initial(&name, &data); // Some(..) for text, None for streamed media\nlet blob = preview_blob_url(&cached);       // Some(..) only for blob-backed media previews\n\n// Pick variants: with progress, with a cancel token, and the shared-progress one\nuse functora_egui::files::{pick_files_with_cancel, pick_files_with_progress};\n\nlet rx = functora_egui::spawn_async(async move {\n    pick_files_with_progress(true, None).await\n});\n\nlet cancel = new_cancel_token();\nlet rx = functora_egui::spawn_async(async move {\n    pick_files_with_cancel(true, None, Some(&cancel)).await\n});\n\n// The picker state the platform reports back\nuse functora_egui::files::FilePickerState;\n\nassert_eq!(FilePickerState::from(0), FilePickerState::Pending);\nassert_eq!(FilePickerState::from(-1), FilePickerState::Cancelled);\n\n// Blob URLs can be revoked, and the memo forgets them\nuse functora_egui::files::{BlobMemo, revoke_blob_url};\n\nlet mut memo = BlobMemo::default();\nmemo.insert(\"a.txt\", 1, \"blob:https://example.com/1\".to_owned());\nassert_eq!(memo.forget(\"blob:https://example.com/1\"), 1);\nrevoke_blob_url(\"blob:https://example.com/1\")?;\n\n// Data URLs carry their mime in the prefix\nuse functora_egui::files::data_url_mime;\n\nassert_eq!(data_url_mime(\"data:image/png;base64,AAA\"), Some(\"image/png\"));\n\n// Web only: the picker needs a user gesture, so the sync variant returns a slot\n#[cfg(target_arch = \"wasm32\")]\n{\n    use functora_egui::files::pick_files_sync_web_with_cancel;\n\n    let slot = pick_files_sync_web_with_cancel(true, &cancel);\n    // poll slot each frame for the PickResult\n}",
        );
    }
}
