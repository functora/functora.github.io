use crate::app::CryptonoteApp;
use crate::error::AppError;
use crate::hooks::handle_open_url;
use crate::messages::Msg;
use crate::progress::{Stage, claim_job};
use functora_egui::i18n::I18N;
use functora_egui::{Button, ButtonVariant, Flex, Label, Progress, TextareaPasteClear, ToastVariant};

impl CryptonoteApp {
    pub(crate) fn home_open(&mut self, ui: &mut egui::Ui) {
        let lang = self.lang();
        let toast_time = ui.ctx().input(|i| i.time);
        _ = Label::new(Msg::OpenUrlLabel.render(lang)).show(ui);
        let () = ui.add_space(4.0);
        let resp = TextareaPasteClear::new(&mut self.temporary.url_input)
            .placeholder(Msg::OpenUrlPlaceholder.render(lang))
            .min_height(120.0)
            .show(ui);
        self.paste_clear_feedback(resp, toast_time);
        let () = ui.add_space(8.0);
        _ = Flex::row().gap(8.0).wrap().show(ui, |f| {
            self.dock_back_flex(f);
            if f.add(
                Button::new(Msg::OpenButton.render(lang))
                    .icon(functora_egui::LucideIcon::FolderOpen)
                    .variant(ButtonVariant::Default),
            )
            .inner
            .clicked()
            {
                let url = self.temporary.url_input.trim().to_string();
                if url.is_empty() {
                    self.toast.add(
                        Msg::Error(crate::error::MsgError::from(AppError::NoNoteParam)).render(lang),
                        ToastVariant::Error,
                        toast_time,
                    );
                } else {
                    match handle_open_url(&url, &mut self.temporary) {
                        Ok(screen) => self.navigate(screen),
                        Err(e) => self.toast.add(
                            Msg::Error(crate::error::MsgError::from(e)).render(lang),
                            ToastVariant::Error,
                            toast_time,
                        ),
                    }
                }
            }
            if f.add(
                Button::new(Msg::OpenArchive.render(lang))
                    .icon(functora_egui::LucideIcon::Paperclip)
                    .variant(ButtonVariant::Outline),
            )
            .inner
            .clicked()
                && claim_job(&mut self.temporary.progress, Stage::Attach).is_some()
            {
                let cancel = functora_egui::new_cancel_token();
                self.pick_cancel = Some(cancel.clone());
                let slot = std::sync::Arc::new(std::sync::Mutex::new(None));
                self.progress_slot = Some(std::sync::Arc::clone(&slot));
                let load_progress = {
                    let load_slot = std::sync::Arc::clone(&slot);
                    move |job| {
                        *load_slot.lock().unwrap_or_else(std::sync::PoisonError::into_inner) = job;
                    }
                };
                let rx = functora_egui::spawn_async(async move {
                    let files =
                        functora_egui::files::pick_files_with_shared_progress(false, Some(slot), Some(&cancel)).await?;
                    let bytes = files
                        .into_iter()
                        .next()
                        .ok_or(AppError::NoFileSelected)
                        .map(|(_, data)| data)?;
                    let source = crate::archive::ArchiveSource::Bytes(bytes);
                    crate::hooks::load_archive_async(source, load_progress).await
                });
                self.archive_rx = Some(rx);
            }
        });
        if let Some(job) = self.temporary.progress.clone() {
            let () = ui.add_space(8.0);
            _ = ui.add(Progress::new(f32::from(job.percent()) / 100.0));
        }
    }
}
