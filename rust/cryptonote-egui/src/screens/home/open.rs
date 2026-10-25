use crate::app::{CryptonoteApp, OpStart};
use crate::error::AppError;
use crate::hooks::handle_open_url;
use crate::messages::Msg;
use crate::progress::Stage;
use functora_egui::i18n::I18N;
use functora_egui::{Button, ButtonVariant, Flex, Label, TextareaPasteClear, ToastVariant};

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
                && let Some(start) = self.begin_op(Stage::Attach)
            {
                let OpStart {
                    cancel,
                    slot,
                    progress: load_progress,
                } = start;
                let load_cancel = cancel.clone();
                self.archive_rx = Some(functora_egui::spawn_async(async move {
                    let files =
                        functora_egui::files::pick_files_with_shared_progress(false, Some(slot), Some(&cancel)).await?;
                    let bytes = files
                        .into_iter()
                        .next()
                        .ok_or(AppError::NoFileSelected)
                        .map(|(_, data)| data)?;
                    let source = crate::archive::ArchiveSource::Bytes(bytes);
                    crate::hooks::load_archive_async(source, load_cancel, load_progress).await
                }));
            }
        });
    }
}
