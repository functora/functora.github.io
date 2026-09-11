use crate::app::CryptonoteApp;
use crate::encoding::NoteData;
use crate::error::AppError;
use crate::messages::Msg;
use crate::progress::{Stage, claim_job, clear_progress};
use crate::state::External;
use functora_egui::i18n::I18N;
use functora_egui::messages::Msg as BaseMsg;
use functora_egui::{Button, ButtonVariant, Flex, Input, Label, Progress, ToastVariant};

impl CryptonoteApp {
    pub(crate) fn screen_open(&mut self, ui: &mut egui::Ui) {
        let lang = self.lang();
        let toast_time = ui.ctx().input(|i| i.time);
        let is_encrypted = match &self.temporary.external {
            External::Note(n) => matches!(n.data, NoteData::CipherText(_)),
            External::Archive(_) => true,
            External::Nothing => false,
        };
        if is_encrypted {
            _ = Label::new(Msg::EncryptedNote.render(lang)).show(ui);
            let () = ui.add_space(4.0);
            _ = Label::new(Msg::EncryptedNoteDesc.render(lang)).show(ui);
            let () = ui.add_space(8.0);
            _ = Label::new(BaseMsg::Password.render(lang)).show(ui);
            let () = ui.add_space(4.0);
            let resp = ui.add(
                Input::new(&mut self.temporary.password)
                    .placeholder(BaseMsg::PasswordPlaceholder.render(lang))
                    .password(),
            );
            if resp.lost_focus() && ui.input(|i| i.key_pressed(egui::Key::Enter)) {
                self.decrypt_current(toast_time);
            }
            let () = ui.add_space(12.0);
            _ = Flex::row().gap(8.0).wrap().show(ui, |f| {
                if f.add(
                    Button::new(Msg::DecryptButton.render(lang))
                        .icon(functora_egui::LucideIcon::LockOpen)
                        .variant(ButtonVariant::Default),
                )
                .inner
                .clicked()
                {
                    self.decrypt_current(toast_time);
                }
                if f.add(
                    Button::new(BaseMsg::Paste.render(lang))
                        .icon(functora_egui::LucideIcon::ClipboardPaste)
                        .variant(ButtonVariant::Outline),
                )
                .inner
                .clicked()
                {
                    let rx = functora_egui::spawn_async(async move {
                        functora_egui::clipboard::read().await.map_err(AppError::from)
                    });
                    self.clipboard_rx = Some(rx);
                }
                if f.add(
                    Button::new(Msg::Clear.render(lang))
                        .icon(functora_egui::LucideIcon::X)
                        .variant(ButtonVariant::Ghost),
                )
                .inner
                .clicked()
                {
                    self.temporary.password.clear();
                }
                if f.add(
                    Button::new(Msg::CreateNewNote.render(lang))
                        .icon(functora_egui::LucideIcon::Trash2)
                        .variant(ButtonVariant::Ghost),
                )
                .inner
                .clicked()
                {
                    self.reset();
                }
            });
            if let Some(job) = self.temporary.progress.clone() {
                let () = ui.add_space(8.0);
                _ = ui.add(Progress::new(f32::from(job.percent()) / 100.0));
            }
        } else {
            self.screen_view(ui);
        }
    }

    pub(crate) fn decrypt_current(&mut self, now: f64) {
        if self.temporary.password.is_empty() {
            let lang = self.lang();
            self.toast.add(
                Msg::Base(functora_egui::messages::Msg::PasswordRequired).render(lang),
                ToastVariant::Error,
                now,
            );
            return;
        }
        if claim_job(&mut self.temporary.progress, Stage::Decrypt).is_none() {
            return;
        }
        let external = self.temporary.external.clone();
        let password = self.temporary.password.clone();
        match external {
            External::Note(note) => {
                if let NoteData::CipherText(enc) = note.data {
                    let rx = functora_egui::spawn_async(async move {
                        let password_clone = password.clone();
                        let enc_clone = enc.clone();
                        let res: Result<String, AppError> = (|| {
                            let decrypted = crate::crypto::decrypt_symmetric(&enc_clone, &password_clone)?;
                            Ok(String::from_utf8(decrypted)?)
                        })();
                        res
                    });
                    self.decrypt_rx = Some(rx);
                }
            }
            External::Archive(archive) => {
                let rx = functora_egui::spawn_async(async move {
                    let bytes = archive.untag();
                    let source = crate::archive::ArchiveSource::Bytes(bytes);

                    functora_egui::spawn_async(async move {
                        crate::archive::extract_archive_package_async_with_progress(source, &password, |_| {})
                            .await
                            .map(|(text, _)| text)
                    })
                    .recv()
                    .unwrap_or(Err(AppError::Cancelled))
                });
                self.decrypt_rx = Some(rx);
            }
            External::Nothing => {
                clear_progress(&mut self.temporary.progress);
            }
        }
    }
}
