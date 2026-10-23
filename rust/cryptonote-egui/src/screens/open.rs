use crate::app::{CryptonoteApp, OpStart};
use crate::encoding::NoteData;
use crate::error::AppError;
use crate::messages::Msg;
use crate::progress::Stage;
use crate::state::External;
use functora_egui::i18n::I18N;
use functora_egui::messages::Msg as BaseMsg;
use functora_egui::{Button, ButtonVariant, Flex, InputPasteClear, Label, ToastVariant};

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
            let resp = InputPasteClear::new(&mut self.temporary.password)
                .placeholder(BaseMsg::PasswordPlaceholder.render(lang))
                .password()
                .show(ui);
            if resp.response.lost_focus() && ui.input(|i| i.key_pressed(egui::Key::Enter)) {
                self.decrypt_current(toast_time);
            }
            self.paste_clear_feedback(resp, toast_time);
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
            });
        } else {
            self.screen_view(ui);
        }
    }

    pub fn decrypt_current(&mut self, now: f64) {
        if self.temporary.password.is_empty() {
            let lang = self.lang();
            self.toast.add(
                Msg::Base(functora_egui::messages::Msg::PasswordRequired).render(lang),
                ToastVariant::Error,
                now,
            );
            return;
        }
        let Some(OpStart { cancel, progress, .. }) = self.begin_op(Stage::Decrypt) else {
            return;
        };
        let external = self.temporary.external.clone();
        let password = self.temporary.password.clone();
        match external {
            External::Note(note) => {
                if let NoteData::CipherText(enc) = note.data {
                    let rx = functora_egui::spawn_async(async move {
                        let password_clone = password.clone();
                        let enc_clone = enc.clone();
                        let res: Result<(String, Vec<crate::archive::Attachment>), AppError> = (|| {
                            let decrypted = crate::crypto::decrypt_symmetric(&enc_clone, &password_clone)?;
                            Ok((String::from_utf8(decrypted)?, Vec::new()))
                        })(
                        );
                        res
                    });
                    self.decrypt_rx = Some(rx);
                }
            }
            External::Archive(archive) => {
                let rx = functora_egui::spawn_async(async move {
                    let bytes = archive.untag();
                    let source = crate::archive::ArchiveSource::Bytes(bytes);
                    crate::archive::extract_archive_package_async_with_progress(source, &password, cancel, progress)
                        .await
                });
                self.decrypt_rx = Some(rx);
            }
            External::Nothing => {
                self.end_op();
            }
        }
    }
}
