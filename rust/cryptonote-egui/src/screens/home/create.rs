use crate::app::CryptonoteApp;
use crate::error::AppError;
use crate::hooks::share_error;
use crate::messages::Msg;
use crate::progress::{Stage, claim_job};
use crate::route::Screen;
use crate::state::CipherChoice;
use functora_egui::i18n::I18N;
use functora_egui::messages::Msg as BaseMsg;
use functora_egui::{
    Button, ButtonVariant, Flex, InputPasteClear, Label, SelectValueLabeled, TextareaPasteClear, ToastVariant,
};

impl CryptonoteApp {
    pub(crate) fn home_create(&mut self, ui: &mut egui::Ui) {
        let lang = self.lang();
        let toast_time = ui.ctx().input(|i| i.time);
        self.show_pick_overlay(ui.ctx(), lang);
        _ = Label::new(Msg::Mode.render(lang)).show(ui);
        let () = ui.add_space(4.0);
        let options = [
            (CipherChoice::Plain, Msg::NoEncryption.render(lang)),
            (CipherChoice::Aes256Gcm, BaseMsg::CipherAesLabel.render(lang)),
            (CipherChoice::ChaCha20Poly1305, BaseMsg::CipherChaChaLabel.render(lang)),
        ];
        let mut choice = CipherChoice::from(self.temporary.cipher);
        _ = SelectValueLabeled::new(&mut choice, &options).show(ui);
        self.temporary.cipher = choice.into();
        let () = ui.add_space(8.0);
        if self.temporary.cipher.is_some() {
            _ = Label::new(BaseMsg::Password.render(lang)).show(ui);
            let () = ui.add_space(4.0);
            let resp = InputPasteClear::new(&mut self.temporary.password)
                .placeholder(BaseMsg::PasswordPlaceholder.render(lang))
                .password()
                .show(ui);
            self.paste_clear_feedback(resp, toast_time);
            let () = ui.add_space(8.0);
        }
        _ = Label::new(Msg::Note.render(lang)).show(ui);
        let () = ui.add_space(4.0);
        let resp = TextareaPasteClear::new(&mut self.temporary.note)
            .placeholder(Msg::NotePlaceholder.render(lang))
            .show(ui);
        self.paste_clear_feedback(resp, toast_time);
        let () = ui.add_space(8.0);
        self.show_attachments(ui);
        let () = ui.add_space(8.0);
        _ = Flex::row().gap(8.0).wrap().show(ui, |f| {
            let can_share = self.temporary.progress.is_none();
            if f.add(
                Button::new(Msg::Share.render(lang))
                    .icon(functora_egui::LucideIcon::Share2)
                    .variant(ButtonVariant::Default)
                    .enabled(can_share),
            )
            .inner
            .clicked()
            {
                if let Some(err) = share_error(self.temporary.cipher, &self.temporary.password) {
                    self.toast.add(err.render(lang), ToastVariant::Error, toast_time);
                } else {
                    let note = self.temporary.note.clone();
                    let password = self.temporary.password.clone();
                    let cipher = self.temporary.cipher;
                    let attachments = self.temporary.attachments.clone();
                    if claim_job(&mut self.temporary.progress, Stage::Encrypt).is_some() {
                        let progress = self.track_progress();
                        let rx = functora_egui::spawn_async(async move {
                            crate::hooks::build_external(&note, &password, cipher, &attachments, progress).await
                        });
                        self.generate_rx = Some(rx);
                    }
                }
            }
            if f.add(
                Button::new(Msg::AttachFiles.render(lang))
                    .icon(functora_egui::LucideIcon::Paperclip)
                    .variant(ButtonVariant::Outline),
            )
            .inner
            .clicked()
                && claim_job(&mut self.temporary.progress, Stage::Attach).is_some()
            {
                let cancel = functora_egui::new_cancel_token();
                self.pick_cancel = Some(cancel.clone());
                self.pick_overlay_open = true;
                let rx = functora_egui::spawn_async(async move {
                    functora_egui::files::pick_files_with_cancel(true, None, Some(&cancel))
                        .await
                        .map_err(AppError::from)
                });
                self.pick_rx = Some(rx);
            }
            if f.add(
                Button::new(Msg::ViewButton.render(lang))
                    .icon(functora_egui::LucideIcon::Eye)
                    .variant(ButtonVariant::Outline),
            )
            .inner
            .clicked()
            {
                self.navigate(Screen::View);
            }
            if f.add(
                Button::new(Msg::CreateNewNote.render(lang))
                    .icon(functora_egui::LucideIcon::RotateCcw)
                    .variant(ButtonVariant::Ghost),
            )
            .inner
            .clicked()
            {
                self.reset();
            }
        });
    }
}
