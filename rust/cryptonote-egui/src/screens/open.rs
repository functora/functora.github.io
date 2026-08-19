use crate::app::CryptonoteApp;
use crate::encoding::NoteData;
use crate::messages::Msg;
use crate::state::{External, PasteTarget};
use functora_core::messages::Msg as BaseMsg;

impl CryptonoteApp {
    pub(crate) fn render_open(&mut self, ui: &mut egui::Ui) {
        let encrypted = match &self.external {
            External::Note(n) => matches!(n.data, NoteData::CipherText(_)),
            External::Archive(_) => true,
            External::Nothing => false,
        };
        if encrypted {
            self.render_encrypted_open(ui);
        } else {
            self.render_note_display(ui);
        }
    }

    fn render_encrypted_open(&mut self, ui: &mut egui::Ui) {
        _ = ui.heading(self.text(&Msg::EncryptedNote));
        _ = ui.label(self.text(&Msg::EncryptedNoteDesc));
        let hint = self.text(&Msg::Base(BaseMsg::PasswordPlaceholder));
        let _password_edit = ui.add(
            egui::TextEdit::singleline(&mut self.password)
                .password(true)
                .hint_text(hint),
        );
        let _buttons = ui.horizontal(|buttons| {
            if buttons.button(self.text(&Msg::DecryptButton)).clicked() {
                self.decrypt_note();
            }
            if buttons
                .button(self.text(&Msg::Base(BaseMsg::Paste)))
                .clicked()
            {
                self.paste(PasteTarget::Password);
            }
            if buttons.button(self.text(&Msg::Clear)).clicked() {
                self.password.clear();
            }
            if buttons.button(self.text(&Msg::CreateNewNote)).clicked() {
                self.reset();
            }
        });
    }
}
