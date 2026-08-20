use crate::app::CryptonoteApp;
use crate::encoding::NoteData;
use crate::messages::Msg;
use crate::state::{External, PasteTarget};
use elegance::glyphs;
use elegance::{Accent, Button, Card, TextInput};
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
        let heading = self.text(&Msg::EncryptedNote);
        let desc = self.text(&Msg::EncryptedNoteDesc);
        let hint = self.text(&Msg::Base(BaseMsg::PasswordPlaceholder));
        let _ = Card::new().heading(heading).show(ui, |card| {
            _ = card.label(desc);
            let password_label = self.text(&Msg::Base(BaseMsg::Password));
            _ = card.add(
                TextInput::new(&mut self.password)
                    .label(password_label)
                    .hint(&hint)
                    .revealable(true)
                    .id_salt("open-password"),
            );
        });
        let decrypt_label = format!("{} {}", glyphs::KEY, self.text(&Msg::DecryptButton));
        let paste_label = format!("{} {}", glyphs::COPY, self.text(&Msg::Base(BaseMsg::Paste)));
        let clear_label = format!("{} {}", glyphs::X, self.text(&Msg::Clear));
        let reset_label = format!("{} {}", glyphs::TRASH, self.text(&Msg::CreateNewNote));
        ui.add_space(8.0);
        self.render_dock(ui, |row, app| {
            if row
                .add(Button::new(&decrypt_label).accent(Accent::Blue))
                .clicked()
            {
                app.decrypt_note();
            }
            if row.add(Button::new(&paste_label).outline()).clicked() {
                app.paste(PasteTarget::Password);
            }
            if row.add(Button::new(&clear_label).outline()).clicked() {
                app.password.clear();
            }
            if row.add(Button::new(&reset_label).outline()).clicked() {
                app.reset();
            }
        });
    }
}
