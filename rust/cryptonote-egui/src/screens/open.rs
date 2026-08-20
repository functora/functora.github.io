use crate::app::CryptonoteApp;
use crate::encoding::NoteData;
use crate::messages::Msg;
use crate::state::{External, PasteTarget};
use egui_shadcn::{Button, ButtonVariant, Card, Input, Label, LucideIcon};
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
            _ = Label::new(password_label).show(card);
            _ = card.add(Input::new(&mut self.password).password().placeholder(hint));
        });
        let decrypt_label = self.text(&Msg::DecryptButton);
        let paste_label = self.text(&Msg::Base(BaseMsg::Paste));
        let clear_label = self.text(&Msg::Clear);
        let reset_label = self.text(&Msg::CreateNewNote);
        ui.add_space(8.0);
        self.render_dock(ui, |row, app| {
            if row
                .add(Button::new(&decrypt_label).icon(LucideIcon::KeyRound))
                .clicked()
            {
                app.decrypt_note();
            }
            if row
                .add(
                    Button::new(&paste_label)
                        .icon(LucideIcon::Copy)
                        .variant(ButtonVariant::Outline),
                )
                .clicked()
            {
                app.paste(PasteTarget::Password);
            }
            if row
                .add(
                    Button::new(&clear_label)
                        .icon(LucideIcon::X)
                        .variant(ButtonVariant::Outline),
                )
                .clicked()
            {
                app.password.clear();
            }
            if row
                .add(
                    Button::new(&reset_label)
                        .icon(LucideIcon::Trash2)
                        .variant(ButtonVariant::Outline),
                )
                .clicked()
            {
                app.reset();
            }
        });
    }
}
