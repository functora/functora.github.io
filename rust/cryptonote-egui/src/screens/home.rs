use crate::app::CryptonoteApp;
use crate::messages::Msg;
use crate::screens::Screen;
use crate::state::{ActionMode, PasteTarget, PickKind};
use functora_core::crypto::CipherType;
use functora_core::files::format_size;
use functora_core::messages::Msg as BaseMsg;

impl CryptonoteApp {
    pub(crate) fn render_home(&mut self, ui: &mut egui::Ui) {
        _ = ui.heading(self.text(&Msg::ActionLabel));
        let _modes = ui.horizontal(|row| {
            for (mode, label) in [
                (ActionMode::Create, self.text(&Msg::ActionCreate)),
                (ActionMode::Open, self.text(&Msg::ActionOpen)),
                (ActionMode::Scan, self.text(&Msg::ActionScan)),
            ] {
                _ = row.selectable_value(&mut self.action, mode, label);
            }
        });
        match self.action {
            ActionMode::Create => self.render_home_create(ui),
            ActionMode::Open => self.render_home_open(ui),
            ActionMode::Scan => self.render_home_scan(ui),
        }
    }

    fn render_home_create(&mut self, ui: &mut egui::Ui) {
        _ = ui.label(self.text(&Msg::Mode));
        let _combo = egui::ComboBox::from_id_salt("mode")
            .selected_text(self.cipher_label())
            .show_ui(ui, |combo| {
                if combo
                    .selectable_label(self.cipher.is_none(), self.text(&Msg::NoEncryption))
                    .clicked()
                {
                    self.cipher = None;
                }
                for cipher in [CipherType::Aes256Gcm, CipherType::ChaCha20Poly1305] {
                    if combo
                        .selectable_label(
                            self.cipher == Some(cipher),
                            self.cipher_label_of(Some(cipher)),
                        )
                        .clicked()
                    {
                        self.cipher = Some(cipher);
                    }
                }
            });
        if self.cipher.is_some() {
            let hint = self.text(&Msg::Base(BaseMsg::PasswordPlaceholder));
            let _password_edit = ui.add(
                egui::TextEdit::singleline(&mut self.password)
                    .password(true)
                    .hint_text(hint),
            );
        }
        _ = ui.label(self.text(&Msg::Note));
        let hint = self.text(&Msg::NotePlaceholder);
        let _note_edit = ui.add(
            egui::TextEdit::multiline(&mut self.note)
                .hint_text(hint)
                .desired_width(f32::INFINITY)
                .desired_rows(12),
        );
        if !self.attachments.is_empty() {
            _ = ui.separator();
            let mut remove: Option<usize> = None;
            for (i, att) in self.attachments.iter().enumerate() {
                let _row = ui.horizontal(|row| {
                    _ = row.label(&att.name);
                    _ = row.label(format_size(att.data.len() as u64));
                    if row.button(self.text(&Msg::RemoveFile)).clicked() {
                        remove = Some(i);
                    }
                });
            }
            if let Some(i) = remove {
                _ = self.attachments.remove(i);
            }
        }
        _ = ui.separator();
        let _buttons = ui.horizontal(|buttons| {
            if buttons.button(self.text(&Msg::Share)).clicked() {
                self.generate_share();
            }
            if buttons.button(self.text(&Msg::AttachFiles)).clicked() {
                self.pick_files(PickKind::Attach);
            }
            if buttons
                .button(self.text(&Msg::Base(BaseMsg::Paste)))
                .clicked()
            {
                self.paste(PasteTarget::Note);
            }
            if buttons.button(self.text(&Msg::ViewButton)).clicked() {
                self.navigate(Screen::View);
            }
            if buttons.button(self.text(&Msg::CreateNewNote)).clicked() {
                self.reset();
            }
        });
    }

    fn render_home_open(&mut self, ui: &mut egui::Ui) {
        _ = ui.label(self.text(&Msg::OpenUrlLabel));
        let hint = self.text(&Msg::OpenUrlPlaceholder);
        let _url_edit = ui.add(
            egui::TextEdit::multiline(&mut self.url_input)
                .hint_text(hint)
                .desired_width(f32::INFINITY)
                .desired_rows(6),
        );
        let _buttons = ui.horizontal(|buttons| {
            if buttons.button(self.text(&Msg::OpenButton)).clicked() {
                self.open_url();
            }
            if buttons.button(self.text(&Msg::OpenArchive)).clicked() {
                self.pick_files(PickKind::OpenArchive);
            }
            if buttons
                .button(self.text(&Msg::Base(BaseMsg::Paste)))
                .clicked()
            {
                self.paste(PasteTarget::Url);
            }
            if buttons.button(self.text(&Msg::Clear)).clicked() {
                self.url_input.clear();
            }
            if buttons.button(self.text(&Msg::CreateNewNote)).clicked() {
                self.reset();
            }
        });
    }

    fn render_home_scan(&mut self, ui: &mut egui::Ui) {
        _ = ui.label(self.text(&Msg::ActionScan));
        let _buttons = ui.horizontal(|buttons| {
            if buttons.button(self.text(&Msg::ActionScan)).clicked() {
                self.scan_image();
            }
            if buttons.button(self.text(&Msg::CreateNewNote)).clicked() {
                self.reset();
            }
        });
    }

    fn cipher_label(&self) -> String {
        self.cipher_label_of(self.cipher)
    }

    fn cipher_label_of(&self, cipher: Option<CipherType>) -> String {
        match cipher {
            None => self.text(&Msg::NoEncryption),
            Some(CipherType::Aes256Gcm) => {
                format!("🔐 AES-256-GCM {}", self.text(&Msg::EncryptionSuffix))
            }
            Some(CipherType::ChaCha20Poly1305) => {
                format!("🔐 ChaCha20-Poly1305 {}", self.text(&Msg::EncryptionSuffix))
            }
        }
    }
}
