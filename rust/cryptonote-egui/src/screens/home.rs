use crate::app::CryptonoteApp;
use crate::messages::Msg;
use crate::screens::Screen;
use crate::state::{ActionMode, PasteTarget, PickKind};
use elegance::glyphs;
use elegance::{
    Accent, Button, ButtonSize, Card, Segment, SegmentedControl, Select, TextArea, TextInput,
};
use functora_core::crypto::CipherType;
use functora_core::files::format_size;
use functora_core::messages::Msg as BaseMsg;

impl CryptonoteApp {
    pub(crate) fn render_home(&mut self, ui: &mut egui::Ui) {
        let heading = self.text(&Msg::ActionLabel);
        let _ = Card::new().heading(heading).show(ui, |card| {
            if self.is_mobile() {
                for mode in ActionMode::ALL {
                    let label = format!("{} {}", Self::action_glyph(mode), self.action_label(mode));
                    let button = if self.action == mode {
                        Button::new(label).accent(Accent::Blue).full_width()
                    } else {
                        Button::new(label).outline().full_width()
                    };
                    if card.add(button).clicked() {
                        self.action = mode;
                    }
                }
            } else {
                let mut index = self.action.index();
                let segments: Vec<Segment> = ActionMode::ALL
                    .iter()
                    .map(|mode| {
                        Segment::icon_text(
                            egui::RichText::new(Self::action_glyph(*mode)),
                            self.action_label(*mode),
                        )
                    })
                    .collect();
                if card
                    .add(SegmentedControl::from_segments(&mut index, segments))
                    .changed()
                {
                    self.action = ActionMode::from_index(index);
                }
            }
        });
        match self.action {
            ActionMode::Create => self.render_home_create(ui),
            ActionMode::Open => self.render_home_open(ui),
            ActionMode::Scan => self.render_home_scan(ui),
        }
    }

    fn action_glyph(mode: ActionMode) -> char {
        match mode {
            ActionMode::Create => glyphs::PLUS,
            ActionMode::Open => glyphs::FOLDER_OPEN,
            ActionMode::Scan => glyphs::SEARCH,
        }
    }

    fn action_label(&self, mode: ActionMode) -> String {
        let msg = match mode {
            ActionMode::Create => Msg::ActionCreate,
            ActionMode::Open => Msg::ActionOpen,
            ActionMode::Scan => Msg::ActionScan,
        };
        self.text(&msg)
    }

    fn render_home_create(&mut self, ui: &mut egui::Ui) {
        let mode_label = self.text(&Msg::Mode);
        let options: Vec<(Option<CipherType>, String)> = [
            (None, self.text(&Msg::NoEncryption)),
            (
                Some(CipherType::Aes256Gcm),
                self.cipher_label_of(Some(CipherType::Aes256Gcm)),
            ),
            (
                Some(CipherType::ChaCha20Poly1305),
                self.cipher_label_of(Some(CipherType::ChaCha20Poly1305)),
            ),
        ]
        .into_iter()
        .collect();
        let cipher = &mut self.cipher;
        _ = ui.add(
            Select::new("mode", cipher)
                .label(mode_label)
                .options(options),
        );
        if self.cipher.is_some() {
            let password_label = self.text(&Msg::Base(BaseMsg::Password));
            let hint = self.text(&Msg::Base(BaseMsg::PasswordPlaceholder));
            _ = ui.add(
                TextInput::new(&mut self.password)
                    .label(password_label)
                    .hint(&hint)
                    .revealable(true)
                    .id_salt("password"),
            );
        }
        let note_label = self.text(&Msg::Note);
        let hint = self.text(&Msg::NotePlaceholder);
        _ = ui.add(
            TextArea::new(&mut self.note)
                .label(note_label)
                .hint(&hint)
                .rows(12)
                .id_salt("note"),
        );
        if !self.attachments.is_empty() {
            let heading = self.text(&Msg::File);
            let mut remove: Option<usize> = None;
            let _ = Card::new().heading(heading).show(ui, |card| {
                for (i, att) in self.attachments.iter().enumerate() {
                    let _row = card.horizontal(|row| {
                        _ = row.label(egui::RichText::new(&att.name).strong());
                        _ = row.label(format_size(att.data.len() as u64));
                        if row
                            .add(
                                Button::new(egui::RichText::new(glyphs::TRASH))
                                    .outline()
                                    .size(ButtonSize::Small),
                            )
                            .on_hover_text(self.text(&Msg::RemoveFile))
                            .clicked()
                        {
                            remove = Some(i);
                        }
                    });
                }
            });
            if let Some(i) = remove {
                _ = self.attachments.remove(i);
            }
        }
        let share_label = format!("{} {}", glyphs::NETWORK, self.text(&Msg::Share));
        let attach_label = format!("{} {}", glyphs::UPLOAD, self.text(&Msg::AttachFiles));
        let paste_label = format!("{} {}", glyphs::COPY, self.text(&Msg::Base(BaseMsg::Paste)));
        let view_label = format!("{} {}", glyphs::EYE, self.text(&Msg::ViewButton));
        let reset_label = format!("{} {}", glyphs::TRASH, self.text(&Msg::CreateNewNote));
        ui.add_space(8.0);
        self.render_dock(ui, |row, app| {
            if row
                .add(Button::new(&share_label).accent(Accent::Blue))
                .clicked()
            {
                app.generate_share();
            }
            if row.add(Button::new(&attach_label).outline()).clicked() {
                app.pick_files(PickKind::Attach);
            }
            if row.add(Button::new(&paste_label).outline()).clicked() {
                app.paste(PasteTarget::Note);
            }
            if row.add(Button::new(&view_label).outline()).clicked() {
                app.navigate(Screen::View);
            }
            if row.add(Button::new(&reset_label).outline()).clicked() {
                app.reset();
            }
        });
    }

    fn render_home_open(&mut self, ui: &mut egui::Ui) {
        let label = self.text(&Msg::OpenUrlLabel);
        let hint = self.text(&Msg::OpenUrlPlaceholder);
        _ = ui.add(
            TextArea::new(&mut self.url_input)
                .label(label)
                .hint(&hint)
                .rows(6)
                .id_salt("url"),
        );
        let open_label = format!("{} {}", glyphs::EXTERNAL_LINK, self.text(&Msg::OpenButton));
        let archive_label = format!("{} {}", glyphs::FOLDER_OPEN, self.text(&Msg::OpenArchive));
        let paste_label = format!("{} {}", glyphs::COPY, self.text(&Msg::Base(BaseMsg::Paste)));
        let clear_label = format!("{} {}", glyphs::X, self.text(&Msg::Clear));
        let reset_label = format!("{} {}", glyphs::TRASH, self.text(&Msg::CreateNewNote));
        ui.add_space(8.0);
        self.render_dock(ui, |row, app| {
            if row
                .add(Button::new(&open_label).accent(Accent::Blue))
                .clicked()
            {
                app.open_url();
            }
            if row.add(Button::new(&archive_label).outline()).clicked() {
                app.pick_files(PickKind::OpenArchive);
            }
            if row.add(Button::new(&paste_label).outline()).clicked() {
                app.paste(PasteTarget::Url);
            }
            if row.add(Button::new(&clear_label).outline()).clicked() {
                app.url_input.clear();
            }
            if row.add(Button::new(&reset_label).outline()).clicked() {
                app.reset();
            }
        });
    }

    fn render_home_scan(&mut self, ui: &mut egui::Ui) {
        let scan_label = format!("{} {}", glyphs::SEARCH, self.text(&Msg::ActionScan));
        let reset_label = format!("{} {}", glyphs::TRASH, self.text(&Msg::CreateNewNote));
        ui.add_space(8.0);
        self.render_dock(ui, |row, app| {
            if row
                .add(Button::new(&scan_label).accent(Accent::Blue))
                .clicked()
            {
                app.scan_image();
            }
            if row.add(Button::new(&reset_label).outline()).clicked() {
                app.reset();
            }
        });
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
