use crate::app::CryptonoteApp;
use crate::messages::Msg;
use crate::screens::Screen;
use crate::state::{ActionMode, PasteTarget, PickKind};
use egui_shadcn::{
    Button, ButtonVariant, Card, Input, Label, LucideIcon, SelectValue, Textarea, ToggleGroup,
};
use functora_core::crypto::CipherType;
use functora_core::files::format_size;
use functora_core::messages::Msg as BaseMsg;
use std::fmt;

#[derive(Clone, PartialEq)]
struct CipherChoice {
    cipher: Option<CipherType>,
    label: String,
}

impl fmt::Display for CipherChoice {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.label)
    }
}

impl CryptonoteApp {
    pub(crate) fn render_home(&mut self, ui: &mut egui::Ui) {
        let heading = self.text(&Msg::ActionLabel);
        let _ = Card::new().heading(heading).show(ui, |card| {
            if self.is_mobile() {
                for mode in ActionMode::ALL {
                    let button = if self.action == mode {
                        Button::new(self.action_label(mode)).full_width()
                    } else {
                        Button::new(self.action_label(mode))
                            .variant(ButtonVariant::Outline)
                            .full_width()
                    };
                    if card.add(button).clicked() {
                        self.action = mode;
                    }
                }
            } else {
                let items: Vec<String> = ActionMode::ALL
                    .iter()
                    .map(|mode| self.action_label(*mode))
                    .collect();
                let icons: Vec<LucideIcon> = ActionMode::ALL
                    .iter()
                    .map(|mode| Self::action_icon(*mode))
                    .collect();
                let mut index = self.action.index();
                _ = ToggleGroup::new(items).icons(icons).show(card, &mut index);
                if index != self.action.index() {
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

    fn action_icon(mode: ActionMode) -> LucideIcon {
        match mode {
            ActionMode::Create => LucideIcon::Plus,
            ActionMode::Open => LucideIcon::FolderOpen,
            ActionMode::Scan => LucideIcon::Search,
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
        _ = Label::new(mode_label).show(ui);
        let options: Vec<CipherChoice> = [
            CipherChoice {
                cipher: None,
                label: self.text(&Msg::NoEncryption),
            },
            CipherChoice {
                cipher: Some(CipherType::Aes256Gcm),
                label: self.cipher_label_of(Some(CipherType::Aes256Gcm)),
            },
            CipherChoice {
                cipher: Some(CipherType::ChaCha20Poly1305),
                label: self.cipher_label_of(Some(CipherType::ChaCha20Poly1305)),
            },
        ]
        .into_iter()
        .collect();
        let mut choice = options
            .iter()
            .find(|option| option.cipher == self.cipher)
            .cloned()
            .unwrap_or_else(|| options[0].clone());
        _ = ui.add(SelectValue::new(&mut choice, &options));
        self.cipher = choice.cipher;
        if self.cipher.is_some() {
            let password_label = self.text(&Msg::Base(BaseMsg::Password));
            let hint = self.text(&Msg::Base(BaseMsg::PasswordPlaceholder));
            _ = Label::new(password_label).show(ui);
            _ = ui.add(Input::new(&mut self.password).password().placeholder(hint));
        }
        let note_label = self.text(&Msg::Note);
        let hint = self.text(&Msg::NotePlaceholder);
        _ = Label::new(note_label).show(ui);
        _ = ui.add(
            Textarea::new(&mut self.note)
                .placeholder(hint)
                .min_height(240.0),
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
                                Button::icon_only(LucideIcon::Trash2)
                                    .variant(ButtonVariant::Outline)
                                    .size(egui_shadcn::ComponentSize::Sm),
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
        let share_label = self.text(&Msg::Share);
        let attach_label = self.text(&Msg::AttachFiles);
        let paste_label = self.text(&Msg::Base(BaseMsg::Paste));
        let view_label = self.text(&Msg::ViewButton);
        let reset_label = self.text(&Msg::CreateNewNote);
        ui.add_space(8.0);
        self.render_dock(ui, |row, app| {
            if row
                .add(Button::new(&share_label).icon(LucideIcon::Share2))
                .clicked()
            {
                app.generate_share();
            }
            if row
                .add(
                    Button::new(&attach_label)
                        .icon(LucideIcon::Upload)
                        .variant(ButtonVariant::Outline),
                )
                .clicked()
            {
                app.pick_files(PickKind::Attach);
            }
            if row
                .add(
                    Button::new(&paste_label)
                        .icon(LucideIcon::Copy)
                        .variant(ButtonVariant::Outline),
                )
                .clicked()
            {
                app.paste(PasteTarget::Note);
            }
            if row
                .add(
                    Button::new(&view_label)
                        .icon(LucideIcon::Eye)
                        .variant(ButtonVariant::Outline),
                )
                .clicked()
            {
                app.navigate(Screen::View);
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

    fn render_home_open(&mut self, ui: &mut egui::Ui) {
        let label = self.text(&Msg::OpenUrlLabel);
        let hint = self.text(&Msg::OpenUrlPlaceholder);
        _ = Label::new(label).show(ui);
        _ = ui.add(
            Textarea::new(&mut self.url_input)
                .placeholder(hint)
                .min_height(160.0),
        );
        let open_label = self.text(&Msg::OpenButton);
        let archive_label = self.text(&Msg::OpenArchive);
        let paste_label = self.text(&Msg::Base(BaseMsg::Paste));
        let clear_label = self.text(&Msg::Clear);
        let reset_label = self.text(&Msg::CreateNewNote);
        ui.add_space(8.0);
        self.render_dock(ui, |row, app| {
            if row
                .add(Button::new(&open_label).icon(LucideIcon::ExternalLink))
                .clicked()
            {
                app.open_url();
            }
            if row
                .add(
                    Button::new(&archive_label)
                        .icon(LucideIcon::FolderOpen)
                        .variant(ButtonVariant::Outline),
                )
                .clicked()
            {
                app.pick_files(PickKind::OpenArchive);
            }
            if row
                .add(
                    Button::new(&paste_label)
                        .icon(LucideIcon::Copy)
                        .variant(ButtonVariant::Outline),
                )
                .clicked()
            {
                app.paste(PasteTarget::Url);
            }
            if row
                .add(
                    Button::new(&clear_label)
                        .icon(LucideIcon::X)
                        .variant(ButtonVariant::Outline),
                )
                .clicked()
            {
                app.url_input.clear();
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

    fn render_home_scan(&mut self, ui: &mut egui::Ui) {
        let scan_label = self.text(&Msg::ActionScan);
        let reset_label = self.text(&Msg::CreateNewNote);
        ui.add_space(8.0);
        self.render_dock(ui, |row, app| {
            if row
                .add(Button::new(&scan_label).icon(LucideIcon::Search))
                .clicked()
            {
                app.scan_image();
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
