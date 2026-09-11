use crate::app::CryptonoteApp;
use crate::error::AppError;
use crate::hooks::{handle_open_url, remove_attachment, share_error};
use crate::messages::Msg;
use crate::progress::{Stage, claim_job};
use crate::route::Screen;
use crate::state::{ActionMode, AttachmentIdx};
use functora_egui::files::format_size;
use functora_egui::i18n::I18N;
use functora_egui::messages::Msg as BaseMsg;
use functora_egui::{Alert, Button, ButtonVariant, ComponentSize, Flex, Input, Label, Progress, Separator, Textarea};

impl CryptonoteApp {
    pub(crate) fn screen_home(&mut self, ui: &mut egui::Ui) {
        let lang = self.lang();
        let message = self.temporary.message.clone();
        let has_message = message.is_some();
        if let Some(msg) = message.clone() {
            let variant = match &msg {
                Msg::Error(_) => functora_egui::AlertVariant::Destructive,
                _ => functora_egui::AlertVariant::Default,
            };
            _ = Alert::new().title(msg.render(lang)).variant(variant).show(ui, |inner| {
                _ = inner.label(msg.render(lang));
            });
            let () = ui.add_space(8.0);
            if ui
                .add(
                    Button::new(BaseMsg::Dismiss.render(lang))
                        .variant(ButtonVariant::Outline)
                        .size(ComponentSize::Sm),
                )
                .clicked()
            {
                self.temporary.message = None;
            }
            let () = ui.add_space(12.0);
        }
        // Action selector
        let () = ui.add_space(4.0);
        _ = Label::new(Msg::ActionLabel.render(lang)).show(ui);
        let () = ui.add_space(4.0);
        _ = Flex::row().gap(8.0).wrap().show(ui, |f| {
            for (mode, label, icon) in [
                (
                    ActionMode::Create,
                    Msg::ActionCreate.render(lang),
                    functora_egui::LucideIcon::SquarePlus,
                ),
                (
                    ActionMode::Open,
                    Msg::ActionOpen.render(lang),
                    functora_egui::LucideIcon::FolderOpen,
                ),
                (
                    ActionMode::Scan,
                    Msg::ActionScan.render(lang),
                    functora_egui::LucideIcon::QrCode,
                ),
            ] {
                let selected = self.temporary.action == mode;
                let variant = if selected {
                    ButtonVariant::Default
                } else {
                    ButtonVariant::Outline
                };
                if f.add(Button::new(label).icon(icon).variant(variant).selected(selected))
                    .inner
                    .clicked()
                {
                    self.temporary.action = mode;
                    self.temporary.message = None;
                }
            }
        });
        let () = ui.add_space(12.0);
        _ = Separator::horizontal().show(ui);
        let () = ui.add_space(12.0);
        match self.temporary.action {
            ActionMode::Create => self.home_create(ui),
            ActionMode::Open => self.home_open(ui),
            ActionMode::Scan => self.home_scan(ui),
        }
        let () = ui.add_space(12.0);
        if let Some(job) = self.temporary.progress.clone() {
            _ = ui.add(Progress::new(f32::from(job.percent()) / 100.0));
            let () = ui.add_space(4.0);
            _ = Label::new(format!("{:?} {} / {}", job.stage, job.done, job.total)).show(ui);
        }
        let _ = has_message;
    }

    pub(crate) fn home_create(&mut self, ui: &mut egui::Ui) {
        let lang = self.lang();
        _ = Label::new(Msg::Mode.render(lang)).show(ui);
        let () = ui.add_space(4.0);
        _ = Flex::row().gap(8.0).wrap().show(ui, |f| {
            for (cipher_opt, label) in [
                (None, Msg::NoEncryption.render(lang)),
                (
                    Some(crate::crypto::CipherType::Aes256Gcm),
                    BaseMsg::CipherAesLabel.render(lang),
                ),
                (
                    Some(crate::crypto::CipherType::ChaCha20Poly1305),
                    BaseMsg::CipherChaChaLabel.render(lang),
                ),
            ] {
                let selected = self.temporary.cipher == cipher_opt;
                let variant = if selected {
                    ButtonVariant::Default
                } else {
                    ButtonVariant::Outline
                };
                if f.add(Button::new(label).variant(variant).selected(selected))
                    .inner
                    .clicked()
                {
                    self.temporary.cipher = cipher_opt;
                }
            }
        });
        let () = ui.add_space(8.0);
        if self.temporary.cipher.is_some() {
            _ = Label::new(BaseMsg::Password.render(lang)).show(ui);
            let () = ui.add_space(4.0);
            _ = ui.add(
                Input::new(&mut self.temporary.password)
                    .placeholder(BaseMsg::PasswordPlaceholder.render(lang))
                    .password(),
            );
            let () = ui.add_space(8.0);
        }
        _ = Label::new(Msg::Note.render(lang)).show(ui);
        let () = ui.add_space(4.0);
        _ = ui.add(
            Textarea::new(&mut self.temporary.note)
                .placeholder(Msg::NotePlaceholder.render(lang))
                .desired_width(ui.available_width())
                .min_height(120.0),
        );
        let () = ui.add_space(8.0);
        // attachments
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
                    self.temporary.message = Some(err);
                } else {
                    let note = self.temporary.note.clone();
                    let password = self.temporary.password.clone();
                    let cipher = self.temporary.cipher;
                    let attachments = self.temporary.attachments.clone();
                    if claim_job(&mut self.temporary.progress, Stage::Encrypt).is_some() {
                        let rx = functora_egui::spawn_async(async move {
                            let res =
                                crate::hooks::build_external(&note, &password, cipher, &attachments, |_| {}).await;
                            match res {
                                Ok(ext) => Ok(ext),
                                Err(e) => Err(e),
                            }
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
                let rx = functora_egui::spawn_async(async move {
                    functora_egui::files::pick_files_with_cancel(true, None, Some(&cancel))
                        .await
                        .map_err(AppError::from)
                });
                self.pick_rx = Some(rx);
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
                    .icon(functora_egui::LucideIcon::Trash2)
                    .variant(ButtonVariant::Ghost),
            )
            .inner
            .clicked()
            {
                self.reset();
            }
        });
    }

    pub(crate) fn home_open(&mut self, ui: &mut egui::Ui) {
        let lang = self.lang();
        _ = Label::new(Msg::OpenUrlLabel.render(lang)).show(ui);
        let () = ui.add_space(4.0);
        _ = ui.add(
            Textarea::new(&mut self.temporary.url_input)
                .placeholder(Msg::OpenUrlPlaceholder.render(lang))
                .desired_width(ui.available_width())
                .min_height(90.0),
        );
        let () = ui.add_space(8.0);
        _ = Flex::row().gap(8.0).wrap().show(ui, |f| {
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
                    self.temporary.message = Some(Msg::Error(crate::error::MsgError::from(AppError::NoNoteInUrl)));
                } else {
                    match handle_open_url(&url, &mut self.temporary) {
                        Ok(screen) => self.navigate(screen),
                        Err(e) => self.temporary.message = Some(Msg::Error(crate::error::MsgError::from(e))),
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
                && claim_job(&mut self.temporary.progress, Stage::Attach).is_some()
            {
                let cancel = functora_egui::new_cancel_token();
                self.pick_cancel = Some(cancel.clone());
                let rx = functora_egui::spawn_async(async move {
                    let files = functora_egui::files::pick_files_with_cancel(false, None, Some(&cancel)).await?;
                    let bytes = files
                        .into_iter()
                        .next()
                        .ok_or(AppError::NoFileSelected)
                        .map(|(_, data)| data)?;
                    let source = crate::archive::ArchiveSource::Bytes(bytes);
                    crate::hooks::load_archive_async(source).await
                });
                self.archive_rx = Some(rx);
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
                self.temporary.url_input.clear();
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
    }

    pub(crate) fn home_scan(&mut self, ui: &mut egui::Ui) {
        let lang = self.lang();
        let () = ui.add_space(8.0);
        _ = functora_egui::QrScanner::new()
            .continuous(true)
            .show(ui, &mut self.qr_state);
        if let Some(text) = self.qr_state.decoded() {
            self.qr_state.clear_decoded();
            match handle_open_url(&text, &mut self.temporary) {
                Ok(screen) => self.navigate(screen),
                Err(e) => self.temporary.message = Some(Msg::Error(crate::error::MsgError::from(e))),
            }
        }
        if let Some(err) = self.qr_state.error() {
            self.qr_state.clear_error();
            self.temporary.message = Some(Msg::Error(crate::error::MsgError::from(AppError::InvalidFormat(
                err.to_string(),
            ))));
        }
        let () = ui.add_space(8.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
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
    }

    pub(crate) fn show_attachments(&mut self, ui: &mut egui::Ui) {
        if self.temporary.attachments.is_empty() {
            return;
        }
        let () = ui.add_space(8.0);
        let mut to_remove: Option<AttachmentIdx> = None;
        let mut to_open: Option<AttachmentIdx> = None;
        for (idx, att) in self.temporary.attachments.iter().enumerate() {
            let size = format_size(att.data.len() as u64);
            _ = Flex::row().gap(8.0).show(ui, |f| {
                _ = f.ui(|inner| {
                    _ = inner.label(format!("{} ({})", att.name, size));
                });
                if f.add(
                    Button::new(Msg::ViewButton.render(self.lang()))
                        .icon(functora_egui::LucideIcon::Eye)
                        .variant(ButtonVariant::Ghost)
                        .size(ComponentSize::Sm),
                )
                .inner
                .clicked()
                {
                    to_open = Some(AttachmentIdx(idx));
                }
                if f.add(
                    Button::new(Msg::RemoveFile.render(self.lang()))
                        .icon(functora_egui::LucideIcon::Trash2)
                        .variant(ButtonVariant::Ghost)
                        .size(ComponentSize::Sm),
                )
                .inner
                .clicked()
                {
                    to_remove = Some(AttachmentIdx(idx));
                }
            });
            let () = ui.add_space(4.0);
        }
        if let Some(idx) = to_remove {
            remove_attachment(&mut self.temporary, idx);
        }
        if let Some(idx) = to_open {
            self.temporary.attachment = Some(idx);
            self.navigate(Screen::File);
        }
    }
}
