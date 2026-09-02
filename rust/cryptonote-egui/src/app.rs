#![allow(
    clippy::shadow_reuse,
    clippy::shadow_same,
    clippy::shadow_unrelated,
    clippy::type_complexity,
    clippy::too_many_lines
)]
use egui::ScrollArea;
use functora_egui::i18n::{I18N, Language};
use functora_egui::route::AppRouter;
use functora_egui::storage::persist_value;
use functora_egui::{
    Alert, Button, ButtonVariant, Card, Flex, Input, Label, Progress, ResponsiveExt, Separator, ShadcnThemeExt, Shell,
    Textarea, ToastState, ToastVariant,
};

use crate::encoding::{NoteData, decode_note, extract_note_param};
use crate::error::AppError;
use crate::hooks::handle_open_url;
use crate::hooks::{remove_attachment, share_error};
use crate::messages::Msg;
use crate::progress::{Job, Stage, claim_job, clear_progress};
use crate::route::Screen;
use crate::state::{ActionMode, AttachmentIdx, External, TemporaryState};
use crate::storage::{APP_ATTRS, PersistentState};
use functora_egui::files::format_size;

const PERSISTENT_KEY: &str = "cryptonote_persistent";
const BYTES_URI_PREFIX: &str = "bytes://";

pub struct CryptonoteApp {
    router: AppRouter<Screen, ()>,
    persistent: PersistentState<()>,
    temporary: TemporaryState,
    toast: ToastState,
    sidebar_collapsed: bool,
    // async receivers
    clipboard_rx: Option<std::sync::mpsc::Receiver<Result<String, String>>>,
    clipboard_write_rx: Option<std::sync::mpsc::Receiver<Result<(), String>>>,
    share_rx: Option<std::sync::mpsc::Receiver<Result<(), String>>>,
    download_rx: Option<std::sync::mpsc::Receiver<Result<String, String>>>,
    pick_rx: Option<std::sync::mpsc::Receiver<Result<Vec<(String, Vec<u8>)>, String>>>,
    pick_cancel: Option<functora_egui::CancelToken>,
    generate_rx: Option<std::sync::mpsc::Receiver<Result<External, String>>>,
    decrypt_rx: Option<std::sync::mpsc::Receiver<Result<String, String>>>,
    archive_rx: Option<std::sync::mpsc::Receiver<Result<Screen, String>>>,
    qr_state: functora_egui::QrScannerState,
    qr_continuous: bool,
}

impl Default for CryptonoteApp {
    fn default() -> Self {
        Self {
            router: AppRouter::new(&mut (), Screen::default()),
            persistent: PersistentState::default(),
            temporary: TemporaryState::default(),
            toast: ToastState::new(),
            sidebar_collapsed: true,
            clipboard_rx: None,
            clipboard_write_rx: None,
            share_rx: None,
            download_rx: None,
            pick_rx: None,
            pick_cancel: None,
            generate_rx: None,
            decrypt_rx: None,
            archive_rx: None,
            qr_state: functora_egui::QrScannerState::new(),
            qr_continuous: false,
        }
    }
}

impl CryptonoteApp {
    #[must_use]
    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        functora_egui::setup_fonts(&cc.egui_ctx);
        let persistent = PersistentState::load_or_default(&cc.egui_ctx, PERSISTENT_KEY, ());
        functora_egui::theme_extra::set_theme(&cc.egui_ctx, persistent.theme);
        let mut this = Self {
            persistent,
            ..Default::default()
        };
        let width = cc.egui_ctx.input(|i| i.viewport_rect().width());
        this.sidebar_collapsed = if width == 0.0 {
            true
        } else {
            width < functora_egui::Breakpoint::MOBILE_MAX_WIDTH
        };
        #[cfg(target_arch = "wasm32")]
        {
            let mut tmp = ();
            let router = AppRouter::new(&mut tmp, Screen::default());
            let current = *router.current();
            this.router = router;
            this.temporary.screen = current;
        }
        this
    }

    fn lang(&self) -> Language {
        self.persistent.language
    }

    fn navigate(&mut self, screen: Screen) {
        self.temporary.screen = screen;
        self.router.navigate(&mut (), screen);
    }

    fn apply_theme(&self, ctx: &egui::Context) {
        functora_egui::theme_extra::set_theme(ctx, self.persistent.theme);
    }

    fn reset(&mut self) {
        self.temporary.reset();
        self.navigate(Screen::Home);
    }

    fn poll_receivers(&mut self, ctx: &egui::Context) {
        let mut needs_repaint = false;
        if self.clipboard_rx.is_some()
            || self.clipboard_write_rx.is_some()
            || self.share_rx.is_some()
            || self.download_rx.is_some()
            || self.pick_rx.is_some()
            || self.generate_rx.is_some()
            || self.decrypt_rx.is_some()
            || self.archive_rx.is_some()
        {
            needs_repaint = true;
        }
        if let Some(rx) = self.clipboard_rx.take() {
            match rx.try_recv() {
                Ok(Ok(text)) => {
                    if self.temporary.screen == Screen::Home && self.temporary.action == ActionMode::Create {
                        self.temporary.note = text;
                    } else if self.temporary.action == ActionMode::Open {
                        self.temporary.url_input = text;
                    } else if matches!(self.temporary.external, External::Note(_)) {
                        self.temporary.password = text;
                    }
                    self.toast.add("Pasted", ToastVariant::Success, ctx.input(|i| i.time));
                }
                Ok(Err(e)) => {
                    self.temporary.message = Some(Msg::Error(crate::error::MsgError::from(AppError::FunctoraEgui(
                        functora_egui::error::Error::JS(e),
                    ))));
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => self.clipboard_rx = Some(rx),
                Err(_) => {}
            }
        }
        if let Some(rx) = self.clipboard_write_rx.take() {
            match rx.try_recv() {
                Ok(Ok(())) => self.toast.add("Copied", ToastVariant::Success, ctx.input(|i| i.time)),
                Ok(Err(e)) => {
                    self.temporary.message = Some(Msg::Error(crate::error::MsgError::from(AppError::FunctoraEgui(
                        functora_egui::error::Error::JS(e),
                    ))))
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => self.clipboard_write_rx = Some(rx),
                Err(_) => {}
            }
        }
        if let Some(rx) = self.share_rx.take() {
            match rx.try_recv() {
                Ok(Ok(())) => self.toast.add("Shared", ToastVariant::Success, ctx.input(|i| i.time)),
                Ok(Err(e)) => {
                    self.temporary.message = Some(Msg::Error(crate::error::MsgError::from(AppError::FunctoraEgui(
                        functora_egui::error::Error::JS(e),
                    ))))
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => self.share_rx = Some(rx),
                Err(_) => {}
            }
        }
        if let Some(rx) = self.download_rx.take() {
            match rx.try_recv() {
                Ok(Ok(name)) => {
                    self.temporary.message = Some(Msg::Downloaded(name.clone()));
                    self.toast.add(
                        format!("Downloaded {name}"),
                        ToastVariant::Success,
                        ctx.input(|i| i.time),
                    );
                    clear_progress(&mut self.temporary.progress);
                }
                Ok(Err(e)) => {
                    self.temporary.message = Some(Msg::Error(crate::error::MsgError::from(AppError::FunctoraEgui(
                        functora_egui::error::Error::JS(e),
                    ))));
                    clear_progress(&mut self.temporary.progress);
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => self.download_rx = Some(rx),
                Err(_) => clear_progress(&mut self.temporary.progress),
            }
        }
        if let Some(rx) = self.pick_rx.take() {
            match rx.try_recv() {
                Ok(Ok(files)) => {
                    for (name, data) in files {
                        let att = functora_egui::files::Attachment {
                            name: name.clone(),
                            data: data.into(),
                        };
                        crate::hooks::add_attachment(&mut self.temporary.attachments, att);
                    }
                    clear_progress(&mut self.temporary.progress);
                    self.pick_cancel = None;
                }
                Ok(Err(e)) => {
                    if e != "Cancelled" && !e.contains("cancelled") {
                        self.temporary.message = Some(Msg::Error(crate::error::MsgError::from(AppError::Archive(e))));
                    }
                    clear_progress(&mut self.temporary.progress);
                    self.pick_cancel = None;
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => self.pick_rx = Some(rx),
                Err(_) => {
                    clear_progress(&mut self.temporary.progress);
                    self.pick_cancel = None;
                }
            }
        }
        if let Some(rx) = self.generate_rx.take() {
            match rx.try_recv() {
                Ok(Ok(external)) => {
                    self.temporary.external = external;
                    clear_progress(&mut self.temporary.progress);
                    self.navigate(Screen::Share);
                }
                Ok(Err(e)) => {
                    self.temporary.message = Some(Msg::Error(crate::error::MsgError::from(AppError::Archive(e))));
                    clear_progress(&mut self.temporary.progress);
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => self.generate_rx = Some(rx),
                Err(_) => clear_progress(&mut self.temporary.progress),
            }
        }
        if let Some(rx) = self.decrypt_rx.take() {
            match rx.try_recv() {
                Ok(Ok(text)) => {
                    self.temporary.note = text;
                    self.temporary.external = External::Nothing;
                    clear_progress(&mut self.temporary.progress);
                    self.navigate(Screen::View);
                }
                Ok(Err(e)) => {
                    self.temporary.message = Some(Msg::Error(crate::error::MsgError::from(AppError::InvalidFormat(e))));
                    clear_progress(&mut self.temporary.progress);
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => self.decrypt_rx = Some(rx),
                Err(_) => clear_progress(&mut self.temporary.progress),
            }
        }
        if let Some(rx) = self.archive_rx.take() {
            match rx.try_recv() {
                Ok(Ok(screen)) => {
                    clear_progress(&mut self.temporary.progress);
                    self.navigate(screen);
                }
                Ok(Err(e)) => {
                    self.temporary.message = Some(Msg::Error(crate::error::MsgError::from(AppError::Archive(e))));
                    clear_progress(&mut self.temporary.progress);
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => self.archive_rx = Some(rx),
                Err(_) => clear_progress(&mut self.temporary.progress),
            }
        }
        if needs_repaint
            || self.clipboard_rx.is_some()
            || self.clipboard_write_rx.is_some()
            || self.share_rx.is_some()
            || self.download_rx.is_some()
            || self.pick_rx.is_some()
            || self.generate_rx.is_some()
            || self.decrypt_rx.is_some()
            || self.archive_rx.is_some()
        {
            ctx.request_repaint();
        }
    }

    fn handle_deep_link(&mut self) {
        if let Some(url) = functora_egui::deep_link::poll_deep_link()
            && let Ok(note) = extract_note_param(&url)
            && let Ok(data) = decode_note(&note)
        {
            match data {
                NoteData::CipherText(enc) => {
                    self.temporary.external = External::Note(crate::state::ExternalNote {
                        data: NoteData::CipherText(enc),
                        url: String::new(),
                        qr: String::new(),
                    });
                    self.navigate(Screen::Open);
                }
                NoteData::PlainText(text) => {
                    self.temporary.note = text;
                    self.temporary.cipher = None;
                    self.temporary.external = External::Nothing;
                    self.navigate(Screen::View);
                }
            }
        }
        if let Some(source) = crate::deep_link::take_archive() {
            let tmp = std::sync::Arc::new(std::sync::Mutex::new(None::<Job<Stage>>));
            let _ = tmp;
            let rx = functora_egui::spawn_async(async move {
                let meta = crate::archive::read_archive_metadata(&source);
                match meta {
                    Ok(m) => {
                        if m.cipher.is_some() {
                            Ok(Screen::Open)
                        } else {
                            Ok(Screen::View)
                        }
                    }
                    Err(e) => Err(e.to_string()),
                }
            });
            self.archive_rx = Some(rx);
        }
    }

    fn screen_home(&mut self, ui: &mut egui::Ui) {
        let lang = self.lang();
        let message = self.temporary.message.clone();
        let has_message = message.is_some();
        if let Some(msg) = message.clone() {
            let variant = match &msg {
                Msg::Error(_) => functora_egui::AlertVariant::Destructive,
                _ => functora_egui::AlertVariant::Default,
            };
            _ = Alert::new().title(msg.render(lang)).variant(variant).show(ui, |ui| {
                _ = ui.label(msg.render(lang));
            });
            let () = ui.add_space(8.0);
            if ui
                .add(
                    Button::new("Dismiss")
                        .variant(ButtonVariant::Outline)
                        .size(functora_egui::ComponentSize::Sm),
                )
                .clicked()
            {
                self.temporary.message = None;
            }
            let () = ui.add_space(12.0);
        }
        // Action selector
        let () = ui.add_space(4.0);
        _ = Label::new("Action").show(ui);
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

    fn home_create(&mut self, ui: &mut egui::Ui) {
        let lang = self.lang();
        _ = Label::new(Msg::Mode.render(lang)).show(ui);
        let () = ui.add_space(4.0);
        _ = Flex::row().gap(8.0).wrap().show(ui, |f| {
            for (cipher_opt, label) in [
                (None, "No encryption"),
                (Some(crate::crypto::CipherType::Aes256Gcm), "AES-256-GCM"),
                (Some(crate::crypto::CipherType::ChaCha20Poly1305), "ChaCha20-Poly1305"),
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
            _ = Label::new("Password").show(ui);
            let () = ui.add_space(4.0);
            _ = ui.add(
                Input::new(&mut self.temporary.password)
                    .placeholder("Password")
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
                                Err(e) => Err(e.to_string()),
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
                        .map_err(|e| e.to_string())
                });
                self.pick_rx = Some(rx);
            }
            if f.add(
                Button::new("Paste")
                    .icon(functora_egui::LucideIcon::ClipboardPaste)
                    .variant(ButtonVariant::Outline),
            )
            .inner
            .clicked()
            {
                let rx = functora_egui::spawn_async(async move {
                    functora_egui::clipboard::read().await.map_err(|e| e.to_string())
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

    fn home_open(&mut self, ui: &mut egui::Ui) {
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
                    let files = functora_egui::files::pick_files_with_cancel(false, None, Some(&cancel))
                        .await
                        .map_err(|e| e.to_string())?;
                    files
                        .into_iter()
                        .next()
                        .ok_or_else(|| "No file selected".to_string())
                        .map(|(_, data)| data)
                });
                let arc_rx: std::sync::mpsc::Receiver<Result<Vec<u8>, String>> = rx;
                let rx2 = functora_egui::spawn_async(async move {
                    match arc_rx.recv() {
                        Ok(Ok(bytes)) => {
                            let source = crate::archive::ArchiveSource::Bytes(bytes);
                            match crate::archive::read_archive_metadata(&source) {
                                Ok(meta) => {
                                    if meta.cipher.is_some() {
                                        Ok(Screen::Open)
                                    } else {
                                        Ok(Screen::View)
                                    }
                                }
                                Err(e) => Err(e.to_string()),
                            }
                        }
                        Ok(Err(e)) => Err(e),
                        Err(e) => Err(e.to_string()),
                    }
                });
                self.archive_rx = Some(rx2);
                clear_progress(&mut self.temporary.progress);
            }
            if f.add(
                Button::new("Paste")
                    .icon(functora_egui::LucideIcon::ClipboardPaste)
                    .variant(ButtonVariant::Outline),
            )
            .inner
            .clicked()
            {
                let rx = functora_egui::spawn_async(async move {
                    functora_egui::clipboard::read().await.map_err(|e| e.to_string())
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

    fn home_scan(&mut self, ui: &mut egui::Ui) {
        let lang = self.lang();
        let () = ui.add_space(8.0);
        _ = functora_egui::QrScanner::new()
            .continuous(self.qr_continuous)
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
            let label = if self.qr_continuous {
                "Continuous: ON"
            } else {
                "Continuous: OFF"
            };
            if f.add(
                Button::new(label)
                    .variant(ButtonVariant::Outline)
                    .size(functora_egui::ComponentSize::Sm),
            )
            .inner
            .clicked()
            {
                self.qr_continuous = !self.qr_continuous;
            }
        });
        if let Some(job) = self.temporary.progress.clone() {
            let () = ui.add_space(8.0);
            _ = ui.add(Progress::new(f32::from(job.percent()) / 100.0));
        }
    }

    fn show_attachments(&mut self, ui: &mut egui::Ui) {
        if self.temporary.attachments.is_empty() {
            return;
        }
        let () = ui.add_space(8.0);
        _ = Label::new(format!("Attachments: {}", self.temporary.attachments.len())).show(ui);
        let () = ui.add_space(4.0);
        let mut to_remove: Option<AttachmentIdx> = None;
        let mut to_open: Option<AttachmentIdx> = None;
        for (idx, att) in self.temporary.attachments.iter().enumerate() {
            let size = format_size(att.data.len() as u64);
            _ = Flex::row().gap(8.0).show(ui, |f| {
                _ = f.ui(|ui| {
                    _ = ui.label(format!("{} ({})", att.name, size));
                });
                if f.add(
                    Button::new("Open")
                        .icon(functora_egui::LucideIcon::Eye)
                        .variant(ButtonVariant::Ghost)
                        .size(functora_egui::ComponentSize::Sm),
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
                        .size(functora_egui::ComponentSize::Sm),
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

    fn screen_open(&mut self, ui: &mut egui::Ui) {
        let lang = self.lang();
        let is_encrypted = match &self.temporary.external {
            External::Note(n) => matches!(n.data, NoteData::CipherText(_)),
            External::Archive(_) => true,
            External::Nothing => false,
        };
        if let Some(msg) = self.temporary.message.clone() {
            _ = Alert::new()
                .title(msg.render(lang))
                .variant(functora_egui::AlertVariant::Destructive)
                .show(ui, |ui| {
                    _ = ui.label(msg.render(lang));
                });
            let () = ui.add_space(8.0);
        }
        if is_encrypted {
            _ = Label::new(Msg::EncryptedNote.render(lang)).show(ui);
            let () = ui.add_space(4.0);
            _ = Label::new(Msg::EncryptedNoteDesc.render(lang)).show(ui);
            let () = ui.add_space(8.0);
            _ = Label::new("Password").show(ui);
            let () = ui.add_space(4.0);
            let resp = ui.add(
                Input::new(&mut self.temporary.password)
                    .placeholder("Password")
                    .password(),
            );
            if resp.lost_focus() && ui.input(|i| i.key_pressed(egui::Key::Enter)) {
                self.decrypt_current();
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
                    self.decrypt_current();
                }
                if f.add(
                    Button::new("Paste")
                        .icon(functora_egui::LucideIcon::ClipboardPaste)
                        .variant(ButtonVariant::Outline),
                )
                .inner
                .clicked()
                {
                    let rx = functora_egui::spawn_async(async move {
                        functora_egui::clipboard::read().await.map_err(|e| e.to_string())
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

    fn decrypt_current(&mut self) {
        if self.temporary.password.is_empty() {
            self.temporary.message = Some(Msg::Base(functora_egui::messages::Msg::PasswordRequired));
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
                        let res: Result<String, String> = (|| {
                            let decrypted = crate::crypto::decrypt_symmetric(&enc_clone, &password_clone)
                                .map_err(|e| e.to_string())?;
                            String::from_utf8(decrypted).map_err(|e| e.to_string())
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
                            .map_err(|e| e.to_string())
                    })
                    .recv()
                    .unwrap_or_else(|_| Err("Cancelled".to_string()))
                });
                self.decrypt_rx = Some(rx);
            }
            External::Nothing => {
                clear_progress(&mut self.temporary.progress);
            }
        }
    }

    fn screen_view(&mut self, ui: &mut egui::Ui) {
        let lang = self.lang();
        if let Some(msg) = self.temporary.message.clone()
            && matches!(msg, Msg::Error(_))
        {
            _ = Alert::new()
                .title(msg.render(lang))
                .variant(functora_egui::AlertVariant::Destructive)
                .show(ui, |ui| {
                    _ = ui.label(msg.render(lang));
                });
            let () = ui.add_space(8.0);
        }
        if self.temporary.note.is_empty() && self.temporary.attachments.is_empty() {
            _ = Alert::new()
                .title(Msg::Error(crate::error::MsgError::from(AppError::NoNoteInUrl)).render(lang))
                .variant(functora_egui::AlertVariant::Destructive)
                .show(ui, |ui| {
                    _ = ui.label(Msg::Error(crate::error::MsgError::from(AppError::NoNoteInUrl)).render(lang));
                });
            let () = ui.add_space(8.0);
        } else {
            let rendered = crate::markdown::render_markdown(&self.temporary.note);
            _ = Card::new().show(ui, |ui| {
                let _ = egui::ScrollArea::vertical().max_height(400.0).show(ui, |ui| {
                    _ = ui.label(egui::RichText::new(&rendered).color(ui.visuals().text_color()));
                    // Since we render markdown to HTML, we display as text fallback; actual markdown html can't be rendered in egui easily, show raw?
                    // We'll show markdown source as well.
                });
            });
            let () = ui.add_space(8.0);
            if !self.temporary.note.is_empty() {
                _ = ui.collapsing("Markdown preview (raw HTML)", |ui| {
                    _ = ui.label(egui::RichText::new(rendered).small());
                });
                let () = ui.add_space(8.0);
            }
            if !self.temporary.attachments.is_empty() {
                _ = Separator::horizontal().show(ui);
                let () = ui.add_space(8.0);
                _ = Label::new(format!("Attachments ({})", self.temporary.attachments.len())).show(ui);
                let () = ui.add_space(4.0);
                let attachments = self.temporary.attachments.clone();
                for (idx, att) in attachments.iter().enumerate() {
                    let size = format_size(att.data.len() as u64);
                    let name = att.name.clone();
                    _ = Flex::row().gap(8.0).show(ui, |f| {
                        let _ = f.ui(|ui| {
                            _ = ui.label(format!("{name} ({size})"));
                        });
                        if f.add(
                            Button::new("Open")
                                .icon(functora_egui::LucideIcon::Eye)
                                .size(functora_egui::ComponentSize::Sm),
                        )
                        .inner
                        .clicked()
                        {
                            self.temporary.attachment = Some(AttachmentIdx(idx));
                            self.navigate(Screen::File);
                        }
                        if f.add(
                            Button::new("Download")
                                .icon(functora_egui::LucideIcon::Download)
                                .size(functora_egui::ComponentSize::Sm),
                        )
                        .inner
                        .clicked()
                        {
                            let att_clone = att.clone();
                            if claim_job(&mut self.temporary.progress, Stage::Download).is_some() {
                                let rx = functora_egui::spawn_async(async move {
                                    functora_egui::download::download(att_clone.data.to_vec(), &att_clone.name)
                                        .await
                                        .map_err(|e| e.to_string())
                                });
                                self.download_rx = Some(rx);
                            }
                        }
                    });
                    let () = ui.add_space(4.0);
                    // preview via functora_egui::files::preview
                    let preview = functora_egui::files::preview(&att.name, &att.data);
                    match preview {
                        functora_egui::files::Preview::Text(t) => {
                            _ = ui.label(egui::RichText::new(t.chars().take(300).collect::<String>()).small());
                        }
                        functora_egui::files::Preview::Image(_) => {
                            _ = ui.label(format!("Image preview available for {}", att.name));
                        }
                        _ => {}
                    }
                    let () = ui.add_space(4.0);
                }
                let () = ui.add_space(8.0);
                if ui
                    .add(
                        Button::new(Msg::DownloadAll.render(lang))
                            .icon(functora_egui::LucideIcon::Download)
                            .variant(ButtonVariant::Outline),
                    )
                    .clicked()
                {
                    let files = self.temporary.attachments.clone();
                    if claim_job(&mut self.temporary.progress, Stage::Zip).is_some() {
                        let rx = functora_egui::spawn_async(async move {
                            let zipped = functora_egui::zip::create_zip_async(&files, |_| {}, Stage::Zip)
                                .await
                                .map_err(|e| e.to_string())?;
                            functora_egui::download::download(zipped, "cryptonote-unlocked.zip")
                                .await
                                .map_err(|e| e.to_string())
                        });
                        self.download_rx = Some(rx);
                    }
                }
            }
        }
        let () = ui.add_space(12.0);
        let toast_time = ui.ctx().input(|i| i.time);
        _ = Flex::row().gap(8.0).wrap().show(ui, |f| {
            if f.add(
                Button::new("Copy")
                    .icon(functora_egui::LucideIcon::Copy)
                    .variant(ButtonVariant::Default),
            )
            .inner
            .clicked()
            {
                let text = self.temporary.note.clone();
                let rx = functora_egui::spawn_async(async move {
                    functora_egui::clipboard::write(text).await.map_err(|e| e.to_string())
                });
                self.clipboard_write_rx = Some(rx);
            }
            if f.add(
                Button::new(Msg::Share.render(lang))
                    .icon(functora_egui::LucideIcon::Share2)
                    .variant(ButtonVariant::Default),
            )
            .inner
            .clicked()
            {
                if let Some(err) = share_error(self.temporary.cipher, &self.temporary.password) {
                    self.temporary.message = Some(err);
                } else if !matches!(self.temporary.external, External::Nothing) {
                    self.navigate(Screen::Share);
                } else if claim_job(&mut self.temporary.progress, Stage::Encrypt).is_some() {
                    let note = self.temporary.note.clone();
                    let password = self.temporary.password.clone();
                    let cipher = self.temporary.cipher;
                    let attachments = self.temporary.attachments.clone();
                    let rx = functora_egui::spawn_async(async move {
                        crate::hooks::build_external(&note, &password, cipher, &attachments, |_| {})
                            .await
                            .map_err(|e| e.to_string())
                    });
                    self.generate_rx = Some(rx);
                }
            }
            if f.add(
                Button::new(Msg::Print.render(lang))
                    .icon(functora_egui::LucideIcon::Printer)
                    .variant(ButtonVariant::Outline),
            )
            .inner
            .clicked()
            {
                // Print not directly supported in egui; copy to clipboard fallback
                let text = self.temporary.note.clone();
                let rx = functora_egui::spawn_async(async move {
                    functora_egui::clipboard::write(text).await.map_err(|e| e.to_string())
                });
                self.clipboard_write_rx = Some(rx);
                self.toast.add("Copied for printing", ToastVariant::Success, toast_time);
            }
            if f.add(
                Button::new(Msg::EditNote.render(lang))
                    .icon(functora_egui::LucideIcon::SquarePen)
                    .variant(ButtonVariant::Outline),
            )
            .inner
            .clicked()
            {
                self.temporary.action = ActionMode::Create;
                self.navigate(Screen::Home);
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

    fn screen_share(&mut self, ui: &mut egui::Ui) {
        let lang = self.lang();
        let (url, qr) = match &self.temporary.external {
            External::Note(n) => (n.url.clone(), n.qr.clone()),
            _ => (String::new(), String::new()),
        };
        let pkg_ready = matches!(self.temporary.external, External::Archive(_));
        if let Some(msg) = self.temporary.message.clone() {
            _ = Alert::new()
                .title(msg.render(lang))
                .variant(functora_egui::AlertVariant::Default)
                .show(ui, |ui| {
                    _ = ui.label(msg.render(lang));
                });
            let () = ui.add_space(8.0);
        }
        if pkg_ready {
            _ = ui.label(egui::RichText::new(Msg::ArchiveReady.render(lang)).size(16.0).strong());
            let () = ui.add_space(12.0);
        } else if !url.is_empty() {
            if !qr.is_empty() {
                let _ = egui::ScrollArea::vertical().show(ui, |ui| {
                    _ = ui.label(egui::RichText::new("QR Code (SVG):").strong());
                    _ = ui.add(
                        Textarea::new(&mut qr.clone())
                            .min_height(90.0)
                            .desired_width(ui.available_width()),
                    );
                    // Try to render QR as image if possible via egui Image?
                    // For now show SVG text; egui can't render SVG directly.
                });
                let () = ui.add_space(8.0);
            }
            _ = ui.add(
                Textarea::new(&mut url.clone())
                    .min_height(60.0)
                    .desired_width(ui.available_width()),
            );
            let () = ui.add_space(4.0);
            _ = ui.label(egui::RichText::new("Tap to copy URL").small().weak());
            if ui.input(|i| i.pointer.any_click()) {
                let u = url.clone();
                let rx = functora_egui::spawn_async(async move {
                    functora_egui::clipboard::write(u).await.map_err(|e| e.to_string())
                });
                self.clipboard_write_rx = Some(rx);
            }
            let () = ui.add_space(8.0);
        } else if self.temporary.message.is_some() {
            // already shown
        } else {
            _ = ui.label(Msg::Base(functora_egui::messages::Msg::Loading).render(lang));
            let () = ui.add_space(8.0);
            _ = ui.add(Progress::new(0.5));
        }
        let () = ui.add_space(12.0);
        if pkg_ready || !url.is_empty() {
            let toast_time2 = ui.ctx().input(|i| i.time);
            _ = Flex::row().gap(8.0).wrap().show(ui, |f| {
                if !url.is_empty() {
                    if f.add(
                        Button::new("Copy")
                            .icon(functora_egui::LucideIcon::Copy)
                            .variant(ButtonVariant::Default),
                    )
                    .inner
                    .clicked()
                    {
                        let u = url.clone();
                        let rx = functora_egui::spawn_async(async move {
                            functora_egui::clipboard::write(u).await.map_err(|e| e.to_string())
                        });
                        self.clipboard_write_rx = Some(rx);
                    }
                    if f.add(
                        Button::new(Msg::Share.render(lang))
                            .icon(functora_egui::LucideIcon::Share2)
                            .variant(ButtonVariant::Default),
                    )
                    .inner
                    .clicked()
                    {
                        let data = functora_egui::share::ShareData {
                            title: "Cryptonote".to_string(),
                            text: Msg::SharedNoteText.render(lang),
                            url: url.clone(),
                        };
                        let rx = functora_egui::spawn_async(async move {
                            functora_egui::share::share(data).await.map_err(|e| e.to_string())
                        });
                        self.share_rx = Some(rx);
                    }
                    if f.add(
                        Button::new(Msg::Print.render(lang))
                            .icon(functora_egui::LucideIcon::Printer)
                            .variant(ButtonVariant::Outline),
                    )
                    .inner
                    .clicked()
                    {
                        let u_clone = url.clone();
                        let rx = functora_egui::spawn_async(async move {
                            functora_egui::clipboard::write(u_clone)
                                .await
                                .map_err(|e| e.to_string())
                        });
                        self.clipboard_write_rx = Some(rx);
                        self.toast
                            .add("Copied URL for printing", ToastVariant::Success, toast_time2);
                    }
                }
                if pkg_ready
                    && f.add(
                        Button::new(Msg::Download.render(lang))
                            .icon(functora_egui::LucideIcon::Download)
                            .variant(ButtonVariant::Default),
                    )
                    .inner
                    .clicked()
                {
                    let bytes = match &self.temporary.external {
                        External::Archive(a) => a.clone().untag(),
                        _ => Vec::new(),
                    };
                    if !bytes.is_empty() && claim_job(&mut self.temporary.progress, Stage::Download).is_some() {
                        let rx = functora_egui::spawn_async(async move {
                            functora_egui::download::download(bytes, "archive.cryptonote")
                                .await
                                .map_err(|e| e.to_string())
                        });
                        self.download_rx = Some(rx);
                    }
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
                    Button::new(Msg::EditNote.render(lang))
                        .icon(functora_egui::LucideIcon::SquarePen)
                        .variant(ButtonVariant::Outline),
                )
                .inner
                .clicked()
                {
                    self.temporary.action = ActionMode::Create;
                    self.navigate(Screen::Home);
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
    }

    fn screen_file(&mut self, ui: &mut egui::Ui) {
        let lang = self.lang();
        let att_opt = self
            .temporary
            .attachment
            .and_then(|idx| self.temporary.attachments.get(idx.get()).cloned());
        if let Some(att) = att_opt.clone() {
            let size = format_size(att.data.len() as u64);
            _ = ui.label(egui::RichText::new(&att.name).size(18.0).strong());
            _ = ui.label(egui::RichText::new(size).small().weak());
            let () = ui.add_space(8.0);
            let preview = functora_egui::files::preview(&att.name, &att.data);
            match preview {
                functora_egui::files::Preview::Image(_) => {
                    _ = ui.label(format!("Image: {} (preview blob available)", att.name));
                    // Try to show image via egui Image from bytes if possible
                    let uri = format!("{BYTES_URI_PREFIX}{}", att.name);
                    _ = ui
                        .add(egui::Image::from_bytes(uri, att.data.clone()).max_width(ui.available_width().min(400.0)));
                }
                functora_egui::files::Preview::Text(t) => {
                    _ = ScrollArea::vertical().max_height(400.0).show(ui, |ui| {
                        _ = ui.label(egui::RichText::new(t).monospace());
                    });
                }
                functora_egui::files::Preview::Markdown(t) => {
                    let rendered = crate::markdown::render_markdown(&t);
                    _ = ui.label(rendered);
                }
                functora_egui::files::Preview::Video(url) => {
                    _ = ui.label(format!("Video: {url}"));
                }
                functora_egui::files::Preview::Audio(url) => {
                    _ = ui.label(format!("Audio: {url}"));
                }
                functora_egui::files::Preview::Pdf(url) => {
                    _ = ui.label(format!("PDF: {url}"));
                }
                functora_egui::files::Preview::Download => {
                    _ = ui.label(Msg::PreviewUnavailable.render(lang));
                }
                functora_egui::files::Preview::Missing => {
                    _ = ui.label(Msg::FileNotFound.render(lang));
                }
            }
            let () = ui.add_space(12.0);
            if ui
                .add(
                    Button::new(Msg::Download.render(lang))
                        .icon(functora_egui::LucideIcon::Download)
                        .variant(ButtonVariant::Default),
                )
                .clicked()
                && claim_job(&mut self.temporary.progress, Stage::Download).is_some()
            {
                let data = att.data.to_vec();
                let name = att.name.clone();
                let rx = functora_egui::spawn_async(async move {
                    functora_egui::download::download(data, &name)
                        .await
                        .map_err(|e| e.to_string())
                });
                self.download_rx = Some(rx);
            }
            if let Some(job) = self.temporary.progress.clone() {
                let () = ui.add_space(8.0);
                _ = ui.add(Progress::new(f32::from(job.percent()) / 100.0));
            }
        } else {
            _ = Alert::new()
                .title(Msg::FileNotFound.render(lang))
                .variant(functora_egui::AlertVariant::Destructive)
                .show(ui, |ui| {
                    _ = ui.label(Msg::FileNotFound.render(lang));
                });
        }
    }

    fn screen_about(&mut self, ui: &mut egui::Ui) {
        let lang = self.lang();
        _ = ScrollArea::vertical().show(ui, |ui| {
            _ = ui.label(egui::RichText::new("Cryptonote").size(22.0).strong());
            let () = ui.add_space(8.0);
            _ = ui.label(Msg::AboutText.render(lang));
            let () = ui.add_space(12.0);
            _ = Separator::horizontal().show(ui);
            let () = ui.add_space(8.0);
            if ui
                .add(
                    Button::new("Donate")
                        .icon(functora_egui::LucideIcon::Heart)
                        .variant(ButtonVariant::Outline),
                )
                .clicked()
            {
                self.navigate(Screen::Donate);
            }
            let () = ui.add_space(8.0);
            if ui
                .add(
                    Button::new("Back")
                        .icon(functora_egui::LucideIcon::ArrowLeft)
                        .variant(ButtonVariant::Ghost),
                )
                .clicked()
            {
                self.navigate(Screen::Home);
            }
        });
    }

    fn screen_donate(&mut self, ui: &mut egui::Ui) {
        _ = ui.label(egui::RichText::new("Donate").size(20.0).strong());
        let () = ui.add_space(8.0);
        _ = ui.label("Support Cryptonote development:");
        let () = ui.add_space(8.0);
        for block in functora_egui::white_label::donate_blocks() {
            _ = Card::new().show(ui, |ui| {
                _ = ui.label(egui::RichText::new(&block.label).strong());
                let () = ui.add_space(4.0);
                _ = ui.label(&block.address);
                let () = ui.add_space(4.0);
                if ui
                    .add(
                        Button::new("Copy")
                            .icon(functora_egui::LucideIcon::Copy)
                            .size(functora_egui::ComponentSize::Sm),
                    )
                    .clicked()
                {
                    let addr = block.address.clone();
                    let rx = functora_egui::spawn_async(async move {
                        functora_egui::clipboard::write(addr).await.map_err(|e| e.to_string())
                    });
                    self.clipboard_write_rx = Some(rx);
                }
            });
            let () = ui.add_space(8.0);
        }
        if ui
            .add(
                Button::new("Back")
                    .icon(functora_egui::LucideIcon::ArrowLeft)
                    .variant(ButtonVariant::Ghost),
            )
            .clicked()
        {
            self.navigate(Screen::Home);
        }
    }

    fn screen_license(&mut self, ui: &mut egui::Ui) {
        _ = ui.label(egui::RichText::new("License").size(20.0).strong());
        let () = ui.add_space(8.0);
        _ = ScrollArea::vertical().show(ui, |ui| {
            _ = ui.label("MIT License - see https://github.com/functora/functora.github.io for details.");
        });
        let () = ui.add_space(8.0);
        if ui
            .add(
                Button::new("Back")
                    .icon(functora_egui::LucideIcon::ArrowLeft)
                    .variant(ButtonVariant::Ghost),
            )
            .clicked()
        {
            self.navigate(Screen::Home);
        }
    }

    fn screen_privacy(&mut self, ui: &mut egui::Ui) {
        _ = ui.label(egui::RichText::new("Privacy").size(20.0).strong());
        let () = ui.add_space(8.0);
        _ = ScrollArea::vertical().show(ui, |ui| {
            _ = ui.label("Cryptonote is fully offline. No data leaves your device unless you explicitly share it. No tracking, no analytics, no servers.");
        });
        let () = ui.add_space(8.0);
        if ui
            .add(
                Button::new("Back")
                    .icon(functora_egui::LucideIcon::ArrowLeft)
                    .variant(ButtonVariant::Ghost),
            )
            .clicked()
        {
            self.navigate(Screen::Home);
        }
    }

    fn footer(ui: &mut egui::Ui) {
        let theme = ShadcnThemeExt::shadcn_theme(ui.ctx());
        _ = Separator::horizontal().show(ui);
        let () = ui.add_space(8.0);
        _ = ui.horizontal(|ui| {
            _ = ui.label(
                egui::RichText::new(format!(
                    "\u{00A9} {} Functora. v{}",
                    functora_egui::FUNCTORA_CORE_YEAR,
                    APP_ATTRS.vsn
                ))
                .size(11.0)
                .color(theme.muted_foreground),
            );
            _ = ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                if ui
                    .add(
                        Button::new("Privacy")
                            .variant(ButtonVariant::Ghost)
                            .size(functora_egui::ComponentSize::Sm),
                    )
                    .clicked()
                {
                    // no-op, footer link handled via breadcrumb
                }
            });
        });
        let () = ui.add_space(4.0);
        _ = ui.label(
            egui::RichText::new(format!("Cryptonote is free and open source. {}", APP_ATTRS.description))
                .size(10.0)
                .color(theme.muted_foreground),
        );
    }
}

impl eframe::App for CryptonoteApp {
    fn ui(&mut self, ui: &mut egui::Ui, _frame: &mut eframe::Frame) {
        let ctx = ui.ctx().clone();
        self.apply_theme(&ctx);
        self.poll_receivers(&ctx);
        self.handle_deep_link();
        self.router.ui(ui, &mut ());
        let routed = *self.router.current();
        if routed != self.temporary.screen {
            self.temporary.screen = routed;
        }
        let mut persistent = std::mem::take(&mut self.persistent);
        let prev_persistent = persistent.clone();
        let mut collapsed_val = self.sidebar_collapsed;
        let route = *self.router.current();
        let history = self.router.history().clone();
        let needs_reset = std::cell::Cell::new(false);
        let temporary_ptr: *mut TemporaryState = &raw mut self.temporary;
        let router_ptr: *mut AppRouter<Screen, ()> = &raw mut self.router;
        let theme_bg = ShadcnThemeExt::shadcn_theme(&ctx);
        let breadcrumb_action = Shell::new("Cryptonote", &mut collapsed_val, |side_ui| {
            let temporary_ref = unsafe { &mut *temporary_ptr };
            let router_ref = unsafe { &mut *router_ptr };
            let mut close = false;
            let items: Vec<(Screen, &'static str, functora_egui::LucideIcon)> = vec![
                (Screen::Home, "Home", functora_egui::LucideIcon::House),
                (Screen::Open, "Open", functora_egui::LucideIcon::FolderOpen),
                (Screen::View, "View", functora_egui::LucideIcon::Eye),
                (Screen::Share, "Share", functora_egui::LucideIcon::Share2),
                (Screen::File, "File", functora_egui::LucideIcon::File),
                (Screen::About, "About", functora_egui::LucideIcon::Info),
                (Screen::Donate, "Donate", functora_egui::LucideIcon::Heart),
                (Screen::License, "License", functora_egui::LucideIcon::Scale),
                (Screen::Privacy, "Privacy", functora_egui::LucideIcon::Shield),
            ];
            for (screen, label, icon) in items {
                let selected = router_ref.current() == &screen || temporary_ref.screen == screen;
                let btn = Button::new(label)
                    .icon(icon)
                    .variant(if selected {
                        ButtonVariant::Default
                    } else {
                        ButtonVariant::Ghost
                    })
                    .selected(selected)
                    .full_width();
                if side_ui.add(btn).clicked() {
                    temporary_ref.screen = screen;
                    router_ref.navigate(&mut (), screen);
                    close |= side_ui.on_mobile();
                }
            }
            side_ui.add_space(12.0);
            let _ = Separator::horizontal().show(side_ui);
            side_ui.add_space(8.0);
            if side_ui
                .add(
                    Button::new("Reset")
                        .icon(functora_egui::LucideIcon::Trash2)
                        .variant(ButtonVariant::Ghost)
                        .full_width(),
                )
                .clicked()
            {
                temporary_ref.reset();
                let home = Screen::Home;
                temporary_ref.screen = home;
                router_ref.navigate(&mut (), home);
                close |= side_ui.on_mobile();
            }
            close
        })
        .version(APP_ATTRS.vsn)
        .theme(&mut persistent.theme)
        .language(&mut persistent.language)
        .on_brand(|| needs_reset.set(true))
        .sidebar_labels([
            "Home", "Open", "View", "Share", "File", "About", "Donate", "License", "Privacy",
        ])
        .breadcrumb(&route, &history)
        .show(ui, |content_ui| {
            match self.temporary.screen {
                Screen::Home => self.screen_home(content_ui),
                Screen::Open => self.screen_open(content_ui),
                Screen::View => self.screen_view(content_ui),
                Screen::Share => self.screen_share(content_ui),
                Screen::File => self.screen_file(content_ui),
                Screen::About => self.screen_about(content_ui),
                Screen::Donate => self.screen_donate(content_ui),
                Screen::License => self.screen_license(content_ui),
                Screen::Privacy => self.screen_privacy(content_ui),
            }
            content_ui.add_space(16.0);
            Self::footer(content_ui);
            content_ui.add_space(48.0);
        });
        self.sidebar_collapsed = collapsed_val;
        self.persistent = persistent;
        if prev_persistent != self.persistent {
            persist_value(PERSISTENT_KEY, &self.persistent);
            self.apply_theme(&ctx);
        }
        if needs_reset.get() {
            self.reset();
        }
        if let Some(action) = breadcrumb_action {
            match action {
                functora_egui::NavAction::Back => {
                    let _ = self.router.go_back(&mut ());
                    self.temporary.screen = *self.router.current();
                }
                functora_egui::NavAction::Forward => {
                    let _ = self.router.go_forward(&mut ());
                    self.temporary.screen = *self.router.current();
                }
                functora_egui::NavAction::Route(r) => {
                    self.navigate(r);
                }
            }
        }
        self.toast.show(&ctx);
        if let Some(job) = self.temporary.progress.clone() {
            _ = egui::Panel::bottom("progress_bottom")
                .frame(egui::Frame::NONE.fill(theme_bg.card))
                .show_separator_line(true)
                .show(ui, |bottom_ui| {
                    bottom_ui.add_space(4.0);
                    let _ = bottom_ui.add(Progress::new(f32::from(job.percent()) / 100.0));
                    let _ = bottom_ui.label(format!("{:?} {} / {}", job.stage, job.done, job.total));
                    bottom_ui.add_space(4.0);
                });
        }
    }
}
