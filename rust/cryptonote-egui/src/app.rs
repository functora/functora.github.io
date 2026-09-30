use std::str::FromStr;

use functora_egui::i18n::{I18N, Language};
use functora_egui::route::AppRouter;
use functora_egui::route::RouteMetadata;
use functora_egui::storage::persist_value;
use functora_egui::{
    Button, ButtonVariant, ComponentSize, Progress, ResponsiveExt, Separator, ShadcnThemeExt, Shell, ToastState,
    ToastVariant,
};

use crate::encoding::{NoteData, decode_note, extract_note_param};
use crate::error::AppError;
use crate::messages::Msg;
use crate::progress::{Stage, claim_job, clear_progress};
use crate::route::Screen;
use crate::state::{External, TemporaryState};
use crate::storage::{APP_ATTRS, PersistentState};
use functora_egui::messages::Msg as BaseMsg;

const PERSISTENT_KEY: &str = "cryptonote_persistent";
pub(crate) const BYTES_URI_PREFIX: &str = "bytes://";

type PickResult = Result<Vec<(String, Vec<u8>)>, AppError>;

pub struct CryptonoteApp {
    pub(crate) router: AppRouter<Screen, ()>,
    pub(crate) persistent: PersistentState<()>,
    pub temporary: TemporaryState,
    pub toast: ToastState,
    pub(crate) sidebar_collapsed: bool,
    // async receivers
    pub(crate) clipboard_write_rx: Option<std::sync::mpsc::Receiver<Result<(), AppError>>>,
    pub(crate) share_rx: Option<std::sync::mpsc::Receiver<Result<(), AppError>>>,
    pub(crate) download_rx: Option<std::sync::mpsc::Receiver<Result<String, AppError>>>,
    pub pick_rx: Option<std::sync::mpsc::Receiver<PickResult>>,
    pub(crate) pick_cancel: Option<functora_egui::CancelToken>,
    pub pick_overlay_open: bool,
    pub(crate) generate_rx: Option<std::sync::mpsc::Receiver<Result<External, AppError>>>,
    pub(crate) decrypt_rx: Option<std::sync::mpsc::Receiver<Result<String, AppError>>>,
    pub(crate) archive_rx: Option<std::sync::mpsc::Receiver<Result<crate::state::OpenedArchive, AppError>>>,
    pub(crate) pwa_rx: Option<std::sync::mpsc::Receiver<Result<functora_egui::messages::Msg, AppError>>>,
    preview_rx: Vec<std::sync::mpsc::Receiver<(String, functora_egui::files::Preview)>>,
    pub(crate) qr_state: functora_egui::QrScannerState,
    pub(crate) qr_error_notified: Option<String>,
    pub(crate) md_cache: functora_egui::CommonMarkCache,
}

impl Default for CryptonoteApp {
    fn default() -> Self {
        Self {
            router: AppRouter::new(&Screen::default()),
            persistent: PersistentState::default(),
            temporary: TemporaryState::default(),
            toast: ToastState::new(),
            sidebar_collapsed: true,
            clipboard_write_rx: None,
            share_rx: None,
            download_rx: None,
            pick_rx: None,
            pick_cancel: None,
            pick_overlay_open: false,
            generate_rx: None,
            decrypt_rx: None,
            archive_rx: None,
            pwa_rx: None,
            preview_rx: Vec::new(),
            qr_state: functora_egui::QrScannerState::new(),
            qr_error_notified: None,
            md_cache: functora_egui::CommonMarkCache::default(),
        }
    }
}

impl CryptonoteApp {
    #[must_use]
    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        functora_egui::setup_fonts(&cc.egui_ctx);
        functora_egui::setup_image_loaders(&cc.egui_ctx);
        let persistent = PersistentState::load_or_default(&cc.egui_ctx, PERSISTENT_KEY, ());
        functora_egui::theme_extra::set_theme(&cc.egui_ctx, persistent.theme);
        let mut this = Self {
            persistent,
            ..Default::default()
        };
        this.sidebar_collapsed = functora_egui::initial_sidebar_collapsed(&cc.egui_ctx);
        #[cfg(target_arch = "wasm32")]
        {
            let router = AppRouter::new(&Screen::default());
            let current = *router.current();
            this.router = router;
            this.temporary.screen = current;
        }
        this
    }

    pub(crate) fn lang(&self) -> Language {
        self.persistent.language
    }

    pub(crate) fn navigate(&mut self, screen: Screen) {
        self.temporary.screen = screen;
        self.router.navigate(&mut (), screen);
    }

    fn apply_theme(&self, ctx: &egui::Context) {
        functora_egui::theme_extra::set_theme(ctx, self.persistent.theme);
    }

    pub(crate) fn reset(&mut self) {
        self.temporary.reset();
        self.router.reset(&mut (), Screen::Home);
        self.temporary.screen = Screen::Home;
    }

    /// Edge-trigger for scanner errors: true only the first time a message
    /// appears. The scanner holds its error until the user retries, so a
    /// level-triggered toast here would spam every frame.
    pub fn unseen_qr_error(&mut self, message: &str) -> bool {
        if self.qr_error_notified.as_deref() == Some(message) {
            false
        } else {
            self.qr_error_notified = Some(message.to_owned());
            true
        }
    }

    pub(crate) fn paste_clear_feedback(&mut self, resp: functora_egui::InputPasteClearResponse, now: f64) {
        let lang = self.lang();
        if resp.pasted || resp.copied {
            self.toast.add(BaseMsg::Copied.render(lang), ToastVariant::Success, now);
        }
        if let Some(clipboard_error) = resp.clipboard_error {
            let app_error = AppError::from(clipboard_error);
            if !matches!(&app_error, AppError::Cancelled)
                && !matches!(&app_error, AppError::FunctoraEgui(inner) if *inner == functora_egui::error::Error::Cancelled)
            {
                self.toast.add(
                    Msg::Error(crate::error::MsgError::from(app_error)).render(lang),
                    ToastVariant::Error,
                    now,
                );
            }
        }
    }

    fn is_cancelled(error: &AppError) -> bool {
        matches!(error, AppError::Cancelled)
            || matches!(error, AppError::FunctoraEgui(inner) if *inner == functora_egui::error::Error::Cancelled)
    }

    fn generate_previews(&mut self) {
        let new_attachments: Vec<_> = self
            .temporary
            .attachments
            .iter()
            .filter(|att| !self.temporary.preview_cache.contains_key(&att.name))
            .cloned()
            .collect();
        for att in new_attachments {
            let name = att.name.clone();
            let data = att.data.clone();
            let rx = functora_egui::spawn_async(async move {
                let preview = functora_egui::files::preview(&name, &data);
                (name, preview)
            });
            self.preview_rx.push(rx);
        }
    }

    fn poll_previews(&mut self) {
        let mut remaining = Vec::with_capacity(self.preview_rx.len());
        for rx in self.preview_rx.drain(..) {
            match rx.try_recv() {
                Ok((name, preview)) => {
                    let _ = self.temporary.preview_cache.insert(name, preview);
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => remaining.push(rx),
                Err(std::sync::mpsc::TryRecvError::Disconnected) => {}
            }
        }
        self.preview_rx = remaining;
    }

    fn cancel_all(&mut self) {
        self.clipboard_write_rx = None;
        self.share_rx = None;
        self.download_rx = None;
        self.pick_rx = None;
        self.generate_rx = None;
        self.decrypt_rx = None;
        self.archive_rx = None;
        self.pwa_rx = None;
        self.preview_rx.clear();
        clear_progress(&mut self.temporary.progress);
        self.pick_cancel = None;
        self.pick_overlay_open = false;
    }

    pub fn poll_receivers(&mut self, ctx: &egui::Context) {
        self.poll_previews();
        let lang = self.lang();
        let time = ctx.input(|i| i.time);

        if let Some(rx) = self.clipboard_write_rx.take() {
            match rx.try_recv() {
                Ok(Ok(())) => self
                    .toast
                    .add(BaseMsg::Copied.render(lang), ToastVariant::Success, time),
                Ok(Err(e)) => {
                    if !Self::is_cancelled(&e) {
                        self.toast.add(
                            Msg::Error(crate::error::MsgError::from(e)).render(lang),
                            ToastVariant::Error,
                            time,
                        );
                    }
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => self.clipboard_write_rx = Some(rx),
                Err(std::sync::mpsc::TryRecvError::Disconnected) => {}
            }
        }
        if let Some(rx) = self.share_rx.take() {
            match rx.try_recv() {
                Ok(Ok(())) => self.toast.add(Msg::Sent.render(lang), ToastVariant::Success, time),
                Ok(Err(e)) => {
                    if !Self::is_cancelled(&e) {
                        self.toast.add(
                            Msg::Error(crate::error::MsgError::from(e)).render(lang),
                            ToastVariant::Error,
                            time,
                        );
                    }
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => self.share_rx = Some(rx),
                Err(std::sync::mpsc::TryRecvError::Disconnected) => {}
            }
        }
        if let Some(rx) = self.download_rx.take() {
            match rx.try_recv() {
                Ok(Ok(name)) => {
                    self.toast
                        .add(Msg::Downloaded(name).render(lang), ToastVariant::Success, time);
                    clear_progress(&mut self.temporary.progress);
                }
                Ok(Err(e)) => {
                    if !Self::is_cancelled(&e) {
                        self.toast.add(
                            Msg::Error(crate::error::MsgError::from(e)).render(lang),
                            ToastVariant::Error,
                            time,
                        );
                    }
                    clear_progress(&mut self.temporary.progress);
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => self.download_rx = Some(rx),
                Err(std::sync::mpsc::TryRecvError::Disconnected) => clear_progress(&mut self.temporary.progress),
            }
        }
        if let Some(rx) = self.pick_rx.take() {
            match rx.try_recv() {
                Ok(Ok(files)) => {
                    let before = self.temporary.attachments.len();
                    for (name, data) in files {
                        let att = functora_egui::files::Attachment {
                            name,
                            data: data.into(),
                        };
                        crate::hooks::add_attachment(&mut self.temporary.attachments, att);
                    }
                    let added = self.temporary.attachments.len() - before;
                    self.toast
                        .add(BaseMsg::FilesAttached(added).render(lang), ToastVariant::Success, time);
                    clear_progress(&mut self.temporary.progress);
                    self.pick_cancel = None;
                    self.pick_overlay_open = false;
                    self.generate_previews();
                }
                Ok(Err(e)) => {
                    if Self::is_cancelled(&e) {
                        self.toast.add(e.render(lang), ToastVariant::Default, time);
                    } else {
                        self.toast.add(
                            Msg::Error(crate::error::MsgError::from(e)).render(lang),
                            ToastVariant::Error,
                            time,
                        );
                    }
                    clear_progress(&mut self.temporary.progress);
                    self.pick_cancel = None;
                    self.pick_overlay_open = false;
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => self.pick_rx = Some(rx),
                Err(std::sync::mpsc::TryRecvError::Disconnected) => {
                    clear_progress(&mut self.temporary.progress);
                    self.pick_cancel = None;
                    self.pick_overlay_open = false;
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
                    if !Self::is_cancelled(&e) {
                        self.toast.add(
                            Msg::Error(crate::error::MsgError::from(e)).render(lang),
                            ToastVariant::Error,
                            time,
                        );
                    }
                    clear_progress(&mut self.temporary.progress);
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => self.generate_rx = Some(rx),
                Err(std::sync::mpsc::TryRecvError::Disconnected) => clear_progress(&mut self.temporary.progress),
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
                    if !Self::is_cancelled(&e) {
                        self.toast.add(
                            Msg::Error(crate::error::MsgError::from(e)).render(lang),
                            ToastVariant::Error,
                            time,
                        );
                    }
                    clear_progress(&mut self.temporary.progress);
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => self.decrypt_rx = Some(rx),
                Err(std::sync::mpsc::TryRecvError::Disconnected) => clear_progress(&mut self.temporary.progress),
            }
        }
        if let Some(rx) = self.archive_rx.take() {
            match rx.try_recv() {
                Ok(Ok(opened)) => {
                    self.temporary.external = opened.external;
                    self.temporary.note = opened.note;
                    self.temporary.attachments = opened.attachments;
                    if opened.screen == Screen::Open {
                        self.temporary.password.clear();
                    }
                    clear_progress(&mut self.temporary.progress);
                    self.pick_cancel = None;
                    self.generate_previews();
                    self.navigate(opened.screen);
                }
                Ok(Err(e)) => {
                    if !Self::is_cancelled(&e) {
                        self.toast.add(
                            Msg::Error(crate::error::MsgError::from(e)).render(lang),
                            ToastVariant::Error,
                            time,
                        );
                    }
                    clear_progress(&mut self.temporary.progress);
                    self.pick_cancel = None;
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => self.archive_rx = Some(rx),
                Err(std::sync::mpsc::TryRecvError::Disconnected) => clear_progress(&mut self.temporary.progress),
            }
        }
        if let Some(rx) = self.pwa_rx.take() {
            match rx.try_recv() {
                Ok(Ok(msg)) => {
                    let variant = if matches!(msg, BaseMsg::PwaInstallSuccess) {
                        ToastVariant::Success
                    } else {
                        ToastVariant::Default
                    };
                    self.toast.add(Msg::Base(msg).render(lang), variant, time);
                }
                Ok(Err(e)) => {
                    if !Self::is_cancelled(&e) {
                        self.toast.add(
                            Msg::Error(crate::error::MsgError::from(e)).render(lang),
                            ToastVariant::Error,
                            time,
                        );
                    }
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => self.pwa_rx = Some(rx),
                Err(std::sync::mpsc::TryRecvError::Disconnected) => {}
            }
        }
        if self.clipboard_write_rx.is_some()
            || self.share_rx.is_some()
            || self.download_rx.is_some()
            || self.pick_rx.is_some()
            || self.generate_rx.is_some()
            || self.decrypt_rx.is_some()
            || self.archive_rx.is_some()
            || self.pwa_rx.is_some()
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
        if crate::deep_link::has_pending_archive()
            && claim_job(&mut self.temporary.progress, Stage::Preview).is_some()
            && let Some(source) = crate::deep_link::take_archive()
        {
            let rx = functora_egui::spawn_async(async move { crate::hooks::load_archive_async(source).await });
            self.archive_rx = Some(rx);
        }
    }

    fn footer(&mut self, ui: &mut egui::Ui, lang: Language) {
        // One text layout for the whole sentence: identical font, size and
        // baseline for plain text, the author link and the navigation
        // actions, on every screen width.
        let (_, clicked) = functora_egui::Hypertext::new()
            .text(BaseMsg::Copyright.render(lang))
            .text(format!("{} ", functora_egui::FUNCTORA_CORE_YEAR))
            .link("Functora", APP_ATTRS.author_url())
            .text(format!(". {} ", BaseMsg::AllRightsReserved.render(lang)))
            .text(format!("{} ", BaseMsg::ByContinuing.render(lang)))
            .action(BaseMsg::TermsOfService.render(lang), Screen::License.to_string())
            .text(format!(" {} ", BaseMsg::YouAgree.render(lang)))
            .action(BaseMsg::PrivacyPolicyAnd.render(lang), Screen::Privacy.to_string())
            .text(". ")
            .action(BaseMsg::DonateLink.render(lang), Screen::Donate.to_string())
            .text(format!(" {} ", BaseMsg::And.render(lang)))
            .action(BaseMsg::FooterShareWord.render(lang), Screen::About.to_string())
            .text(format!(
                " {} {} {}.",
                BaseMsg::FooterAppWord.render(lang),
                BaseMsg::VersionLabel.render(lang),
                APP_ATTRS.vsn
            ))
            .size(11.0)
            .centered()
            .show_action(ui);
        match clicked.as_deref().and_then(|id| Screen::from_str(id).ok()) {
            Some(Screen::License) => self.navigate(Screen::License),
            Some(Screen::Privacy) => self.navigate(Screen::Privacy),
            Some(Screen::Donate) => self.navigate(Screen::Donate),
            Some(Screen::About) => self.navigate(Screen::About),
            Some(Screen::Home | Screen::Open | Screen::View | Screen::Share | Screen::File) | None => {}
        }
    }
}

impl eframe::App for CryptonoteApp {
    fn ui(&mut self, ui: &mut egui::Ui, _frame: &mut eframe::Frame) {
        let ctx = ui.ctx().clone();
        self.apply_theme(&ctx);
        self.poll_receivers(&ctx);
        self.handle_deep_link();
        self.router.ui(ui, &mut ());
        #[cfg(target_os = "android")]
        functora_egui::android::poll_ime(&ctx);
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
        let pending_nav = std::cell::Cell::new(None::<Screen>);
        let selected_snapshot = self.temporary.screen;
        let lang_cell = std::cell::Cell::new(persistent.language);
        let theme_bg = ShadcnThemeExt::shadcn_theme(&ctx);
        let nav_items: Vec<(Screen, String, functora_egui::LucideIcon)> = [
            (Screen::Home, functora_egui::LucideIcon::House),
            (Screen::Open, functora_egui::LucideIcon::FolderOpen),
            (Screen::View, functora_egui::LucideIcon::Eye),
            (Screen::Share, functora_egui::LucideIcon::Share2),
            (Screen::File, functora_egui::LucideIcon::File),
            (Screen::About, functora_egui::LucideIcon::Info),
            (Screen::Donate, functora_egui::LucideIcon::Heart),
            (Screen::License, functora_egui::LucideIcon::Scale),
            (Screen::Privacy, functora_egui::LucideIcon::Shield),
        ]
        .iter()
        .map(|(screen, icon)| (*screen, screen.label(lang_cell.get()).into_owned(), *icon))
        .collect();
        let sidebar_names: Vec<String> = nav_items.iter().map(|(_, label, _)| label.clone()).collect();
        let breadcrumb_action = Shell::new("Cryptonote", &mut collapsed_val, {
            let lang_ref = &lang_cell;
            let pending_ref = &pending_nav;
            let reset_ref = &needs_reset;
            move |side_ui| {
                let mut close = false;
                for (screen, label, icon) in &nav_items {
                    let is_selected =
                        *screen == selected_snapshot && pending_ref.get().is_none_or(|next| next == *screen);
                    let btn = Button::new(label)
                        .icon(*icon)
                        .variant(if is_selected {
                            ButtonVariant::Default
                        } else {
                            ButtonVariant::Ghost
                        })
                        .selected(is_selected)
                        .full_width();
                    if side_ui.add(btn).clicked() {
                        pending_ref.set(Some(*screen));
                        close |= side_ui.on_mobile();
                        side_ui.ctx().request_repaint();
                    }
                }
                side_ui.add_space(12.0);
                let _ = Separator::horizontal().show(side_ui);
                side_ui.add_space(8.0);
                if side_ui
                    .add(
                        Button::new(Msg::CreateNewNote.render(lang_ref.get()))
                            .icon(functora_egui::LucideIcon::RotateCcw)
                            .variant(ButtonVariant::Ghost)
                            .full_width(),
                    )
                    .clicked()
                {
                    reset_ref.set(true);
                    close |= side_ui.on_mobile();
                }
                close
            }
        })
        .brand_icon(None)
        .theme(&mut persistent.theme)
        .language(&lang_cell)
        .on_brand(|| needs_reset.set(true))
        .sidebar_labels(sidebar_names.iter().map(String::as_str))
        .breadcrumb(&route, &history)
        .show(ui, |content_ui| {
            let content_lang = lang_cell.get();
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
            self.footer(content_ui, content_lang);
        });
        persistent.language = lang_cell.get();
        self.sidebar_collapsed = collapsed_val;
        self.persistent = persistent;
        if let Some(screen) = pending_nav.get() {
            self.navigate(screen);
            ctx.request_repaint();
        }
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
                    if bottom_ui.add(Button::new("Cancel").size(ComponentSize::Sm)).clicked() {
                        self.cancel_all();
                    }
                    bottom_ui.add_space(4.0);
                });
        }
    }
}
