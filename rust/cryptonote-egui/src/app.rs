#![allow(
    clippy::shadow_reuse,
    clippy::shadow_same,
    clippy::shadow_unrelated,
    clippy::type_complexity,
    clippy::too_many_lines
)]
use functora_egui::i18n::{I18N, Language};
use functora_egui::route::AppRouter;
use functora_egui::route::RouteMetadata;
use functora_egui::storage::persist_value;
use functora_egui::{
    Button, ButtonVariant, Progress, ResponsiveExt, Separator, ShadcnThemeExt, Shell, ToastState, ToastVariant,
};

use crate::encoding::{NoteData, decode_note, extract_note_param};
use crate::error::AppError;
use crate::messages::Msg;
use crate::progress::{Stage, claim_job, clear_progress};
use crate::route::Screen;
use crate::state::{ActionMode, External, TemporaryState};
use crate::storage::{APP_ATTRS, PersistentState};
use functora_egui::messages::Msg as BaseMsg;

const PERSISTENT_KEY: &str = "cryptonote_persistent";
pub(crate) const BYTES_URI_PREFIX: &str = "bytes://";

pub struct CryptonoteApp {
    pub(crate) router: AppRouter<Screen, ()>,
    pub(crate) persistent: PersistentState<()>,
    pub(crate) temporary: TemporaryState,
    pub(crate) toast: ToastState,
    pub(crate) sidebar_collapsed: bool,
    // async receivers
    pub(crate) clipboard_rx: Option<std::sync::mpsc::Receiver<Result<String, AppError>>>,
    pub(crate) clipboard_write_rx: Option<std::sync::mpsc::Receiver<Result<(), AppError>>>,
    pub(crate) share_rx: Option<std::sync::mpsc::Receiver<Result<(), AppError>>>,
    pub(crate) download_rx: Option<std::sync::mpsc::Receiver<Result<String, AppError>>>,
    pub(crate) pick_rx: Option<std::sync::mpsc::Receiver<Result<Vec<(String, Vec<u8>)>, AppError>>>,
    pub(crate) pick_cancel: Option<functora_egui::CancelToken>,
    pub(crate) pick_overlay_open: bool,
    pub(crate) generate_rx: Option<std::sync::mpsc::Receiver<Result<External, AppError>>>,
    pub(crate) decrypt_rx: Option<std::sync::mpsc::Receiver<Result<String, AppError>>>,
    pub(crate) archive_rx: Option<std::sync::mpsc::Receiver<Result<crate::state::OpenedArchive, AppError>>>,
    pub(crate) pwa_rx: Option<std::sync::mpsc::Receiver<Result<functora_egui::messages::Msg, AppError>>>,
    pub(crate) qr_state: functora_egui::QrScannerState,
    pub(crate) qr_error_notified: Option<String>,
    pub(crate) md_cache: functora_egui::CommonMarkCache,
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
            pick_overlay_open: false,
            generate_rx: None,
            decrypt_rx: None,
            archive_rx: None,
            pwa_rx: None,
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
    pub(crate) fn unseen_qr_error(&mut self, message: &str) -> bool {
        if self.qr_error_notified.as_deref() == Some(message) {
            false
        } else {
            self.qr_error_notified = Some(message.to_owned());
            true
        }
    }

    fn poll_receivers(&mut self, ctx: &egui::Context) {
        let lang = self.lang();
        let mut needs_repaint = false;
        if self.clipboard_rx.is_some()
            || self.clipboard_write_rx.is_some()
            || self.share_rx.is_some()
            || self.download_rx.is_some()
            || self.pick_rx.is_some()
            || self.generate_rx.is_some()
            || self.decrypt_rx.is_some()
            || self.archive_rx.is_some()
            || self.pwa_rx.is_some()
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
                    self.toast.add(
                        BaseMsg::Copied.render(lang),
                        ToastVariant::Success,
                        ctx.input(|i| i.time),
                    );
                }
                Ok(Err(e)) => {
                    if !matches!(&e, AppError::Cancelled)
                        && !matches!(&e, AppError::FunctoraEgui(inner) if *inner == functora_egui::error::Error::Cancelled)
                    {
                        self.toast.add(
                            Msg::Error(crate::error::MsgError::from(e)).render(lang),
                            ToastVariant::Error,
                            ctx.input(|i| i.time),
                        );
                    }
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => self.clipboard_rx = Some(rx),
                Err(_) => {}
            }
        }
        if let Some(rx) = self.clipboard_write_rx.take() {
            match rx.try_recv() {
                Ok(Ok(())) => self.toast.add(
                    BaseMsg::Copied.render(lang),
                    ToastVariant::Success,
                    ctx.input(|i| i.time),
                ),
                Ok(Err(e)) => {
                    if !matches!(&e, AppError::Cancelled)
                        && !matches!(&e, AppError::FunctoraEgui(inner) if *inner == functora_egui::error::Error::Cancelled)
                    {
                        self.toast.add(
                            Msg::Error(crate::error::MsgError::from(e)).render(lang),
                            ToastVariant::Error,
                            ctx.input(|i| i.time),
                        );
                    }
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => self.clipboard_write_rx = Some(rx),
                Err(_) => {}
            }
        }
        if let Some(rx) = self.share_rx.take() {
            match rx.try_recv() {
                Ok(Ok(())) => self
                    .toast
                    .add(Msg::Sent.render(lang), ToastVariant::Success, ctx.input(|i| i.time)),
                Ok(Err(e)) => {
                    if !matches!(&e, AppError::Cancelled)
                        && !matches!(&e, AppError::FunctoraEgui(inner) if *inner == functora_egui::error::Error::Cancelled)
                    {
                        self.toast.add(
                            Msg::Error(crate::error::MsgError::from(e)).render(lang),
                            ToastVariant::Error,
                            ctx.input(|i| i.time),
                        );
                    }
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => self.share_rx = Some(rx),
                Err(_) => {}
            }
        }
        if let Some(rx) = self.download_rx.take() {
            match rx.try_recv() {
                Ok(Ok(name)) => {
                    self.toast.add(
                        Msg::Downloaded(name).render(lang),
                        ToastVariant::Success,
                        ctx.input(|i| i.time),
                    );
                    clear_progress(&mut self.temporary.progress);
                }
                Ok(Err(e)) => {
                    if !matches!(&e, AppError::Cancelled)
                        && !matches!(&e, AppError::FunctoraEgui(inner) if *inner == functora_egui::error::Error::Cancelled)
                    {
                        self.toast.add(
                            Msg::Error(crate::error::MsgError::from(e)).render(lang),
                            ToastVariant::Error,
                            ctx.input(|i| i.time),
                        );
                    }
                    clear_progress(&mut self.temporary.progress);
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => self.download_rx = Some(rx),
                Err(_) => clear_progress(&mut self.temporary.progress),
            }
        }
        if let Some(rx) = self.pick_rx.take() {
            match rx.try_recv() {
                Ok(Ok(files)) => {
                    let before = self.temporary.attachments.len();
                    for (name, data) in files {
                        let att = functora_egui::files::Attachment {
                            name: name.clone(),
                            data: data.into(),
                        };
                        crate::hooks::add_attachment(&mut self.temporary.attachments, att);
                    }
                    let added = self.temporary.attachments.len() - before;
                    self.toast.add(
                        BaseMsg::FilesAttached(added).render(lang),
                        ToastVariant::Success,
                        ctx.input(|i| i.time),
                    );
                    clear_progress(&mut self.temporary.progress);
                    self.pick_cancel = None;
                    self.pick_overlay_open = false;
                }
                Ok(Err(e)) => {
                    if matches!(&e, AppError::Cancelled)
                        || matches!(&e, AppError::FunctoraEgui(inner) if *inner == functora_egui::error::Error::Cancelled)
                    {
                        self.toast
                            .add(e.render(lang), ToastVariant::Default, ctx.input(|i| i.time));
                    } else {
                        self.toast.add(
                            Msg::Error(crate::error::MsgError::from(e)).render(lang),
                            ToastVariant::Error,
                            ctx.input(|i| i.time),
                        );
                    }
                    clear_progress(&mut self.temporary.progress);
                    self.pick_cancel = None;
                    self.pick_overlay_open = false;
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => self.pick_rx = Some(rx),
                Err(_) => {
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
                    if !matches!(&e, AppError::Cancelled)
                        && !matches!(&e, AppError::FunctoraEgui(inner) if *inner == functora_egui::error::Error::Cancelled)
                    {
                        self.toast.add(
                            Msg::Error(crate::error::MsgError::from(e)).render(lang),
                            ToastVariant::Error,
                            ctx.input(|i| i.time),
                        );
                    }
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
                    if !matches!(&e, AppError::Cancelled)
                        && !matches!(&e, AppError::FunctoraEgui(inner) if *inner == functora_egui::error::Error::Cancelled)
                    {
                        self.toast.add(
                            Msg::Error(crate::error::MsgError::from(e)).render(lang),
                            ToastVariant::Error,
                            ctx.input(|i| i.time),
                        );
                    }
                    clear_progress(&mut self.temporary.progress);
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => self.decrypt_rx = Some(rx),
                Err(_) => clear_progress(&mut self.temporary.progress),
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
                    self.navigate(opened.screen);
                }
                Ok(Err(e)) => {
                    if !matches!(&e, AppError::Cancelled)
                        && !matches!(&e, AppError::FunctoraEgui(inner) if *inner == functora_egui::error::Error::Cancelled)
                    {
                        self.toast.add(
                            Msg::Error(crate::error::MsgError::from(e)).render(lang),
                            ToastVariant::Error,
                            ctx.input(|i| i.time),
                        );
                    }
                    clear_progress(&mut self.temporary.progress);
                    self.pick_cancel = None;
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => self.archive_rx = Some(rx),
                Err(_) => clear_progress(&mut self.temporary.progress),
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
                    self.toast
                        .add(Msg::Base(msg).render(lang), variant, ctx.input(|i| i.time));
                }
                Ok(Err(e)) => {
                    if !matches!(&e, AppError::Cancelled)
                        && !matches!(&e, AppError::FunctoraEgui(inner) if *inner == functora_egui::error::Error::Cancelled)
                    {
                        self.toast.add(
                            Msg::Error(crate::error::MsgError::from(e)).render(lang),
                            ToastVariant::Error,
                            ctx.input(|i| i.time),
                        );
                    }
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => self.pwa_rx = Some(rx),
                Err(_) => {}
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
        if let Some(source) = crate::deep_link::take_archive()
            && claim_job(&mut self.temporary.progress, Stage::Preview).is_some()
        {
            let rx = functora_egui::spawn_async(async move { crate::hooks::load_archive_async(source).await });
            self.archive_rx = Some(rx);
        }
    }

    fn footer(&mut self, ui: &mut egui::Ui, lang: Language) {
        _ = Separator::horizontal().show(ui);
        let () = ui.add_space(8.0);
        // One text layout for the whole sentence: identical font, size and
        // baseline for plain text, the author link and the navigation
        // actions, on every screen width.
        let (_, clicked) = functora_egui::Hypertext::new()
            .text(BaseMsg::Copyright.render(lang))
            .text(format!("{} ", functora_egui::FUNCTORA_CORE_YEAR))
            .link("Functora", APP_ATTRS.author_url())
            .text(format!(". {} ", BaseMsg::AllRightsReserved.render(lang)))
            .text(format!("{} ", BaseMsg::ByContinuing.render(lang)))
            .action(BaseMsg::TermsOfService.render(lang), "license")
            .text(format!(" {} ", BaseMsg::YouAgree.render(lang)))
            .action(BaseMsg::PrivacyPolicyAnd.render(lang), "privacy")
            .text(". ")
            .action(BaseMsg::DonateLink.render(lang), "donate")
            .text(format!(" {} ", BaseMsg::And.render(lang)))
            .action(BaseMsg::FooterShareWord.render(lang), "about")
            .text(format!(
                " {} {} {}.",
                BaseMsg::FooterAppWord.render(lang),
                BaseMsg::VersionLabel.render(lang),
                APP_ATTRS.vsn
            ))
            .size(11.0)
            .centered()
            .show_action(ui);
        match clicked.as_deref() {
            Some("license") => self.navigate(Screen::License),
            Some("privacy") => self.navigate(Screen::Privacy),
            Some("donate") => self.navigate(Screen::Donate),
            Some("about") => self.navigate(Screen::About),
            _ => {}
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
                let _ = lang_ref.get();
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
                            .icon(functora_egui::LucideIcon::Trash2)
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
        .version(APP_ATTRS.vsn)
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
            content_ui.add_space(48.0);
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
                    bottom_ui.add_space(4.0);
                });
        }
    }
}

#[cfg(test)]
mod qr_scan_toast_tests {
    use super::CryptonoteApp;

    /// Drives the scan screen across frames with no camera available: the
    /// scanner holds its error, so at most one error toast may fire no
    /// matter how many frames render.
    #[test]
    fn scan_error_toasts_once_across_frames() {
        let ctx = egui::Context::default();
        let mut app = CryptonoteApp::default();
        let toasts_before = app.toast.next_id();
        for frame in 0..30 {
            let raw = egui::RawInput {
                screen_rect: Some(egui::Rect::from_min_size(
                    egui::Pos2::ZERO,
                    egui::Vec2::new(1280.0, 800.0),
                )),
                time: Some(f64::from(frame) / 60.0),
                ..Default::default()
            };
            let mut out = ctx.run_ui(raw, |ui| {
                let _ = egui::CentralPanel::default().show(ui, |inner| app.home_scan(inner));
            });
            out.textures_delta.clear();
        }
        assert!(
            app.toast.next_id() - toasts_before <= 1,
            "scan failures must toast at most once, got {}",
            app.toast.next_id() - toasts_before
        );
    }

    #[test]
    fn unseen_qr_error_dedups_repeats() {
        let mut app = CryptonoteApp::default();
        assert!(app.unseen_qr_error("boom"));
        assert!(!app.unseen_qr_error("boom"));
        assert!(app.unseen_qr_error("different"));
    }
}

#[cfg(test)]
mod pick_feedback_tests {
    use super::CryptonoteApp;
    use crate::error::AppError;

    fn ctx() -> egui::Context {
        egui::Context::default()
    }

    fn send_pick(app: &mut CryptonoteApp, result: Result<Vec<(String, Vec<u8>)>, AppError>) {
        let (tx, rx) = std::sync::mpsc::channel();
        app.pick_rx = Some(rx);
        app.pick_overlay_open = true;
        assert!(tx.send(result).is_ok());
        app.poll_receivers(&ctx());
    }

    #[test]
    fn pick_completion_adds_attachments_and_toasts() {
        let mut app = CryptonoteApp::default();
        let toasts_before = app.toast.next_id();
        send_pick(
            &mut app,
            Ok(vec![
                ("a.txt".to_owned(), b"hello".to_vec()),
                ("b.bin".to_owned(), vec![0u8, 1u8]),
            ]),
        );
        assert_eq!(app.temporary.attachments.len(), 2);
        assert!(app.pick_rx.is_none(), "slot must clear");
        assert!(!app.pick_overlay_open, "overlay must close");
        assert!(app.temporary.progress.is_none());
        assert_eq!(
            app.toast.next_id(),
            toasts_before + 1,
            "completion must fire exactly one toast"
        );
    }

    #[test]
    fn pick_completion_dedups_by_name() {
        let mut app = CryptonoteApp::default();
        send_pick(&mut app, Ok(vec![("a.txt".to_owned(), b"old".to_vec())]));
        send_pick(&mut app, Ok(vec![("a.txt".to_owned(), b"new".to_vec())]));
        assert_eq!(app.temporary.attachments.len(), 1);
        let data = app.temporary.attachments[0].data.to_vec();
        assert_eq!(data, b"new".to_vec(), "re-pick must refresh bytes");
    }

    #[test]
    fn pick_cancel_toasts_silently() {
        let mut app = CryptonoteApp::default();
        let toasts_before = app.toast.next_id();
        send_pick(&mut app, Err(AppError::Cancelled));
        assert!(app.pick_rx.is_none());
        assert!(!app.pick_overlay_open);
        assert_eq!(
            app.toast.next_id(),
            toasts_before + 1,
            "cancel must fire exactly one toast"
        );
    }

    #[test]
    fn pick_error_toasts() {
        let mut app = CryptonoteApp::default();
        let toasts_before = app.toast.next_id();
        send_pick(&mut app, Err(AppError::NoFileSelected));
        assert!(app.pick_rx.is_none());
        assert!(!app.pick_overlay_open);
        assert_eq!(
            app.toast.next_id(),
            toasts_before + 1,
            "real errors must fire exactly one error toast"
        );
    }
}
