use std::str::FromStr;

use functora_egui::i18n::{I18N, Language};
use functora_egui::route::AppRouter;
use functora_egui::route::RouteMetadata;
use functora_egui::{Button, ButtonVariant, ResponsiveExt, Shell, ToastState, ToastVariant};

use crate::error::AppError;
use crate::hooks::handle_open_url;
use crate::messages::Msg;
use crate::progress::{Job, Stage, clear_progress};
use crate::route::Screen;
use crate::state::{External, TemporaryState};
use crate::storage::{APP_ATTRS, APP_SCOPE, PersistentState};
use functora_egui::messages::Msg as BaseMsg;

const PERSISTENT_KEY: &str = "cryptonote_persistent";
pub(crate) const BYTES_URI_PREFIX: &str = "bytes://";

type PickResult = Result<Vec<(String, Vec<u8>)>, AppError>;
type DecryptResult = Result<(String, Vec<crate::archive::Attachment>), AppError>;
type SharedProgress = std::sync::Arc<std::sync::Mutex<Option<Job<Stage>>>>;
type TrackFn = Box<dyn FnMut(Option<Job<Stage>>) + Send + 'static>;

pub(crate) struct OpStart {
    pub(crate) cancel: functora_egui::CancelToken,
    pub(crate) slot: SharedProgress,
    pub(crate) progress: TrackFn,
}

pub struct CryptonoteApp {
    pub(crate) router: AppRouter<Screen>,
    pub(crate) storage: functora_egui::Storage,
    pub persistent: PersistentState<()>,
    pub temporary: TemporaryState,
    pub toast: ToastState,
    pub(crate) sidebar_collapsed: bool,
    // async receivers
    pub(crate) clipboard_write_rx: Option<std::sync::mpsc::Receiver<Result<(), AppError>>>,
    pub(crate) share_rx: Option<std::sync::mpsc::Receiver<Result<(), AppError>>>,
    pub(crate) download_rx: Option<std::sync::mpsc::Receiver<Result<String, AppError>>>,
    pub pick_rx: Option<std::sync::mpsc::Receiver<PickResult>>,
    pub(crate) progress_cancel: Option<functora_egui::CancelToken>,
    pub progress_overlay_open: bool,
    pub paste_image_rx:
        Option<std::sync::mpsc::Receiver<Result<Option<functora_egui::paste_image::PasteImage>, AppError>>>,
    pub(crate) generate_rx: Option<std::sync::mpsc::Receiver<Result<External, AppError>>>,
    pub(crate) decrypt_rx: Option<std::sync::mpsc::Receiver<DecryptResult>>,
    pub(crate) archive_rx: Option<std::sync::mpsc::Receiver<Result<crate::state::OpenedArchive, AppError>>>,
    pub(crate) pwa_rx: Option<std::sync::mpsc::Receiver<Result<functora_egui::messages::Msg, AppError>>>,
    pub(crate) progress_slot: Option<SharedProgress>,
    pub(crate) qr_state: functora_egui::QrScannerState,
    pub(crate) qr_error_notified: Option<String>,
    pub(crate) md_cache: functora_egui::CommonMarkCache,
    pub(crate) last_deep_link: Option<String>,
}

impl Default for CryptonoteApp {
    fn default() -> Self {
        Self {
            router: AppRouter::new(&Screen::default()),
            storage: functora_egui::Storage::new(APP_SCOPE),
            persistent: PersistentState::default(),
            temporary: TemporaryState::default(),
            toast: ToastState::new(),
            sidebar_collapsed: true,
            clipboard_write_rx: None,
            share_rx: None,
            download_rx: None,
            pick_rx: None,
            progress_cancel: None,
            progress_overlay_open: false,
            paste_image_rx: None,
            generate_rx: None,
            decrypt_rx: None,
            archive_rx: None,
            pwa_rx: None,
            progress_slot: None,
            qr_state: functora_egui::QrScannerState::new(),
            qr_error_notified: None,
            md_cache: functora_egui::CommonMarkCache::default(),
            last_deep_link: None,
        }
    }
}

impl CryptonoteApp {
    #[must_use]
    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        functora_egui::setup_fonts(&cc.egui_ctx);
        functora_egui::setup_image_loaders(&cc.egui_ctx);
        let persistent = PersistentState::load_or_default(&cc.egui_ctx, APP_SCOPE, PERSISTENT_KEY, ());
        functora_egui::theme_extra::set_theme(&cc.egui_ctx, persistent.theme);
        let mut this = Self {
            persistent,
            ..Default::default()
        };
        this.sidebar_collapsed = functora_egui::initial_sidebar_collapsed(&cc.egui_ctx);
        #[cfg(target_arch = "wasm32")]
        {
            functora_egui::platform::web::install_paste_image_listener();
            let router = AppRouter::new(&Screen::default());
            let current = *router.current();
            this.router = router;
            this.temporary.screen = current;
        }
        #[cfg(not(any(target_arch = "wasm32", target_os = "android")))]
        crate::deep_link::ingest_desktop_args();
        this
    }

    pub(crate) fn lang(&self) -> Language {
        self.persistent.language
    }

    pub fn set_language(&mut self, lang: Language) {
        if self.persistent.language != lang {
            self.persistent.language = lang;
            self.toast = ToastState::new();
            self.qr_error_notified = None;
        }
    }

    pub(crate) fn navigate(&mut self, screen: Screen) {
        self.temporary.screen = screen;
        self.router.navigate(screen);
    }

    pub(crate) fn go_back(&mut self) {
        let _ = self.router.go_back();
        self.temporary.screen = *self.router.current();
    }

    fn apply_theme(&self, ctx: &egui::Context) {
        functora_egui::theme_extra::set_theme(ctx, self.persistent.theme);
    }

    pub fn reset(&mut self, now: f64) {
        self.cancel_all();
        self.temporary.reset();
        self.toast = ToastState::new();
        let lang = self.persistent.language;
        self.toast.add(Msg::ResetDone.render(lang), ToastVariant::Success, now);
        self.qr_error_notified = None;
        self.qr_state = functora_egui::QrScannerState::new();
        self.md_cache = functora_egui::CommonMarkCache::default();
        self.last_deep_link = None;
        let _ = crate::deep_link::take_url();
        if crate::deep_link::has_pending_archive() {
            let _ = crate::deep_link::take_archive();
        }
        self.router.reset(Screen::Home);
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
        if resp.pasted {
            self.toast.add(BaseMsg::Pasted.render(lang), ToastVariant::Success, now);
        }
        if resp.copied {
            self.toast.add(BaseMsg::Copied.render(lang), ToastVariant::Success, now);
        }
        if let Some(clipboard_error) = resp.clipboard_error {
            let app_error = AppError::from(clipboard_error);
            if !Self::is_cancelled(&app_error) {
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

    fn toast_error(&mut self, error: AppError, lang: Language, time: f64) {
        if !Self::is_cancelled(&error) {
            self.toast.add(
                Msg::Error(crate::error::MsgError::from(error)).render(lang),
                ToastVariant::Error,
                time,
            );
        }
    }

    pub(crate) fn begin_op(&mut self, stage: Stage) -> Option<OpStart> {
        if self.temporary.progress.is_some() {
            return None;
        }
        self.temporary.progress = Some(Job {
            stage,
            done: 0,
            total: 1,
            name: None,
        });
        let cancel = functora_egui::new_cancel_token();
        self.progress_cancel = Some(cancel.clone());
        self.progress_overlay_open = true;
        let (slot, progress) = self.track_progress();
        Some(OpStart { cancel, slot, progress })
    }

    #[doc(hidden)]
    pub fn begin_attach_for_tests(&mut self) -> bool {
        self.begin_op(Stage::Attach).is_some()
    }

    pub(crate) fn end_op(&mut self) {
        clear_progress(&mut self.temporary.progress);
        self.progress_cancel = None;
        self.progress_overlay_open = false;
        self.progress_slot = None;
    }

    /// Opens the blocking progress overlay for the given stage without
    /// starting any work. Test support only, so UI tests can render the
    /// overlay deterministically without constructing a valid archive.
    #[doc(hidden)]
    pub fn open_progress_overlay_for_tests(&mut self, stage: Stage) {
        _ = self.begin_op(stage);
    }

    pub(crate) fn track_progress(&mut self) -> (SharedProgress, TrackFn) {
        let slot: SharedProgress = std::sync::Arc::new(std::sync::Mutex::new(None));
        self.progress_slot = Some(std::sync::Arc::clone(&slot));
        let writer = std::sync::Arc::clone(&slot);
        let track: TrackFn = Box::new(move |job| {
            *writer.lock().unwrap_or_else(std::sync::PoisonError::into_inner) = job;
        });
        (slot, track)
    }

    fn sync_progress(&mut self) {
        let reported = self
            .progress_slot
            .as_ref()
            .and_then(|slot| slot.lock().unwrap_or_else(std::sync::PoisonError::into_inner).clone());
        if let Some(job) = reported {
            self.temporary.progress = Some(job);
        }
    }

    fn cancel_all(&mut self) {
        self.clipboard_write_rx = None;
        self.share_rx = None;
        self.download_rx = None;
        self.pick_rx = None;
        self.paste_image_rx = None;
        self.generate_rx = None;
        self.decrypt_rx = None;
        self.archive_rx = None;
        self.pwa_rx = None;
        if let Some(token) = self.progress_cancel.take() {
            functora_egui::files::cancel(&token);
        }
        self.end_op();
    }

    pub fn handle_paste_image(&mut self, ctx: &egui::Context) {
        if let Some(image) = functora_egui::paste_image::take() {
            let time = ctx.input(|i| i.time);
            self.attach_pasted_image(image, time);
        }
        #[cfg(not(any(target_arch = "wasm32", target_os = "android")))]
        {
            use crate::state::ActionMode;
            if self.paste_image_rx.is_none()
                && self.temporary.screen == Screen::Home
                && self.temporary.action == ActionMode::Create
                && self.temporary.progress.is_none()
                && functora_egui::paste_image::is_paste_image_intent(&ctx.input(|i| i.events.clone()))
            {
                self.paste_image_rx = Some(functora_egui::spawn_async(async move {
                    Ok(functora_egui::clipboard::read_image().await?)
                }));
            }
        }
    }

    fn attach_pasted_image(&mut self, image: functora_egui::paste_image::PasteImage, time: f64) {
        let attachment = functora_egui::files::Attachment {
            name: image.name,
            data: image.data.into(),
        };
        crate::hooks::add_attachment(&mut self.temporary.attachments, attachment);
        self.toast.add(
            BaseMsg::FilesAttached(1).render(self.lang()),
            ToastVariant::Success,
            time,
        );
    }

    pub fn poll_receivers(&mut self, ctx: &egui::Context) {
        self.sync_progress();
        let lang = self.lang();
        let time = ctx.input(|i| i.time);

        match functora_egui::poll_channel(&mut self.clipboard_write_rx) {
            None | Some(functora_egui::PollOutcome::Disconnected) => {}
            Some(functora_egui::PollOutcome::Ready(Ok(()))) => {
                self.toast
                    .add(BaseMsg::Copied.render(lang), ToastVariant::Success, time)
            }
            Some(functora_egui::PollOutcome::Ready(Err(e))) => self.toast_error(e, lang, time),
        }
        match functora_egui::poll_channel(&mut self.share_rx) {
            None | Some(functora_egui::PollOutcome::Disconnected) => {}
            Some(functora_egui::PollOutcome::Ready(Ok(()))) => {
                self.toast.add(Msg::Shared.render(lang), ToastVariant::Success, time);
            }
            Some(functora_egui::PollOutcome::Ready(Err(e))) => self.toast_error(e, lang, time),
        }
        if let Some(outcome) = functora_egui::poll_channel(&mut self.download_rx) {
            self.end_op();
            match outcome {
                functora_egui::PollOutcome::Disconnected => {}
                functora_egui::PollOutcome::Ready(Ok(name)) => {
                    self.toast
                        .add(Msg::Saved(name).render(lang), ToastVariant::Success, time);
                }
                functora_egui::PollOutcome::Ready(Err(e)) => self.toast_error(e, lang, time),
            }
        }
        if let Some(outcome) = functora_egui::poll_channel(&mut self.pick_rx) {
            self.end_op();
            match outcome {
                functora_egui::PollOutcome::Disconnected => {}
                functora_egui::PollOutcome::Ready(Ok(files)) => {
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
                }
                functora_egui::PollOutcome::Ready(Err(e)) => {
                    if Self::is_cancelled(&e) {
                        self.toast.add(e.render(lang), ToastVariant::Default, time);
                    } else {
                        self.toast.add(
                            Msg::Error(crate::error::MsgError::from(e)).render(lang),
                            ToastVariant::Error,
                            time,
                        );
                    }
                }
            }
        }
        match functora_egui::poll_channel(&mut self.paste_image_rx) {
            Some(functora_egui::PollOutcome::Ready(Ok(Some(image)))) => {
                self.attach_pasted_image(image, time);
            }
            None | Some(functora_egui::PollOutcome::Disconnected | functora_egui::PollOutcome::Ready(Ok(None))) => {}
            Some(functora_egui::PollOutcome::Ready(Err(e))) => {
                self.toast.add(
                    Msg::Error(crate::error::MsgError::from(e)).render(lang),
                    ToastVariant::Error,
                    time,
                );
            }
        }
        if let Some(outcome) = functora_egui::poll_channel(&mut self.generate_rx) {
            self.end_op();
            match outcome {
                functora_egui::PollOutcome::Disconnected => {}
                functora_egui::PollOutcome::Ready(Ok(external)) => {
                    self.temporary.external = external;
                    self.navigate(Screen::Share);
                }
                functora_egui::PollOutcome::Ready(Err(e)) => self.toast_error(e, lang, time),
            }
        }
        if let Some(outcome) = functora_egui::poll_channel(&mut self.decrypt_rx) {
            self.end_op();
            match outcome {
                functora_egui::PollOutcome::Disconnected => {}
                functora_egui::PollOutcome::Ready(Ok((text, files))) => {
                    self.temporary.note = text;
                    self.temporary.attachments = files;
                    self.temporary.external = External::Nothing;
                    self.navigate(Screen::View);
                }
                functora_egui::PollOutcome::Ready(Err(e)) => self.toast_error(e, lang, time),
            }
        }
        if let Some(outcome) = functora_egui::poll_channel(&mut self.archive_rx) {
            self.end_op();
            match outcome {
                functora_egui::PollOutcome::Disconnected => {}
                functora_egui::PollOutcome::Ready(Ok(opened)) => {
                    self.temporary.external = opened.external;
                    self.temporary.note = opened.note;
                    self.temporary.attachments = opened.attachments;
                    if opened.screen == Screen::Open {
                        self.temporary.password.clear();
                    }
                    self.navigate(opened.screen);
                }
                functora_egui::PollOutcome::Ready(Err(e)) => self.toast_error(e, lang, time),
            }
        }
        match functora_egui::poll_channel(&mut self.pwa_rx) {
            None | Some(functora_egui::PollOutcome::Disconnected) => {}
            Some(functora_egui::PollOutcome::Ready(Ok(msg))) => {
                let variant = if matches!(msg, BaseMsg::PwaInstallSuccess) {
                    ToastVariant::Success
                } else {
                    ToastVariant::Default
                };
                self.toast.add(Msg::Base(msg).render(lang), variant, time);
            }
            Some(functora_egui::PollOutcome::Ready(Err(e))) => self.toast_error(e, lang, time),
        }
        if self.has_pending_receivers() {
            ctx.request_repaint();
        } else {
            self.progress_slot = None;
        }
    }

    #[must_use]
    pub fn has_pending_receivers(&self) -> bool {
        self.clipboard_write_rx.is_some()
            || self.share_rx.is_some()
            || self.download_rx.is_some()
            || self.pick_rx.is_some()
            || self.paste_image_rx.is_some()
            || self.generate_rx.is_some()
            || self.decrypt_rx.is_some()
            || self.archive_rx.is_some()
            || self.pwa_rx.is_some()
    }

    fn handle_deep_link(&mut self, ctx: &egui::Context) {
        #[cfg(target_arch = "wasm32")]
        let pending_url: Option<String> = functora_egui::deep_link::poll_deep_link_web();
        #[cfg(not(target_arch = "wasm32"))]
        let pending_url: Option<String> = functora_egui::deep_link::poll_deep_link();
        if let Some(url) = pending_url
            && self.last_deep_link.as_deref() != Some(url.as_str())
        {
            self.last_deep_link = Some(url.clone());
            match handle_open_url(&url, &mut self.temporary) {
                Ok(screen) => self.navigate(screen),
                Err(AppError::NoNoteParam) => {}
                Err(e) => {
                    let lang = self.lang();
                    let time = ctx.input(|i| i.time);
                    self.toast.add(
                        Msg::Error(crate::error::MsgError::from(e)).render(lang),
                        ToastVariant::Error,
                        time,
                    );
                }
            }
        }
        if crate::deep_link::has_pending_archive()
            && let Some(source) = crate::deep_link::take_archive()
            && let Some(OpStart { cancel, progress, .. }) = self.begin_op(Stage::Preview)
        {
            self.archive_rx = Some(functora_egui::spawn_async(async move {
                crate::hooks::load_archive_async(source, cancel, progress).await
            }));
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
            .action(BaseMsg::FooterShareWord.render(lang), Screen::About.to_string())
            .text(format!(
                " {} {} ",
                BaseMsg::FooterAppWord.render(lang),
                BaseMsg::And.render(lang)
            ))
            .action(BaseMsg::DonateLink.render(lang), Screen::Donate.to_string())
            .text(format!(". {} {}.", BaseMsg::VersionLabel.render(lang), APP_ATTRS.vsn))
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

    /// Unified long-operation overlay. Every cancellable op (pick, archive
    /// load, encrypt, decrypt, zip, download) renders the same modal
    /// `BlockingOverlay` the file picker uses, so long operations share one
    /// consistent, blocking, stage-labelled UI instead of a mix of inline bars
    /// and a bottom card.
    pub(crate) fn show_progress_overlay(&mut self, ctx: &egui::Context) {
        if !self.progress_overlay_open {
            return;
        }
        if let Some(cancel) = self.progress_cancel.clone() {
            let mut open = self.progress_overlay_open;
            let title = self.temporary.progress.as_ref().map_or_else(
                || BaseMsg::Preparing.render(self.lang()),
                |job| BaseMsg::Stage(job.stage).render(self.lang()),
            );
            functora_egui::BlockingOverlay::new(title).show(
                ctx,
                &mut open,
                self.temporary.progress.as_ref(),
                &cancel,
                self.lang(),
            );
            self.progress_overlay_open = open;
        }
    }
}

impl eframe::App for CryptonoteApp {
    fn ui(&mut self, ui: &mut egui::Ui, _frame: &mut eframe::Frame) {
        let ctx = ui.ctx().clone();
        self.apply_theme(&ctx);
        self.poll_receivers(&ctx);
        self.handle_deep_link(&ctx);
        self.handle_paste_image(&ctx);
        self.show_progress_overlay(&ctx);
        self.router.ui(ui);
        #[cfg(target_os = "android")]
        functora_egui::android::poll_ime(&ctx);
        let routed = *self.router.current();
        if routed != self.temporary.screen {
            self.temporary.screen = routed;
        }
        let prev_persistent = self.persistent.clone();
        let mut theme = self.persistent.theme;
        let mut collapsed_val = self.sidebar_collapsed;
        let route = *self.router.current();
        let history = self.router.history().clone();
        let needs_reset = std::cell::Cell::new(false);
        let pending_nav = std::cell::Cell::new(None::<Screen>);
        let pending_reset = std::cell::Cell::new(false);
        let selected_snapshot = self.temporary.screen;
        let lang_cell = std::cell::Cell::new(self.persistent.language);
        let (nav_items, sidebar_names) = Self::sidebar_entries(lang_cell.get());
        let breadcrumb_action = Shell::new("Cryptonote", &mut collapsed_val, {
            let pending_ref = &pending_nav;
            let reset_ref = &pending_reset;
            let lang_ref = &lang_cell;
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
                if side_ui
                    .add(
                        Button::new(Msg::Reset.render(lang_ref.get()))
                            .icon(functora_egui::LucideIcon::RotateCcw)
                            .variant(ButtonVariant::Ghost)
                            .full_width(),
                    )
                    .clicked()
                {
                    reset_ref.set(true);
                    close |= side_ui.on_mobile();
                    side_ui.ctx().request_repaint();
                }
                close
            }
        })
        .brand_icon(None)
        .theme(&mut theme)
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
        self.sidebar_collapsed = collapsed_val;
        if theme != prev_persistent.theme {
            self.persistent.theme = theme;
        }
        self.set_language(lang_cell.get());
        if let Some(screen) = pending_nav.get() {
            self.navigate(screen);
            ctx.request_repaint();
        }
        if needs_reset.get() || pending_reset.get() {
            let now = ctx.input(|i| i.time);
            self.reset(now);
        }
        if prev_persistent != self.persistent {
            self.storage.persist(PERSISTENT_KEY, &self.persistent);
            self.apply_theme(&ctx);
        }
        if let Some(action) = breadcrumb_action {
            match action {
                functora_egui::NavAction::Back => self.go_back(),
                functora_egui::NavAction::Forward => {
                    let _ = self.router.go_forward();
                    self.temporary.screen = *self.router.current();
                }
                functora_egui::NavAction::Route(r) => {
                    self.navigate(r);
                }
            }
        }
        self.toast.show(&ctx);
    }
}

impl CryptonoteApp {
    /// Sidebar entries for `lang`: the navigable screens with their labels
    /// and icons, plus the label list the shell sizes itself against.
    fn sidebar_entries(
        lang: functora_egui::i18n::Language,
    ) -> (Vec<(Screen, String, functora_egui::LucideIcon)>, Vec<String>) {
        let nav_items: Vec<(Screen, String, functora_egui::LucideIcon)> = [
            (Screen::Home, functora_egui::LucideIcon::House),
            (Screen::About, functora_egui::LucideIcon::Info),
            (Screen::Donate, functora_egui::LucideIcon::Heart),
        ]
        .iter()
        .map(|(screen, icon)| (*screen, screen.label(lang).into_owned(), *icon))
        .collect();
        let mut sidebar_names: Vec<String> = nav_items.iter().map(|(_, label, _)| label.clone()).collect();
        sidebar_names.push(Msg::Reset.render(lang));
        (nav_items, sidebar_names)
    }
}
