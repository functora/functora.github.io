use crate::crypto::CipherType;
use crate::deep_link::initial_url;
use crate::encoding::{decode_note, extract_note_param, NoteData};
use crate::error::AppError;
use crate::i18n::{detect_browser_language, Language, I18N, SUPPORTED_LANGUAGES};
use crate::messages::Msg;
use crate::progress::Job;
use crate::screens::Screen;
use crate::state::{ActionMode, External, ExternalNote, PasteTarget, PickKind};
use crate::task::{build_external, decrypt_external, extract_archive, Event};
use crate::theme::Theme;
use elegance::glyphs;
use elegance::{
    Accent, BadgeTone, Button, ButtonSize, Drawer, DrawerSide, Menu, MenuItem, ProgressBar, TabBar,
    Toast, Toasts,
};
use functora_core::encoding::extract_query_param;
use functora_core::files::Attachment;
use functora_core::messages::Msg as BaseMsg;
use functora_core::white_label::AppAttrs;
use functora_tagged::InfallibleInto;
use std::sync::mpsc::{channel, Receiver, Sender};
use std::sync::Arc;

pub const APP_ATTRS: AppAttrs = AppAttrs {
    app: env!("CARGO_PKG_NAME"),
    vsn: env!("CARGO_PKG_VERSION"),
    org: "functora",
    src: Some("rust"),
    dst: "apps",
    description: "Cryptonote is a cross-platform, serverless app for encrypted offline notes.",
};

const MOBILE_BREAKPOINT: f32 = 800.0;
const CONTENT_MAX_WIDTH: f32 = 960.0;
const MAIN_SCREENS: [Screen; 6] = [
    Screen::Home,
    Screen::Open,
    Screen::View,
    Screen::Share,
    Screen::File,
    Screen::About,
];

#[must_use]
pub fn share_error(cipher: Option<CipherType>, password: &str) -> Option<Msg> {
    (cipher.is_some() && password.is_empty()).then_some(Msg::Base(BaseMsg::PasswordRequired))
}

pub struct CryptonoteApp {
    pub(crate) language: Language,
    pub(crate) theme: Theme,
    pub(crate) screen: Screen,
    pub(crate) history: Vec<Screen>,
    pub(crate) note: String,
    pub(crate) password: String,
    pub(crate) cipher: Option<CipherType>,
    pub(crate) attachments: Vec<Attachment>,
    pub(crate) action: ActionMode,
    pub(crate) url_input: String,
    pub(crate) external: External,
    pub(crate) pending_note: Option<String>,
    pub(crate) message: Option<Msg>,
    pub(crate) job: Option<Job>,
    pub(crate) paste_target: Option<PasteTarget>,
    pub(crate) pick_kind: Option<PickKind>,
    pub(crate) pending_cipher: Option<CipherType>,
    pub(crate) qr_texture: Option<egui::TextureHandle>,
    pub(crate) tx: Sender<Event>,
    pub(crate) rx: Receiver<Event>,
    pub(crate) ctx: egui::Context,
    pub(crate) nav_open: bool,
    pub(crate) nav_tab: usize,
    pub(crate) toasted: bool,
}

impl CryptonoteApp {
    #[must_use]
    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        let detected = detect_browser_language();
        let language = if SUPPORTED_LANGUAGES.contains(&detected) {
            detected
        } else {
            Language::Eng
        };
        let theme = Theme::Dark;
        theme.apply(&cc.egui_ctx);
        let (tx, rx) = channel();
        let (screen, note, external) = Self::initial_route();
        Self {
            language,
            theme,
            screen,
            history: Vec::new(),
            note,
            password: String::new(),
            cipher: Some(CipherType::Aes256Gcm),
            attachments: Vec::new(),
            action: ActionMode::default(),
            url_input: String::new(),
            external,
            pending_note: None,
            message: None,
            job: None,
            paste_target: None,
            pick_kind: None,
            pending_cipher: None,
            qr_texture: None,
            tx,
            rx,
            ctx: cc.egui_ctx.clone(),
            nav_open: false,
            nav_tab: 0,
            toasted: false,
        }
    }

    fn initial_route() -> (Screen, String, External) {
        let Some(url) = initial_url() else {
            return (Screen::default(), String::new(), External::default());
        };
        let screen = extract_query_param(&url, "screen")
            .as_deref()
            .map_or_else(Screen::default, |s| s.parse::<Screen>().unwrap_or_default());
        let Some(encoded) = extract_query_param(&url, "note") else {
            return (screen, String::new(), External::default());
        };
        match decode_note(&encoded) {
            Ok(NoteData::CipherText(enc)) => (
                Screen::Open,
                String::new(),
                External::Note(ExternalNote {
                    data: NoteData::CipherText(enc),
                    url: String::new(),
                    qr: String::new(),
                }),
            ),
            Ok(NoteData::PlainText(text)) => (Screen::Open, text, External::default()),
            Err(e) => {
                log::warn!("Failed to decode note from URL: {e}");
                (screen, String::new(), External::default())
            }
        }
    }

    #[must_use]
    pub fn text(&self, msg: &Msg) -> String {
        msg.render(self.language)
    }

    pub(crate) fn busy(&self) -> bool {
        self.job.is_some()
    }

    pub(crate) fn navigate(&mut self, screen: Screen) {
        if self.screen == screen {
            return;
        }
        self.history.push(self.screen);
        self.screen = screen;
    }

    pub(crate) fn back(&mut self) {
        if let Some(prev) = self.history.pop() {
            self.screen = prev;
        } else {
            self.screen = Screen::default();
        }
    }

    pub(crate) fn reset(&mut self) {
        self.note.clear();
        self.password.clear();
        self.cipher = Some(CipherType::Aes256Gcm);
        self.attachments.clear();
        self.action = ActionMode::default();
        self.url_input.clear();
        self.external = External::default();
        self.pending_note = None;
        self.message = None;
        self.job = None;
        self.paste_target = None;
        self.pick_kind = None;
        self.pending_cipher = None;
        self.qr_texture = None;
        self.history.clear();
        self.screen = Screen::default();
        self.nav_tab = 0;
        self.toasted = false;
    }

    pub(crate) fn generate_share(&mut self) {
        self.message = None;
        if let Some(msg) = share_error(self.cipher, &self.password) {
            self.message = Some(msg);
            return;
        }
        if self.busy() {
            return;
        }
        self.job = Some(Job {
            stage: crate::progress::Stage::Encrypt,
            done: 0,
            total: 1,
            name: None,
        });
        build_external(
            self.note.clone(),
            self.password.clone(),
            self.cipher,
            self.attachments.clone(),
            self.tx.clone(),
            self.ctx.clone(),
            APP_ATTRS.origin(),
        );
    }

    pub(crate) fn decrypt_note(&mut self) {
        self.message = None;
        if self.password.is_empty() {
            self.message = Some(Msg::Base(BaseMsg::PasswordRequired));
            return;
        }
        if self.busy() {
            return;
        }
        self.job = Some(Job {
            stage: crate::progress::Stage::Decrypt,
            done: 0,
            total: 1,
            name: None,
        });
        let pwd = self.password.clone();
        match self.external.clone() {
            External::Note(n) => {
                if let NoteData::CipherText(enc) = n.data {
                    self.pending_cipher = Some(enc.cipher);
                    decrypt_external(enc, pwd, self.tx.clone(), self.ctx.clone());
                }
            }
            External::Archive(a) => {
                self.pending_cipher = None;
                extract_archive(a.untag(), pwd, self.tx.clone(), self.ctx.clone());
            }
            External::Nothing => {}
        }
    }

    pub(crate) fn open_url(&mut self) {
        self.message = None;
        let url = self.url_input.trim().to_string();
        if url.is_empty() {
            self.message = Some(Msg::Error(AppError::NoNoteInUrl.into()));
            return;
        }
        match extract_note_param(&url) {
            Ok(note) => {
                self.pending_note = Some(note);
                self.navigate(Screen::Open);
            }
            Err(e) => self.message = Some(Msg::Error(e.into())),
        }
    }

    pub(crate) fn paste(&mut self, target: PasteTarget) {
        self.message = None;
        self.paste_target = Some(target);
        let tx = self.tx.clone();
        let ctx = self.ctx.clone();
        crate::task::spawn_async(async move {
            let result = crate::platform::read_clipboard()
                .await
                .map_err(AppError::Platform);
            crate::task::send(&tx, &ctx, Event::Clipboard(result));
        });
    }

    pub(crate) fn copy_text(&mut self, text: String) {
        self.message = None;
        let tx = self.tx.clone();
        let ctx = self.ctx.clone();
        crate::task::spawn_async(async move {
            let result = crate::platform::write_clipboard(text)
                .await
                .map_err(AppError::Platform);
            let msg = match result {
                Ok(()) => Msg::Base(BaseMsg::Copied),
                Err(e) => Msg::Base(BaseMsg::ClipboardWriteError(e.to_string())),
            };
            crate::task::send(&tx, &ctx, Event::Message(msg));
        });
    }

    pub(crate) fn pick_files(&mut self, kind: PickKind) {
        self.message = None;
        if self.busy() {
            return;
        }
        self.job = Some(Job {
            stage: crate::progress::Stage::Attach,
            done: 0,
            total: 1,
            name: None,
        });
        self.pick_kind = Some(kind);
        let multiple = matches!(kind, PickKind::Attach);
        let tx = self.tx.clone();
        let ctx = self.ctx.clone();
        crate::task::spawn_async(async move {
            let result = crate::platform::pick_files(multiple)
                .await
                .map_err(AppError::Platform);
            crate::task::send(&tx, &ctx, Event::Picked(result));
        });
    }

    pub(crate) fn scan_image(&mut self) {
        self.pick_files(PickKind::Scan);
    }

    pub(crate) fn download(&mut self, filename: String, bytes: Vec<u8>) {
        self.message = None;
        if self.busy() {
            return;
        }
        self.job = Some(Job {
            stage: crate::progress::Stage::Download,
            done: 0,
            total: 1,
            name: None,
        });
        let tx = self.tx.clone();
        let ctx = self.ctx.clone();
        crate::task::spawn_async(async move {
            let result = crate::platform::save_bytes(&filename, bytes)
                .await
                .map_err(AppError::Platform);
            crate::task::send(&tx, &ctx, Event::Downloaded(result));
        });
    }

    pub(crate) fn social_share(&mut self, text: String, url: String) {
        self.message = None;
        let tx = self.tx.clone();
        let ctx = self.ctx.clone();
        crate::task::spawn_async(async move {
            let result = crate::platform::social_share(text, url)
                .await
                .map_err(AppError::Platform);
            let msg = match result {
                Ok(()) => Msg::Sent,
                Err(e) => Msg::Error(e.into()),
            };
            crate::task::send(&tx, &ctx, Event::Message(msg));
        });
    }

    pub(crate) fn print(&mut self) {
        self.message = None;
        let tx = self.tx.clone();
        let ctx = self.ctx.clone();
        crate::task::spawn_async(async move {
            let result = crate::platform::print_page()
                .await
                .map_err(AppError::Platform);
            let msg = match result {
                Ok(()) => Msg::Sent,
                Err(e) => Msg::Error(e.into()),
            };
            crate::task::send(&tx, &ctx, Event::Message(msg));
        });
    }

    pub(crate) fn open_url_from_scan(&mut self, url: &str) {
        self.message = None;
        match extract_note_param(url) {
            Ok(note) => {
                self.pending_note = Some(note);
                self.navigate(Screen::Open);
            }
            Err(e) => self.message = Some(Msg::Error(e.into())),
        }
    }

    pub(crate) fn handle_initial_note(&mut self) {
        let Some(encoded) = self.pending_note.take() else {
            return;
        };
        match decode_note(&encoded) {
            Ok(NoteData::CipherText(enc)) => {
                self.external = External::Note(ExternalNote {
                    data: NoteData::CipherText(enc),
                    url: String::new(),
                    qr: String::new(),
                });
            }
            Ok(NoteData::PlainText(text)) => {
                self.note = text;
                self.cipher = None;
                self.external = External::default();
            }
            Err(e) => self.message = Some(Msg::Error(e.into())),
        }
    }

    pub(crate) fn handle_event(&mut self, event: Event) {
        match event {
            Event::Job(job) => self.job = job,
            Event::Message(msg) => self.message = Some(msg),
            Event::ExternalReady(result) => {
                self.job = None;
                self.pending_cipher = None;
                self.qr_texture = None;
                match result {
                    Ok(external) => {
                        self.external = external;
                        self.navigate(Screen::Share);
                    }
                    Err(e) => self.message = Some(Msg::Error(e)),
                }
            }
            Event::Opened(result) => {
                self.job = None;
                match result {
                    Ok((text, files)) => {
                        self.note = text;
                        self.attachments = files;
                        self.external = External::default();
                        if let Some(cipher) = self.pending_cipher.take() {
                            self.cipher = Some(cipher);
                        }
                        self.navigate(Screen::View);
                    }
                    Err(e) => {
                        self.pending_cipher = None;
                        self.message = Some(Msg::Error(e));
                    }
                }
            }
            Event::Clipboard(result) => match result {
                Ok(text) => match self.paste_target.take() {
                    Some(PasteTarget::Note) => self.note = text,
                    Some(PasteTarget::Url) => self.url_input = text,
                    Some(PasteTarget::Password) => self.password = text,
                    None => {}
                },
                Err(e) => {
                    self.paste_target = None;
                    self.message = Some(Msg::Error(e.into()));
                }
            },
            Event::Picked(result) => match result {
                Ok(files) => self.handle_picked(files),
                Err(e) => {
                    self.pick_kind = None;
                    self.job = None;
                    self.message = Some(Msg::Error(e.into()));
                }
            },
            Event::Scanned(result) => {
                self.job = None;
                match result {
                    Ok(url) => self.open_url_from_scan(&url),
                    Err(e) => self.message = Some(Msg::Error(e.into())),
                }
            }
            Event::Downloaded(result) => {
                self.job = None;
                match result {
                    Ok(Some(loc)) => self.message = Some(Msg::Downloaded(loc)),
                    Ok(None) => {}
                    Err(e) => self.message = Some(Msg::Error(e.into())),
                }
            }
        }
        self.ctx.request_repaint();
    }

    fn handle_picked(&mut self, files: Vec<(String, Vec<u8>)>) {
        self.job = None;
        match self.pick_kind.take() {
            Some(PickKind::Attach) => {
                for (name, data) in files {
                    let att = Attachment {
                        name,
                        data: Arc::from(data),
                    };
                    self.attachments.retain(|f| f.name != att.name);
                    self.attachments.push(att);
                }
            }
            Some(PickKind::OpenArchive) => {
                let Some((_, bytes)) = files.into_iter().next() else {
                    return;
                };
                let source = functora_core::package::ArchiveSource::Bytes(bytes.clone());
                match crate::archive::read_archive_metadata(&source) {
                    Ok(meta) => {
                        if meta.cipher.is_some() {
                            self.external =
                                External::Archive(functora_tagged::Tagged::new(bytes).infallible());
                            self.password.clear();
                            self.navigate(Screen::Open);
                        } else {
                            self.pending_cipher = None;
                            self.job = Some(Job {
                                stage: crate::progress::Stage::Decrypt,
                                done: 0,
                                total: 1,
                                name: None,
                            });
                            extract_archive(
                                bytes,
                                String::new(),
                                self.tx.clone(),
                                self.ctx.clone(),
                            );
                        }
                    }
                    Err(e) => self.message = Some(Msg::Error(e.into())),
                }
            }
            Some(PickKind::Scan) => {
                let Some((_, bytes)) = files.into_iter().next() else {
                    return;
                };
                self.job = Some(Job {
                    stage: crate::progress::Stage::Preview,
                    done: 0,
                    total: 1,
                    name: None,
                });
                let tx = self.tx.clone();
                let ctx = self.ctx.clone();
                crate::task::spawn_async(async move {
                    let result =
                        crate::platform::decode_qr_image(&bytes).map_err(AppError::Platform);
                    crate::task::send(&tx, &ctx, Event::Scanned(result));
                });
            }
            None => {}
        }
    }

    pub(crate) fn drain_events(&mut self) {
        while let Ok(event) = self.rx.try_recv() {
            self.handle_event(event);
        }
    }

    pub(crate) fn render_screen(&mut self, ui: &mut egui::Ui) {
        match self.screen {
            Screen::Home => self.render_home(ui),
            Screen::Open => {
                self.handle_initial_note();
                self.render_open(ui);
            }
            Screen::View => self.render_view(ui),
            Screen::Share => self.render_share(ui),
            Screen::File => self.render_file(ui),
            Screen::About => self.render_about(ui),
            Screen::Donate => self.render_donate(ui),
            Screen::License => self.render_license(ui),
            Screen::Privacy => self.render_privacy(ui),
        }
    }

    fn render_nav(&mut self, ui: &mut egui::Ui) {
        if self.is_mobile() {
            self.render_nav_mobile(ui);
        } else {
            self.render_nav_desktop(ui);
        }
    }

    fn render_brand(&mut self, ui: &mut egui::Ui) {
        let brand = ui.add(
            egui::Label::new(egui::RichText::new(APP_ATTRS.app_name()).strong())
                .sense(egui::Sense::click()),
        );
        if brand
            .on_hover_text(self.text(&Msg::Base(BaseMsg::Home)))
            .clicked()
        {
            self.reset();
        }
    }

    fn render_nav_desktop(&mut self, ui: &mut egui::Ui) {
        let _row = ui.horizontal(|row| {
            if !self.history.is_empty() {
                if row
                    .add(
                        Button::new(egui::RichText::new(glyphs::ARROW_LEFT))
                            .outline()
                            .size(ButtonSize::Small),
                    )
                    .on_hover_text(self.text(&Msg::Base(BaseMsg::Back)))
                    .clicked()
                {
                    self.back();
                }
                _ = row.separator();
            }
            self.render_brand(row);
            row.add_space(16.0);
            let tabs: Vec<String> = MAIN_SCREENS
                .iter()
                .map(|screen| self.screen_title(*screen))
                .collect();
            if let Some(index) = self.tab_index() {
                self.nav_tab = index;
            }
            if row.add(TabBar::new(&mut self.nav_tab, tabs)).changed() {
                self.navigate(MAIN_SCREENS[self.nav_tab]);
            }
            _ = row.with_layout(egui::Layout::right_to_left(egui::Align::Center), |right| {
                self.render_nav_right(right);
            });
        });
    }

    fn render_nav_mobile(&mut self, ui: &mut egui::Ui) {
        let _row = ui.horizontal(|row| {
            if !self.history.is_empty()
                && row
                    .add(
                        Button::new(egui::RichText::new(glyphs::ARROW_LEFT))
                            .outline()
                            .size(ButtonSize::Small),
                    )
                    .on_hover_text(self.text(&Msg::Base(BaseMsg::Back)))
                    .clicked()
            {
                self.back();
            }
            self.render_brand(row);
            _ = row.with_layout(egui::Layout::right_to_left(egui::Align::Center), |right| {
                if right
                    .add(
                        Button::new(egui::RichText::new(glyphs::MENU))
                            .outline()
                            .size(ButtonSize::Small),
                    )
                    .clicked()
                {
                    self.nav_open = true;
                }
            });
        });
    }

    fn render_nav_right(&mut self, ui: &mut egui::Ui) {
        let theme_label = if self.theme == Theme::Dark {
            "🌙"
        } else {
            "☀️"
        };
        if ui
            .add(Button::new(theme_label).outline().size(ButtonSize::Small))
            .on_hover_text(self.text(&Msg::Base(BaseMsg::Theme)))
            .clicked()
        {
            self.theme = self.theme.toggle();
            self.theme.apply(ui.ctx());
        }
        let trigger = ui.add(
            Button::new(self.language_label(self.language))
                .outline()
                .size(ButtonSize::Small),
        );
        let _ = Menu::new("language_menu").show_below(&trigger, |menu| {
            for language in SUPPORTED_LANGUAGES.iter().copied() {
                if menu
                    .add(
                        MenuItem::new(self.language_label(language))
                            .radio(language == self.language),
                    )
                    .clicked()
                {
                    self.language = language;
                }
            }
        });
    }

    fn render_drawer(&mut self, ui: &mut egui::Ui) -> bool {
        ui.add_space(8.0);
        for screen in MAIN_SCREENS {
            if ui
                .add(
                    Button::new(self.screen_title(screen))
                        .outline()
                        .full_width(),
                )
                .clicked()
            {
                self.navigate(screen);
                return true;
            }
        }
        _ = ui.separator();
        for language in SUPPORTED_LANGUAGES.iter().copied() {
            let label = self.language_label(language);
            let text = if language == self.language {
                format!("{} {}", glyphs::CHECK, label)
            } else {
                label
            };
            if ui.add(Button::new(text).outline().full_width()).clicked() {
                self.language = language;
                return true;
            }
        }
        let theme_label = if self.theme == Theme::Dark {
            format!("{} {}", "🌙", self.text(&Msg::Base(BaseMsg::Theme)))
        } else {
            format!("{} {}", "☀️", self.text(&Msg::Base(BaseMsg::Theme)))
        };
        if ui
            .add(Button::new(theme_label).outline().full_width())
            .clicked()
        {
            self.theme = self.theme.toggle();
            self.theme.apply(ui.ctx());
            return true;
        }
        false
    }

    fn language_label(&self, language: Language) -> String {
        format!(
            "{} {}",
            self.text(&Msg::Base(BaseMsg::LanguageFlag(language))),
            self.text(&Msg::Base(BaseMsg::LanguageName(language)))
        )
    }

    fn tab_index(&self) -> Option<usize> {
        MAIN_SCREENS
            .iter()
            .position(|screen| *screen == self.screen)
    }

    pub(crate) fn is_mobile(&self) -> bool {
        self.ctx.content_rect().width() < MOBILE_BREAKPOINT
    }

    fn screen_title(&self, screen: Screen) -> String {
        let msg = match screen {
            Screen::Home => Msg::Base(BaseMsg::Home),
            Screen::Open => Msg::OpenButton,
            Screen::View => Msg::ViewButton,
            Screen::Share => Msg::Share,
            Screen::File => Msg::File,
            Screen::About => Msg::Base(BaseMsg::Application),
            Screen::Donate => Msg::Base(BaseMsg::Donate),
            Screen::License => Msg::Base(BaseMsg::TermsOfServiceTitle),
            Screen::Privacy => Msg::Base(BaseMsg::PrivacyPolicyTitle),
        };
        self.text(&msg)
    }
}

impl eframe::App for CryptonoteApp {
    fn ui(&mut self, ui: &mut egui::Ui, _frame: &mut eframe::Frame) {
        #[cfg(target_os = "android")]
        crate::android::poll_ime(ui.ctx());
        self.ctx = ui.ctx().clone();
        self.drain_events();
        if !self.is_mobile() {
            self.nav_open = false;
        }
        let _nav = egui::Panel::top("nav").show(ui, |nav| self.render_nav(nav));
        if self.is_mobile() {
            let width = (self.ctx.content_rect().width() * 0.7).clamp(220.0, 300.0);
            let mut nav_open = self.nav_open;
            let close = Drawer::new("nav_drawer", &mut nav_open)
                .side(DrawerSide::Right)
                .width(width)
                .title(APP_ATTRS.app_name())
                .show(ui.ctx(), |drawer| self.render_drawer(drawer))
                .unwrap_or(false);
            if close {
                self.nav_open = false;
            } else {
                self.nav_open = nav_open;
            }
        }
        let _central = egui::CentralPanel::default().show(ui, |central| {
            let available = central.available_width();
            let width = available.min(CONTENT_MAX_WIDTH);
            let margin = (available - width) * 0.5;
            let _ = central.with_layout(egui::Layout::left_to_right(egui::Align::Min), |row| {
                row.add_space(margin);
                let _ = row.vertical(|col| {
                    col.set_max_width(width);
                    let _scroll = egui::ScrollArea::vertical()
                        .auto_shrink([false, false])
                        .show(col, |scroll| {
                            scroll.add_space(8.0);
                            self.render_screen(scroll);
                            self.render_status(scroll);
                        });
                });
            });
        });
        self.render_toasts(ui.ctx());
    }
}

impl CryptonoteApp {
    pub(crate) fn render_dock(
        &mut self,
        ui: &mut egui::Ui,
        mut add: impl FnMut(&mut egui::Ui, &mut Self),
    ) {
        let _ = ui.horizontal_wrapped(|row| add(row, self));
    }

    fn render_status(&mut self, ui: &mut egui::Ui) {
        if let Some(job) = &self.job {
            let label = self.text(&Msg::Base(BaseMsg::Stage(job.stage)));
            ui.add_space(8.0);
            _ = ui.add(
                ProgressBar::new(f32::from(job.percent()) / 100.0)
                    .text(format!("{label} {}%", job.percent()))
                    .accent(Accent::Blue),
            );
        }
    }

    fn render_toasts(&mut self, ctx: &egui::Context) {
        match &self.message {
            Some(msg) if !self.toasted => {
                let tone = if matches!(msg, Msg::Error(_)) {
                    BadgeTone::Danger
                } else {
                    BadgeTone::Ok
                };
                let text = self.text(msg);
                Toast::new(text).tone(tone).show(ctx);
                self.toasted = true;
            }
            None => self.toasted = false,
            Some(_) => {}
        }
        Toasts::new().render(ctx);
    }
}
