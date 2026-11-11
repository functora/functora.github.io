//! White-label default screens, footer and sidebar for egui apps.
//!
//! Document screens (license, privacy, about) require the `markdown` feature.
//! The donate screen requires the `qr` and `clipboard` features.

pub use functora_core::white_label::*;

#[cfg(feature = "markdown")]
use crate::CommonMarkCache;
#[cfg(feature = "markdown")]
use crate::markdown_view::show as show_markdown;

use crate::i18n::{I18N, Language};
use crate::icons::lucide_icon::LucideIcon;
use crate::messages::Msg as BaseMsg;
use crate::route::RouteMetadata;
use crate::tokens::button_variant::ButtonVariant;
use crate::tokens::component_size::ComponentSize;
use crate::widgets::button::widget::Button;
use crate::widgets::card::widget::Card;
use crate::widgets::hypertext::widget::Hypertext;

#[cfg(any(feature = "desktop", feature = "web", feature = "android"))]
use crate::default_theme;
#[cfg(any(feature = "desktop", feature = "web", feature = "android"))]
use crate::i18n::detect_browser_language;
#[cfg(all(feature = "qr", feature = "clipboard"))]
use crate::theme::shadcn_theme_ext::ShadcnThemeExt;
#[cfg(any(feature = "desktop", feature = "web", feature = "android"))]
use crate::theme_extra::set_theme;
#[cfg(any(feature = "desktop", feature = "web", feature = "android"))]
use crate::{
    AppRouter, NavAction, PersistentState, ResponsiveExt, Shell, ToastState, ToastVariant,
    platform::request_quit,
};

/// Render the default MIT license screen. Requires the `markdown` feature.
#[cfg(feature = "markdown")]
pub fn screen_license(
    ui: &mut egui::Ui,
    cache: &mut CommonMarkCache,
    attrs: &AppAttrs,
    lang: Language,
) {
    let text = default_license_text(attrs, lang);
    let egui::InnerResponse { response, .. } = show_markdown(ui, cache, &text);
    let _ = response;
}

/// Render the default privacy policy screen. Requires the `markdown` feature.
#[cfg(feature = "markdown")]
pub fn screen_privacy(
    ui: &mut egui::Ui,
    cache: &mut CommonMarkCache,
    attrs: &AppAttrs,
    lang: Language,
) {
    let text = default_privacy_text(attrs, lang);
    let egui::InnerResponse { response, .. } = show_markdown(ui, cache, &text);
    let _ = response;
}

/// Render the default about screen. Requires the `markdown` feature.
#[cfg(feature = "markdown")]
pub fn screen_about(
    ui: &mut egui::Ui,
    cache: &mut CommonMarkCache,
    attrs: &AppAttrs,
    lang: Language,
) {
    let text = default_about_text(attrs, lang);
    let egui::InnerResponse { response, .. } = show_markdown(ui, cache, &text);
    let _ = response;
}

/// Render the default donate screen: greeting, intro and one block per
/// donation address with a QR code and a read-only address field.
/// Requires the `qr` and `clipboard` features.
#[cfg(all(feature = "qr", feature = "clipboard"))]
pub fn screen_donate(
    ui: &mut egui::Ui,
    attrs: &AppAttrs,
    lang: Language,
    content: &WhiteLabelContent<crate::messages::Msg>,
    now: f64,
    toast: &mut crate::ToastState,
) {
    use crate::widgets::qr_image::widget::QrImage;
    use crate::widgets::textarea_paste_clear::widget::TextareaPasteClear;

    let greeting = content
        .donate_greeting
        .as_ref()
        .map_or_else(|| default_donate_greeting(attrs, lang), |m| m.render(lang));
    let intro = content
        .donate_intro
        .as_ref()
        .map_or_else(|| default_donate_intro(attrs, lang), |m| m.render(lang));
    let blocks = if content.donate_blocks.is_empty() {
        default_donate_blocks()
    } else {
        content.donate_blocks.clone()
    };

    let author_url = attrs.author_url();
    let (greeting_mention, intro_mention) = match attrs.split_author_mention(&greeting) {
        Some(mention) => (Some(mention), None),
        None => (None, attrs.split_author_mention(&intro)),
    };

    match greeting_mention {
        Some(mention) => {
            let _ = Hypertext::new()
                .text(mention.before)
                .link(mention.name, author_url.clone())
                .text(mention.after)
                .size(16.0)
                .show(ui);
        }
        None => {
            let _ = ui.label(egui::RichText::new(&greeting).size(16.0).weak());
        }
    }
    ui.add_space(4.0);
    let _ = Card::new().show(ui, |inner| {
        inner.expand_to_include_x(inner.max_rect().right());
        match intro_mention {
            Some(mention) => {
                let _ = Hypertext::new()
                    .text(mention.before)
                    .link(mention.name, author_url.clone())
                    .text(mention.after)
                    .text_color(ShadcnThemeExt::shadcn_theme(inner.ctx()).foreground)
                    .size(13.0)
                    .show(inner);
            }
            None => {
                let _ = inner.label(egui::RichText::new(&intro).size(13.0));
            }
        }
    });
    ui.add_space(8.0);
    for block in blocks {
        let _ = ui.label(egui::RichText::new(&block.label).size(16.0).weak());
        ui.add_space(4.0);
        let _ = QrImage::new(&block.address).show(ui);
        ui.add_space(4.0);
        let mut address = block.address.clone();
        let resp = TextareaPasteClear::new(&mut address)
            .readonly()
            .min_height(80.0)
            .show(ui);
        if resp.pasted {
            toast.add(
                BaseMsg::Pasted.render(lang),
                crate::ToastVariant::Success,
                now,
            );
        }
        if resp.copied {
            toast.add(
                BaseMsg::Copied.render(lang),
                crate::ToastVariant::Success,
                now,
            );
        }
        ui.add_space(20.0);
    }
}

/// Default sidebar entry labels (Home, About, Donate, Reset) for `lang`, i18n.
#[must_use]
pub fn default_sidebar_labels(lang: Language) -> Vec<String> {
    let reset = match lang {
        Language::Spa => "Restablecer",
        Language::Rus => "Сбросить",
        _ => "Reset",
    };
    vec![
        BaseMsg::Home.render(lang),
        BaseMsg::About.render(lang),
        BaseMsg::Donate.render(lang),
        reset.to_string(),
    ]
}

/// Default icons for the default sidebar entries, in order: Home, About, Donate, Reset.
pub const DEFAULT_SIDEBAR_ICONS: [LucideIcon; 4] = [
    LucideIcon::House,
    LucideIcon::Info,
    LucideIcon::Heart,
    LucideIcon::RotateCcw,
];

/// True on the platforms where the app window can be closed from the UI.
///
/// The web build cannot close the browser tab and Android closes the app
/// through the system, so the quit entry is a desktop-only affordance.
pub const QUIT_AVAILABLE: bool = cfg!(not(any(target_arch = "wasm32", target_os = "android")));

/// What a click on the default sidebar asks the app to do.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SidebarAction<R> {
    /// Open another screen.
    Navigate(R),
    /// Reset the app to its initial state.
    Reset,
    /// Close the app window.
    Quit,
}

/// The label of the quit entry of the default sidebar.
fn quit_label(lang: Language) -> String {
    BaseMsg::Quit.render(lang)
}

/// Renders the default white-label sidebar.
///
/// The navigation entries come first, the reset entry after them and, on
/// desktop, the quit entry last. The returned action is the entry the user
/// picked, if any.
pub fn default_sidebar<R: RouteMetadata>(
    ui: &mut egui::Ui,
    entries: &[(R, LucideIcon)],
    reset_label: &str,
    selected: &R,
    lang: Language,
) -> Option<SidebarAction<R>> {
    let nav_responses = entries
        .iter()
        .map(|(route, icon)| {
            let is_selected = *route == *selected;
            ui.add(
                Button::new(route.label(lang).into_owned())
                    .icon(*icon)
                    .size(ComponentSize::Sm)
                    .variant(if is_selected {
                        ButtonVariant::Default
                    } else {
                        ButtonVariant::Ghost
                    })
                    .selected(is_selected)
                    .full_width(),
            )
        })
        .collect::<Vec<_>>();
    let reset_response = ui.add(
        Button::new(reset_label)
            .icon(LucideIcon::RotateCcw)
            .size(ComponentSize::Sm)
            .variant(ButtonVariant::Ghost)
            .full_width(),
    );
    let quit_response = if QUIT_AVAILABLE {
        Some(
            ui.add(
                Button::new(quit_label(lang))
                    .icon(LucideIcon::Power)
                    .size(ComponentSize::Sm)
                    .variant(ButtonVariant::Ghost)
                    .full_width(),
            ),
        )
    } else {
        None
    };

    nav_responses
        .iter()
        .position(egui::Response::clicked)
        .map(|index| SidebarAction::Navigate(entries[index].0.clone()))
        .or_else(|| reset_response.clicked().then_some(SidebarAction::Reset))
        .or_else(|| {
            quit_response
                .as_ref()
                .is_some_and(egui::Response::clicked)
                .then_some(SidebarAction::Quit)
        })
}

/// Render the default white-label footer. Every link is derived from `attrs`.
///
/// Returns the clicked action id, if any: `"license"`, `"privacy"`, `"about"`
/// or `"donate"`, so the app can navigate to the matching screen.
pub fn default_footer(ui: &mut egui::Ui, attrs: &AppAttrs, lang: Language) -> Option<String> {
    let (_, clicked) = Hypertext::new()
        .text(format!("© {} ", functora_core::FUNCTORA_CORE_YEAR))
        .link(FUNCTORA_ORG, attrs.author_url())
        .text(format!(". {} ", BaseMsg::AllRightsReserved.render(lang)))
        .text(format!("{} ", BaseMsg::ByContinuing.render(lang)))
        .action(BaseMsg::TermsOfService.render(lang), "license".to_string())
        .text(format!(" {} ", BaseMsg::And.render(lang)))
        .action(
            BaseMsg::PrivacyPolicyAnd.render(lang),
            "privacy".to_string(),
        )
        .text(". ")
        .action(BaseMsg::FooterShareWord.render(lang), "about".to_string())
        .text(format!(
            " {} {} ",
            BaseMsg::FooterAppWord.render(lang),
            BaseMsg::And.render(lang)
        ))
        .action(BaseMsg::DonateLink.render(lang), "donate".to_string())
        .text(format!(
            ". {} {}.",
            BaseMsg::VersionLabel.render(lang),
            attrs.vsn
        ))
        .size(11.0)
        .centered()
        .show_action(ui);
    clicked
}

/// Generic white-label application skeleton.
///
/// `R` is the app's route type ([`RouteMetadata`]) and `Extra` is app-specific
/// persistent state carried alongside theme and language. The app supplies
/// [`AppAttrs`], `Extra`, the sidebar route entries and a `route_content`
/// closure rendering each route's body; the white label handles the shell,
/// sidebar, footer, routing, language and theme.
///
/// Standard screens are provided by [`screen_license`], [`screen_privacy`],
/// [`screen_about`] and [`screen_donate`] and can be rendered from
/// `route_content` when the route matches.
#[cfg(any(feature = "desktop", feature = "web", feature = "android"))]
pub struct WhiteLabelApp<R, Extra>
where
    R: RouteMetadata,
    Extra: Clone + PartialEq + Default,
{
    router: AppRouter<R>,
    persistent: PersistentState<Extra>,
    toast: ToastState,
    sidebar_collapsed: bool,
    attrs: AppAttrs,
    content: WhiteLabelContent<crate::messages::Msg>,
    nav_entries: Vec<(R, LucideIcon)>,
    sidebar_labels: Vec<String>,
}

#[cfg(any(feature = "desktop", feature = "web", feature = "android"))]
impl<R, Extra> WhiteLabelApp<R, Extra>
where
    R: RouteMetadata + Default,
    Extra: Clone + PartialEq + Default,
{
    #[must_use]
    pub fn new(ctx: &egui::Context, attrs: AppAttrs, extra: Extra) -> Self {
        Self {
            router: AppRouter::new(&R::default()),
            persistent: PersistentState {
                theme: default_theme(ctx),
                language: detect_browser_language(),
                extra,
            },
            toast: ToastState::new(),
            sidebar_collapsed: crate::initial_sidebar_collapsed(ctx),
            attrs,
            content: WhiteLabelContent::default(),
            nav_entries: Vec::new(),
            sidebar_labels: Vec::new(),
        }
    }

    #[must_use]
    pub fn with_nav_entries(mut self, entries: impl IntoIterator<Item = (R, LucideIcon)>) -> Self {
        self.nav_entries = entries.into_iter().collect();
        self
    }

    /// Override the default sidebar labels ([`default_sidebar_labels`]) with
    /// custom labels, e.g. category names for a demo or catalog app.
    #[must_use]
    pub fn with_sidebar_labels(mut self, labels: impl IntoIterator<Item = String>) -> Self {
        self.sidebar_labels = labels.into_iter().collect();
        self
    }

    #[must_use]
    pub fn lang(&self) -> Language {
        self.persistent.language
    }

    pub fn set_language(&mut self, lang: Language) {
        if self.persistent.language != lang {
            self.persistent.language = lang;
            self.toast = ToastState::new();
        }
    }

    pub fn navigate(&mut self, route: R) {
        self.router.navigate(route);
    }

    #[must_use]
    pub fn router(&self) -> &AppRouter<R> {
        &self.router
    }

    #[must_use]
    pub fn router_mut(&mut self) -> &mut AppRouter<R> {
        &mut self.router
    }

    #[must_use]
    pub fn attrs(&self) -> &AppAttrs {
        &self.attrs
    }

    #[must_use]
    pub fn content(&self) -> &WhiteLabelContent<crate::messages::Msg> {
        &self.content
    }

    pub fn content_mut(&mut self) -> &mut WhiteLabelContent<crate::messages::Msg> {
        &mut self.content
    }

    #[must_use]
    pub fn toast(&self) -> &ToastState {
        &self.toast
    }

    pub fn toast_mut(&mut self) -> &mut ToastState {
        &mut self.toast
    }

    fn reset(&mut self, now: f64) {
        let lang = self.persistent.language;
        let reset_done: String = match lang {
            Language::Spa => "Aplicación restablecida".into(),
            Language::Rus => "Приложение сброшено".into(),
            _ => "App reset".into(),
        };
        self.persistent.extra = Extra::default();
        self.toast = ToastState::new();
        self.toast.add(reset_done, ToastVariant::Success, now);
        self.router.reset(R::default());
    }
}

/// `eframe` integration: renders the adaptive shell, sidebar and footer and
/// delegates each route's body to `route_content`.
/// Requires one of the `desktop`, `web` or `android` features (eframe).
#[cfg(any(feature = "desktop", feature = "web", feature = "android"))]
impl<R, Extra> WhiteLabelApp<R, Extra>
where
    R: RouteMetadata + Default,
    Extra: Clone + PartialEq + Default,
{
    #[allow(clippy::too_many_arguments)]
    pub fn ui(
        &mut self,
        ui: &mut egui::Ui,
        frame: &mut eframe::Frame,
        route_content: &mut impl FnMut(&mut egui::Ui, &R, &Extra),
    ) -> Option<NavAction<R>> {
        let _ = frame;
        let ctx = ui.ctx().clone();
        set_theme(&ctx, self.persistent.theme);
        let mut theme = self.persistent.theme;
        let mut collapsed_val = self.sidebar_collapsed;
        let route = self.router.current().clone();
        let history = self.router.history().clone();
        let needs_reset = std::cell::Cell::new(false);
        let pending_nav = std::cell::Cell::new(None::<R>);
        let selected_snapshot = route.clone();
        let lang_cell = std::cell::Cell::new(self.persistent.language);
        let brand = self.attrs.app;
        let attrs = self.attrs;
        let entries = self.nav_entries.clone();
        let extra = self.persistent.extra.clone();
        let labels = if self.sidebar_labels.is_empty() {
            default_sidebar_labels(lang_cell.get())
        } else {
            self.sidebar_labels.clone()
        };
        let reset_label = labels.last().cloned().unwrap_or_else(|| "Reset".into());
        let quit_text = quit_label(lang_cell.get());
        let sidebar_labels = labels
            .iter()
            .map(String::as_str)
            .chain(QUIT_AVAILABLE.then_some(quit_text.as_str()))
            .collect::<Vec<_>>();

        let pending_ref = &pending_nav;
        let lang_ref = &lang_cell;
        let needs_reset_ref = &needs_reset;
        let breadcrumb_action = Shell::new(brand, &mut collapsed_val, {
            move |side_ui| {
                let mut close = false;
                match default_sidebar(
                    side_ui,
                    &entries,
                    &reset_label,
                    &selected_snapshot,
                    lang_ref.get(),
                ) {
                    Some(SidebarAction::Navigate(target)) => {
                        pending_ref.set(Some(target));
                        close |= side_ui.on_mobile();
                        side_ui.ctx().request_repaint();
                    }
                    Some(SidebarAction::Reset) => {
                        needs_reset_ref.set(true);
                        close |= side_ui.on_mobile();
                        side_ui.ctx().request_repaint();
                    }
                    Some(SidebarAction::Quit) => {
                        request_quit(side_ui.ctx());
                    }
                    None => {}
                }
                close
            }
        })
        .theme(&mut theme)
        .language(&lang_cell)
        .on_brand(move || {
            pending_ref.set(Some(R::default()));
        })
        .footer(move |footer_ui| {
            let _ = default_footer(footer_ui, &attrs, lang_ref.get());
        })
        .sidebar_labels(sidebar_labels)
        .breadcrumb(&route, &history)
        .show(ui, |content_ui| {
            route_content(content_ui, &route, &extra);
            content_ui.add_space(16.0);
        });

        self.sidebar_collapsed = collapsed_val;
        if theme != self.persistent.theme {
            self.persistent.theme = theme;
        }
        self.set_language(lang_cell.get());
        if let Some(next_route) = pending_nav.take() {
            self.navigate(next_route);
            ctx.request_repaint();
        }
        if needs_reset.get() {
            let now = ctx.input(|i| i.time);
            self.reset(now);
        }
        breadcrumb_action
    }
}
