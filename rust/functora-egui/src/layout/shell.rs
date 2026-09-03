use std::cell::Cell;

use crate::nav::NavHistory;
use crate::responsive::responsive_ext::ResponsiveExt;
use crate::route::RouteMetadata;
use crate::theme::shadcn_theme_ext::ShadcnThemeExt;
use crate::theme_extra::Theme;
use crate::widgets::breadcrumb::{Breadcrumb, NavAction};

type FooterFn<'a> = Box<dyn FnOnce(&mut egui::Ui) + 'a>;

#[must_use]
pub fn sidebar_effective_width(ctx: &egui::Context, labels: &[&str]) -> f32 {
    let spacing = ctx.responsive_spacing();
    labels
        .iter()
        .map(|name| {
            let font_id = egui::FontId::proportional(14.0);
            ctx.fonts_mut(|fonts| {
                fonts
                    .layout_no_wrap((*name).to_owned(), font_id, egui::Color32::WHITE)
                    .rect
                    .width()
            })
        })
        .fold(0.0, f32::max)
        .pipe(|max_text| {
            let icon = spacing.touch_height * 0.5;
            max_text + icon + spacing.gap + spacing.touch_padding * 2.0 + spacing.gap
        })
}

trait Pipe: Sized {
    fn pipe<U>(self, f: impl FnOnce(Self) -> U) -> U {
        f(self)
    }
}
impl<T> Pipe for T {}

pub struct Shell<'a, R>
where
    R: RouteMetadata,
{
    brand: &'a str,
    version: Option<&'a str>,
    collapsed: &'a mut bool,
    theme: Option<&'a mut Theme>,
    language: Option<&'a Cell<functora_core::i18n::Language>>,
    search_label: Option<&'a str>,
    search_shortcut: Option<&'a str>,
    on_brand: Option<Box<dyn FnMut() + 'a>>,
    on_search: Option<Box<dyn FnMut() + 'a>>,
    sidebar: Box<dyn FnMut(&mut egui::Ui) -> bool + 'a>,
    breadcrumb: Option<(&'a R, &'a NavHistory<R>)>,
    sidebar_labels: Vec<&'a str>,
    scroll_top: bool,
    footer: Option<FooterFn<'a>>,
}

impl<'a, R> Shell<'a, R>
where
    R: RouteMetadata,
{
    pub fn new(
        brand: &'a str,
        collapsed: &'a mut bool,
        sidebar: impl FnMut(&mut egui::Ui) -> bool + 'a,
    ) -> Self {
        Self {
            brand,
            version: None,
            collapsed,
            theme: None,
            language: None,
            search_label: None,
            search_shortcut: None,
            on_brand: None,
            on_search: None,
            sidebar: Box::new(sidebar),
            breadcrumb: None,
            sidebar_labels: Vec::new(),
            scroll_top: false,
            footer: None,
        }
    }

    #[must_use]
    pub fn version(mut self, version: &'a str) -> Self {
        self.version = Some(version);
        self
    }

    #[must_use]
    pub fn theme(mut self, theme: &'a mut Theme) -> Self {
        self.theme = Some(theme);
        self
    }

    #[must_use]
    pub fn language(mut self, language: &'a Cell<functora_core::i18n::Language>) -> Self {
        self.language = Some(language);
        self
    }

    #[must_use]
    pub fn search(mut self, label: &'a str, shortcut: Option<&'a str>) -> Self {
        self.search_label = Some(label);
        self.search_shortcut = shortcut;
        self
    }

    #[must_use]
    pub fn on_brand(mut self, f: impl FnMut() + 'a) -> Self {
        self.on_brand = Some(Box::new(f));
        self
    }

    #[must_use]
    pub fn on_search(mut self, f: impl FnMut() + 'a) -> Self {
        self.on_search = Some(Box::new(f));
        self
    }

    #[must_use]
    pub fn breadcrumb(mut self, route: &'a R, history: &'a NavHistory<R>) -> Self {
        self.breadcrumb = Some((route, history));
        self
    }

    #[must_use]
    pub fn sidebar_labels(mut self, labels: impl IntoIterator<Item = &'a str>) -> Self {
        self.sidebar_labels = labels.into_iter().collect();
        self
    }

    #[must_use]
    pub fn scroll_top(mut self, top: bool) -> Self {
        self.scroll_top = top;
        self
    }

    #[must_use]
    pub fn footer(mut self, footer: impl FnOnce(&mut egui::Ui) + 'a) -> Self {
        self.footer = Some(Box::new(footer));
        self
    }

    pub fn show(
        self,
        ui: &mut egui::Ui,
        content: impl FnOnce(&mut egui::Ui),
    ) -> Option<NavAction<R>> {
        let Self {
            brand,
            version,
            collapsed,
            theme,
            language,
            search_label,
            search_shortcut,
            mut on_brand,
            mut on_search,
            mut sidebar,
            breadcrumb,
            sidebar_labels,
            scroll_top,
            footer,
        } = self;
        let mut collapsed_val = *collapsed;
        let mut breadcrumb_action: Option<NavAction<R>> = None;
        let ctx = ui.ctx().clone();
        if let Some(th) = theme.as_deref() {
            crate::theme_extra::set_theme(&ctx, *th);
        }
        let theme_bg = ShadcnThemeExt::shadcn_theme(&ctx);
        let top = egui::Panel::top("top_bar")
            .frame(egui::Frame::NONE.fill(theme_bg.card))
            .show_separator_line(false)
            .show(ui, |top_ui| {
                let mut navbar = crate::widgets::navbar::widget::Navbar::new(brand);
                if let Some(v) = version {
                    navbar = navbar.version(v);
                }
                if let Some(l) = search_label {
                    navbar = navbar.search(l, search_shortcut);
                }
                let brand_cb: Option<&mut dyn FnMut()> =
                    on_brand.as_mut().map(|b| b.as_mut() as &mut dyn FnMut());
                let search_cb: Option<&mut dyn FnMut()> =
                    on_search.as_mut().map(|b| b.as_mut() as &mut dyn FnMut());
                let _ = navbar.show(
                    top_ui,
                    &mut collapsed_val,
                    theme,
                    language,
                    brand_cb,
                    search_cb,
                );
            });
        let _ = ui.painter().hline(
            top.response.rect.x_range(),
            top.response.rect.max.y - 0.5,
            egui::Stroke::new(1.0, theme_bg.border),
        );
        if !ctx.on_mobile() {
            let is_rail = collapsed_val;
            let spacing = ctx.responsive_spacing();
            let screen_width = ctx.input(|i| i.viewport_rect().width());
            let max_allowed_outer = (screen_width - spacing.page_padding * 2.0).max(0.0);
            let effective = if is_rail {
                spacing.touch_height
            } else {
                sidebar_effective_width(&ctx, &sidebar_labels)
                    .min((max_allowed_outer - 16.0).max(0.0))
            };
            let panel_outer = effective + 16.0;
            let panel_fill = if is_rail {
                theme_bg.background
            } else {
                theme_bg.card
            };
            let _ = egui::Panel::right("sidebar_panel")
                .exact_size(panel_outer)
                .frame(egui::Frame::NONE.fill(panel_fill))
                .resizable(false)
                .show_separator_line(false)
                .show(ui, |panel_ui| {
                    let close = std::cell::Cell::new(false);
                    let mut tmp = collapsed_val;
                    let _ = egui::ScrollArea::vertical().show(panel_ui, |scroll_ui| {
                        let _ = crate::widgets::sidebar::widget::Sidebar::new()
                            .width(effective)
                            .collapsible()
                            .show(scroll_ui, &mut tmp, |side_ui| {
                                close.set(sidebar(side_ui));
                            });
                    });
                    if close.get() {
                        tmp = true;
                    }
                    collapsed_val = tmp;
                });
        }
        let _ = egui::CentralPanel::default()
            .frame(egui::Frame::NONE.fill(theme_bg.background))
            .show(ui, |central_ui| {
                if central_ui.on_mobile() {
                    let spacing = central_ui.responsive_spacing();
                    let screen_width = central_ui.ctx().input(|i| i.viewport_rect().width());
                    let max_allowed_outer = (screen_width - spacing.page_padding * 2.0).max(0.0);
                    let effective = sidebar_effective_width(central_ui.ctx(), &sidebar_labels)
                        .min((max_allowed_outer - 16.0).max(0.0));
                    let close = std::cell::Cell::new(false);
                    let mut tmp = collapsed_val;
                    let _ = crate::widgets::sidebar::widget::Sidebar::new()
                        .width(effective)
                        .collapsible()
                        .show(central_ui, &mut tmp, |side_ui| {
                            close.set(sidebar(side_ui));
                        });
                    if close.get() {
                        tmp = true;
                    }
                    collapsed_val = tmp;
                    central_ui.add_space(-central_ui.spacing().item_spacing.y);
                }
                if let Some((route, history)) = breadcrumb {
                    let should_show = history.can_go_back()
                        || history.can_go_forward()
                        || route.parent().is_some();
                    if should_show {
                        let lang = language
                            .as_ref()
                            .map_or_else(functora_core::i18n::detect_browser_language, |c| c.get());
                        let available_w = central_ui.available_width();
                        let strip_theme = ShadcnThemeExt::shadcn_theme(central_ui.ctx());
                        let strip = egui::Frame::NONE
                            .fill(strip_theme.card)
                            .inner_margin(egui::Margin::symmetric(12, 8))
                            .show(central_ui, |strip_ui| {
                                strip_ui.set_min_width(available_w - 24.0);
                                if let Some(action) =
                                    Breadcrumb::new(route, history).show(strip_ui, lang)
                                {
                                    breadcrumb_action = Some(action);
                                }
                            });
                        let _ = central_ui.painter().hline(
                            central_ui.max_rect().x_range(),
                            strip.response.rect.max.y + 0.5,
                            egui::Stroke::new(1.0, strip_theme.border),
                        );
                        central_ui.add_space(8.0);
                    }
                }
                let spacing = central_ui.responsive_spacing();
                let available = central_ui.available_width();
                let content_width = available.min(spacing.content_max_width);
                let margin = ((available - content_width) * 0.5).max(0.0);
                let inner_width = (content_width - 2.0 * spacing.page_padding).max(0.0);
                let mut scroll = egui::ScrollArea::vertical().auto_shrink([false; 2]);
                if scroll_top {
                    scroll = scroll.vertical_scroll_offset(0.0);
                }
                let mut footer_opt = footer;
                let _ = scroll.show(central_ui, |scroll_ui| {
                    scroll_ui.add_space(spacing.page_padding);
                    let _ = scroll_ui.horizontal(|h_ui| {
                        h_ui.add_space(margin);
                        h_ui.add_space(spacing.page_padding);
                        let _ = h_ui.vertical(|v_ui| {
                            v_ui.set_max_width(inner_width);
                            content(v_ui);
                            if let Some(f) = footer_opt.take() {
                                v_ui.add_space(16.0);
                                let _ = crate::widgets::separator::widget::Separator::horizontal()
                                    .show(v_ui);
                                v_ui.add_space(8.0);
                                let _ = crate::widgets::footer::widget::Footer::new().show(v_ui, f);
                                v_ui.add_space(12.0);
                            } else {
                                v_ui.add_space(48.0);
                            }
                        });
                        h_ui.add_space(spacing.page_padding);
                        h_ui.add_space(margin);
                    });
                });
            });
        *collapsed = collapsed_val;
        breadcrumb_action
    }
}
