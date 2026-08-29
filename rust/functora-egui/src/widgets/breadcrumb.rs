use egui::{CornerRadius, CursorIcon, Painter, Rect, Sense, Stroke, Ui, pos2, vec2};
use functora_core::i18n::Language;

use crate::icons::lucide_icon::LucideIcon;
use crate::icons::paint_icon::paint_icon;
use crate::nav::NavHistory;
use crate::responsive::responsive_ext::ResponsiveExt;
use crate::route::{RouteMetadata, breadcrumbs_for};
use crate::theme::shadcn_theme_ext::ShadcnThemeExt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NavAction<R> {
    Back,
    Forward,
    Route(R),
}

#[must_use]
pub struct Breadcrumb<'a, R: RouteMetadata> {
    route: &'a R,
    history: &'a NavHistory<R>,
    separator: String,
}

impl<'a, R: RouteMetadata> Breadcrumb<'a, R> {
    pub fn new(route: &'a R, history: &'a NavHistory<R>) -> Self {
        Self {
            route,
            history,
            separator: "/".into(),
        }
    }

    pub fn separator(mut self, s: impl Into<String>) -> Self {
        self.separator = s.into();
        self
    }

    pub fn show(&self, ui: &mut Ui, lang: Language) -> Option<NavAction<R>> {
        let theme = ShadcnThemeExt::shadcn_theme(ui.ctx());
        let mut action = None;

        let btn_size = ui.responsive_spacing().touch_height * 0.5;
        let icon_size = btn_size * 0.5;

        let _ = ui.horizontal(|ui_inner| {
            ui_inner.spacing_mut().item_spacing.x = 4.0;

            if self.history.can_go_back() {
                let (rect, response) = ui_inner
                    .allocate_at_least(vec2(btn_size, ui_inner.available_height()), Sense::click());
                if ui_inner
                    .interact(rect, ui_inner.id().with("back_btn"), Sense::click())
                    .on_hover_cursor(CursorIcon::PointingHand)
                    .clicked()
                {
                    action = Some(NavAction::Back);
                }
                if response.hovered() {
                    paint_hover_bg(ui_inner.painter(), &rect, theme.accent);
                }
                let icon_rect = Rect::from_center_size(rect.center(), vec2(icon_size, icon_size));
                paint_icon(
                    ui_inner.painter(),
                    icon_rect,
                    &LucideIcon::ArrowLeft,
                    theme.foreground,
                );
            }

            let segments = breadcrumbs_for(self.route, lang);
            for (idx, seg) in segments.iter().enumerate() {
                let is_last = idx == segments.len() - 1;
                let sense = if is_last {
                    Sense::hover()
                } else {
                    Sense::click()
                };

                let font_id = egui::FontId::proportional(14.0);
                let galley = ui_inner.painter().layout_no_wrap(
                    seg.name.clone(),
                    font_id.clone(),
                    theme.foreground,
                );
                let padding = vec2(8.0, 4.0);
                let desired = galley.size() + padding * 2.0;

                let (rect, response) = ui_inner.allocate_at_least(desired, sense);

                if is_last {
                    let base_color = theme.foreground;
                    let text_pos = pos2(
                        rect.min.x + padding.x,
                        rect.center().y - galley.size().y / 2.0,
                    );
                    ui_inner.painter().galley(text_pos, galley, base_color);
                } else {
                    if response.hovered() {
                        ui_inner.ctx().set_cursor_icon(CursorIcon::PointingHand);
                    }
                    if response.clicked() {
                        action = Some(NavAction::Route(seg.route.clone()));
                    }
                    let hovered = response.hovered();
                    let pressed = response.is_pointer_button_down_on();
                    let color = if pressed {
                        theme.foreground
                    } else if hovered {
                        theme.accent_foreground
                    } else {
                        theme.foreground
                    };

                    if hovered || pressed {
                        let bg = if pressed {
                            crate::paint::interpolate_color::interpolate_color(
                                theme.accent,
                                theme.primary,
                                0.12,
                            )
                        } else {
                            theme.accent
                        };
                        paint_hover_bg(ui_inner.painter(), &rect, bg);
                    }

                    let galley_for_text = galley.clone();
                    let text_pos = pos2(
                        rect.min.x + padding.x,
                        rect.center().y - galley_for_text.size().y / 2.0,
                    );
                    ui_inner.painter().galley(text_pos, galley, color);

                    if hovered {
                        let underline_y = text_pos.y + galley_for_text.size().y;
                        let _ = ui_inner.painter().hline(
                            text_pos.x..=text_pos.x + rect.width() - padding.x * 2.0,
                            underline_y,
                            Stroke::new(1.0, color),
                        );
                    }
                }

                if !is_last {
                    let _ = ui_inner.label(
                        egui::RichText::new(&self.separator)
                            .color(theme.muted_foreground)
                            .size(14.0),
                    );
                }
            }

            if self.history.can_go_forward() {
                let (rect, response) = ui_inner
                    .allocate_at_least(vec2(btn_size, ui_inner.available_height()), Sense::click());
                if ui_inner
                    .interact(rect, ui_inner.id().with("fwd_btn"), Sense::click())
                    .on_hover_cursor(CursorIcon::PointingHand)
                    .clicked()
                {
                    action = Some(NavAction::Forward);
                }
                if response.hovered() {
                    paint_hover_bg(ui_inner.painter(), &rect, theme.accent);
                }
                let icon_rect = Rect::from_center_size(rect.center(), vec2(icon_size, icon_size));
                paint_icon(
                    ui_inner.painter(),
                    icon_rect,
                    &LucideIcon::ArrowRight,
                    theme.foreground,
                );
            }
        });

        action
    }
}

fn paint_hover_bg(painter: &Painter, rect: &Rect, color: egui::Color32) {
    let _ = painter.rect_filled(*rect, CornerRadius::same(4), color);
}
