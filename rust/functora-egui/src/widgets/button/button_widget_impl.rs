//! Widget trait implementation for Button.

use std::sync::Arc;

/// Paint-independent geometry of a button, computed once and shared by
/// [`super::widget::Button::measure`] and the widget implementation.
struct Metrics {
    width: f32,
    height: f32,
    intrinsic: egui::Vec2,
    text_galley: Arc<egui::Galley>,
    shortcut_galley: Option<Arc<egui::Galley>>,
    icon_size: f32,
    icon_gap: f32,
}

impl Metrics {
    fn size(&self) -> egui::Vec2 {
        egui::vec2(self.width, self.height)
    }
}

/// Where a button sits in its [`crate::ButtonGroup`], resolved from the
/// group's cached row structure.
#[derive(Clone, Copy)]
struct GroupInfo {
    corner_radius: f32,
    first: bool,
    last: bool,
}

const ROW_ROUNDING_SLACK: f32 = 0.5;

fn fit_room(fixed: f32, room: f32) -> f32 {
    if room > 0.0 && fixed > room && fixed - room <= ROW_ROUNDING_SLACK {
        room
    } else {
        fixed
    }
}

impl super::widget::Button<'_> {
    /// The size this button takes up inside `available_width` of room.
    ///
    /// Text is wrapped into that room, so a title that does not fit on one
    /// line makes the button taller instead of wider.
    #[must_use]
    pub fn measure(&self, ui: &egui::Ui, available_width: f32) -> egui::Vec2 {
        self.metrics(ui, available_width, 0.0).size()
    }

    fn metrics(&self, ui: &egui::Ui, available_width: f32, min_height: f32) -> Metrics {
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());

        let mut style = super::button_variant_style::resolve_button_style(
            &theme,
            &crate::responsive::responsive_ext::ResponsiveExt::responsive_spacing(ui.ctx()),
            self.variant,
            self.size,
            false,
            false,
            !ui.is_enabled(),
        );

        if self.full_width {
            let spacing =
                crate::responsive::responsive_ext::ResponsiveExt::responsive_spacing(ui.ctx());
            let (h, pad, fs) = self.size.metrics_for(&spacing);
            style.height = h;
            style.h_padding = pad;
            style.font_size = fs;
        }

        let text_string = self.text.text().to_owned();
        let is_icon_only = text_string.is_empty() && self.icon.is_some();
        let has_icon = self.icon.is_some();

        let icon_size = style.height * 0.5;
        let icon_gap = 6.0;
        let shortcut_gap = 16.0;
        let icon_width = if is_icon_only {
            style.height
        } else if has_icon {
            icon_size + icon_gap
        } else {
            0.0
        };

        // The box: a fixed box keeps its width, a content-sized box is capped
        // by the room that is actually available, so neither can grow past
        // their container.
        let fixed_width = self
            .fixed_width
            .map(|width| fit_room(width, ui.available_rect_before_wrap().width()));
        let room_width = fixed_width.unwrap_or(available_width);

        let shortcut_galley = self.shortcut_text.as_ref().map(|st| {
            crate::text::layout(
                ui,
                st.clone(),
                egui::FontId::proportional(style.font_size - 1.0),
                theme.muted_foreground,
                (room_width - style.h_padding * 2.0 - icon_width).max(0.0),
                Some(crate::text::Wrap::Truncate),
            )
        });
        let shortcut_width = shortcut_galley
            .as_ref()
            .map_or(0.0, |g| shortcut_gap + g.size().x);

        // Everything the title has to wrap around: padding, icon, shortcut.
        let chrome = if is_icon_only {
            icon_width
        } else {
            icon_width + style.h_padding * 2.0 + shortcut_width
        };

        let text_galley = crate::text::layout(
            ui,
            text_string,
            egui::FontId::proportional(style.font_size),
            style.fg,
            (room_width - chrome).max(0.0),
            self.wrap,
        );

        let width = fixed_width.unwrap_or_else(|| {
            let natural = if is_icon_only {
                chrome
            } else {
                chrome + text_galley.size().x
            };
            natural.min(room_width)
        });
        let height = crate::text::box_height(&text_galley, style.height).max(min_height);

        let intrinsic_width = self.fixed_width.unwrap_or_else(|| {
            if is_icon_only {
                chrome
            } else {
                chrome + text_galley.intrinsic_size().x
            }
        });
        let intrinsic_height = crate::text::intrinsic_box_height(&text_galley, style.height)
            .max(min_height)
            .max(height);

        Metrics {
            width,
            height,
            intrinsic: egui::vec2(intrinsic_width, intrinsic_height),
            text_galley,
            shortcut_galley,
            icon_size,
            icon_gap,
        }
    }
}

impl egui::Widget for super::widget::Button<'_> {
    fn ui(self, ui: &mut egui::Ui) -> egui::Response {
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());

        // Check if inside a ButtonGroup and claim an index
        let group_key = crate::widgets::button_group::widget::ButtonGroup::context_key();
        let group_info = ui.ctx().data_mut(|d| {
            d.get_temp::<crate::widgets::button_group::button_group_context::ButtonGroupContext>(
                group_key,
            )
            .and_then(|mut ctx| {
                if !ctx.active {
                    return None;
                }
                let index = ctx.current_index;
                let cached_count = ctx.cached_count;
                let cached_rows = ctx.cached_rows.clone();
                let cr = ctx.corner_radius;
                ctx.current_index += 1;
                let _ = d.insert_temp(group_key, ctx);
                let (first, last) = row_span(&cached_rows, index, cached_count);
                Some(GroupInfo {
                    corner_radius: cr,
                    first,
                    last,
                })
            })
        });
        let in_group = group_info.is_some();

        let min_height = if in_group {
            let cached_max_height = ui.ctx().data(|d| {
                d.get_temp::<crate::widgets::button_group::button_group_context::ButtonGroupContext>(
                    group_key,
                )
                .map_or(0.0, |ctx| ctx.cached_max_height)
            });
            self.min_height.max(cached_max_height)
        } else {
            self.min_height
        };

        let Metrics {
            width,
            height,
            intrinsic,
            text_galley,
            shortcut_galley,
            icon_size,
            icon_gap,
        } = self.metrics(ui, ui.available_width(), min_height);

        let desired = egui::vec2(width, height);

        // fixed_width: exact size for equal-width strips (e.g. stretched tabs).
        // full_width: use allocate_at_least so the button stretches to fill
        // the menu/list width without inflating the menu's own desired size.
        let (rect, mut response) = if self.fixed_width.is_some() {
            ui.allocate_exact_size(desired, egui::Sense::click())
        } else if self.full_width {
            ui.allocate_at_least(desired, egui::Sense::click())
        } else {
            ui.allocate_exact_size(desired, egui::Sense::click())
        };
        response.set_intrinsic_size(intrinsic);

        // Record the rect in the group so it can lay out its rows
        if in_group {
            ui.ctx().data_mut(|d| {
                if let Some(mut ctx) = d.get_temp::<crate::widgets::button_group::button_group_context::ButtonGroupContext>(group_key)
                    && ctx.active {
                        ctx.rects.push(rect);
                        let _ = d.insert_temp(group_key, ctx);
                    }
            });
        }

        let is_loader = matches!(
            self.icon,
            Some(
                crate::icons::lucide_icon::LucideIcon::LoaderCircle
                    | crate::icons::lucide_icon::LucideIcon::Loader
                    | crate::icons::lucide_icon::LucideIcon::LoaderPinwheel
            )
        );
        if is_loader {
            ui.ctx().request_repaint();
        }

        let has_icon = self.icon.is_some();
        let is_icon_only = self.text.text().is_empty() && has_icon;
        let has_text = !self.text.text().is_empty();
        let has_shortcut = self.shortcut_text.is_some();

        // Re-resolve with actual interaction state
        let mut active_style = super::button_variant_style::resolve_button_style(
            &theme,
            &crate::responsive::responsive_ext::ResponsiveExt::responsive_spacing(ui.ctx()),
            self.variant,
            self.size,
            response.hovered(),
            response.is_pointer_button_down_on(),
            !ui.is_enabled(),
        );

        if self.full_width {
            let spacing =
                crate::responsive::responsive_ext::ResponsiveExt::responsive_spacing(ui.ctx());
            let (h, pad, fs) = self.size.metrics_for(&spacing);
            active_style.height = h;
            active_style.h_padding = pad;
            active_style.font_size = fs;
        }

        if self.selected {
            active_style.fg = theme.accent_foreground;
            active_style.bg = if response.is_pointer_button_down_on() && ui.is_enabled() {
                crate::paint::interpolate_color::interpolate_color(
                    theme.accent,
                    theme.primary,
                    0.12,
                )
            } else {
                theme.accent
            };
        }

        if ui.is_rect_visible(rect) {
            let painter = ui.painter();
            let cr = group_corner_radius(group_info.as_ref(), active_style.corner_radius);

            // Background
            let _ = painter.rect_filled(rect, cr, active_style.bg);

            // Border (skip when inside a button group — the group draws borders)
            if !in_group && let Some(border_color) = active_style.border {
                let _ = painter.rect_stroke(
                    rect,
                    cr,
                    egui::Stroke::new(1.0, border_color),
                    egui::epaint::StrokeKind::Inside,
                );
            }

            if is_icon_only {
                if let Some(ref icon) = self.icon {
                    let icon_rect = egui::Rect::from_center_size(
                        rect.center(),
                        egui::vec2(icon_size, icon_size),
                    );
                    if is_loader {
                        let time = crate::utils::f64_to_f32(ui.input(|i| i.time));
                        let angle = time * std::f32::consts::TAU;
                        let radius = icon_rect.width().min(icon_rect.height()) / 2.0 - 1.0;
                        crate::paint::paint_arc::paint_arc(
                            painter,
                            icon_rect.center(),
                            radius,
                            angle,
                            egui::Stroke::new(2.0, active_style.fg),
                        );
                    } else {
                        crate::icons::paint_icon::paint_icon(
                            painter,
                            icon_rect,
                            icon,
                            active_style.fg,
                        );
                    }
                }
            } else if has_icon && has_text {
                if has_shortcut {
                    let x = rect.min.x + active_style.h_padding;
                    if let Some(ref icon) = self.icon {
                        let icon_rect = egui::Rect::from_min_size(
                            egui::pos2(x, rect.center().y - icon_size / 2.0),
                            egui::vec2(icon_size, icon_size),
                        );
                        if is_loader {
                            let time = crate::utils::f64_to_f32(ui.input(|i| i.time));
                            let angle = time * std::f32::consts::TAU;
                            let radius = icon_rect.width().min(icon_rect.height()) / 2.0 - 1.0;
                            crate::paint::paint_arc::paint_arc(
                                painter,
                                icon_rect.center(),
                                radius,
                                angle,
                                egui::Stroke::new(2.0, active_style.fg),
                            );
                        } else {
                            crate::icons::paint_icon::paint_icon(
                                painter,
                                icon_rect,
                                icon,
                                active_style.fg,
                            );
                        }
                    }
                    let text_pos = egui::pos2(
                        x + icon_size + icon_gap,
                        rect.center().y - text_galley.size().y / 2.0,
                    );
                    painter.galley_with_override_text_color(
                        text_pos,
                        text_galley.clone(),
                        active_style.fg,
                    );
                } else {
                    let total_w = icon_size + icon_gap + text_galley.size().x;
                    let start_x = rect.center().x - total_w / 2.0;
                    if let Some(ref icon) = self.icon {
                        let icon_rect = egui::Rect::from_min_size(
                            egui::pos2(start_x, rect.center().y - icon_size / 2.0),
                            egui::vec2(icon_size, icon_size),
                        );
                        if is_loader {
                            let time = crate::utils::f64_to_f32(ui.input(|i| i.time));
                            let angle = time * std::f32::consts::TAU;
                            let radius = icon_rect.width().min(icon_rect.height()) / 2.0 - 1.0;
                            crate::paint::paint_arc::paint_arc(
                                painter,
                                icon_rect.center(),
                                radius,
                                angle,
                                egui::Stroke::new(2.0, active_style.fg),
                            );
                        } else {
                            crate::icons::paint_icon::paint_icon(
                                painter,
                                icon_rect,
                                icon,
                                active_style.fg,
                            );
                        }
                    }
                    let text_pos = egui::pos2(
                        start_x + icon_size + icon_gap,
                        rect.center().y - text_galley.size().y / 2.0,
                    );
                    painter.galley_with_override_text_color(
                        text_pos,
                        text_galley.clone(),
                        active_style.fg,
                    );
                }
            } else if has_shortcut || self.full_width {
                let text_pos = egui::pos2(
                    rect.min.x + active_style.h_padding,
                    rect.center().y - text_galley.size().y / 2.0,
                );
                painter.galley_with_override_text_color(
                    text_pos,
                    text_galley.clone(),
                    active_style.fg,
                );
            } else {
                let text_pos = egui::pos2(
                    rect.center().x - text_galley.size().x / 2.0,
                    rect.center().y - text_galley.size().y / 2.0,
                );
                painter.galley_with_override_text_color(
                    text_pos,
                    text_galley.clone(),
                    active_style.fg,
                );
            }

            if let Some(shortcut_galley_val) = shortcut_galley {
                let shortcut_pos = egui::pos2(
                    rect.max.x - active_style.h_padding - shortcut_galley_val.size().x,
                    rect.center().y - shortcut_galley_val.size().y / 2.0,
                );
                let shortcut_fg = if self.selected
                    || active_style.bg == theme.accent
                    || response.is_pointer_button_down_on()
                {
                    theme.accent_foreground
                } else {
                    theme.muted_foreground
                };
                let shortcut_color = if ui.is_enabled() {
                    shortcut_fg
                } else {
                    theme.muted_foreground
                };
                painter.galley_with_override_text_color(
                    shortcut_pos,
                    shortcut_galley_val,
                    shortcut_color,
                );
            }

            // Underline for Link variant
            if active_style.underline && response.hovered() {
                let text_pos = egui::pos2(
                    rect.center().x - text_galley.size().x / 2.0,
                    rect.center().y - text_galley.size().y / 2.0,
                );
                let underline_y = text_pos.y + text_galley.size().y;
                let _ = painter.hline(
                    text_pos.x..=text_pos.x + text_galley.size().x,
                    underline_y,
                    egui::Stroke::new(1.0, active_style.fg),
                );
            }

            // Focus ring
            if response.has_focus() {
                crate::paint::paint_focus_ring::paint_focus_ring(
                    painter,
                    rect,
                    active_style.corner_radius,
                    theme.ring,
                );
            }
        }

        if ui.is_enabled() {
            response.on_hover_cursor(egui::CursorIcon::PointingHand)
        } else {
            response
        }
    }
}

fn group_corner_radius(group_info: Option<&GroupInfo>, default_cr: f32) -> egui::CornerRadius {
    match group_info {
        Some(info) => {
            let r = crate::utils::f32_to_u8_clamped(info.corner_radius);
            egui::CornerRadius {
                nw: if info.first { r } else { 0 },
                sw: if info.first { r } else { 0 },
                ne: if info.last { r } else { 0 },
                se: if info.last { r } else { 0 },
            }
        }
        None => egui::CornerRadius::same(crate::utils::f32_to_u8_clamped(default_cr)),
    }
}

/// First/last button of the row `index` belongs to, from the group's cached
/// row structure. Falls back to a single row before the first frame of a
/// group, mirroring how [`ButtonGroupContext::cached_count`] behaves.
///
/// [`ButtonGroupContext::cached_count`]: crate::widgets::button_group::button_group_context::ButtonGroupContext::cached_count
fn row_span(rows: &[(usize, usize)], index: usize, cached_count: usize) -> (bool, bool) {
    match rows
        .iter()
        .find(|(first, last)| *first <= index && index <= *last)
    {
        Some((first, last)) => (index == *first, index == *last),
        None => (index == 0, cached_count > 0 && index == cached_count - 1),
    }
}
