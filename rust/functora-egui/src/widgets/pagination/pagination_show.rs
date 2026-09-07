//! Show method for Pagination — renders page navigation controls.

impl super::widget::Pagination {
    /// Shows the pagination. `current` is the active page (0-indexed).
    pub fn show(self, ui: &mut egui::Ui, current: &mut usize) -> egui::Response {
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());
        let spacing =
            crate::responsive::responsive_ext::ResponsiveExt::responsive_spacing(ui.ctx());
        let total = self.total_pages;
        if total == 0 {
            ui.allocate_response(egui::vec2(0.0, 0.0), egui::Sense::hover())
        } else {
            *current = (*current).min(total.saturating_sub(1));
            let last_page = total.saturating_sub(1);
            let requested = self.max_visible.max(1);
            let size = spacing.touch_height;
            let gap: f32 = 4.0;
            let capacity = std::iter::successors(Some((0_usize, 0.0_f32)), |(count, width)| {
                Some((count.saturating_add(1), width + size + gap))
            })
            .take_while(|(_, width)| *width <= ui.available_width() + gap)
            .last()
            .map_or(0, |(count, _)| count);
            let want = requested.min(total);
            let anchored_want = want.saturating_add(6);
            let plain_want = want.saturating_add(2);
            let anchored = want < total && anchored_want <= capacity;
            let window = if plain_want <= capacity {
                want
            } else {
                capacity.saturating_sub(2).max(1).min(want)
            };
            let half = window / 2;
            let start = if *current <= half {
                0
            } else if current.saturating_add(half) >= last_page {
                last_page.saturating_sub(window.saturating_sub(1))
            } else {
                current.saturating_sub(half)
            };
            let end = start.saturating_add(window).min(total);

            ui.horizontal_wrapped(|inner_ui| {
                inner_ui.spacing_mut().item_spacing.x = gap;

                let prev_enabled = *current > 0;
                let prev = Self::icon_button(
                    inner_ui,
                    &theme,
                    crate::icons::lucide_icon::LucideIcon::ChevronLeft,
                    prev_enabled,
                );
                if prev.clicked() && prev_enabled {
                    *current = current.saturating_sub(1);
                    inner_ui.ctx().request_repaint();
                }

                if anchored && start > 0 {
                    let btn = Self::page_button(inner_ui, &theme, "1", false, true);
                    if btn.clicked() {
                        *current = 0;
                        inner_ui.ctx().request_repaint();
                    }
                    if start > 1 {
                        Self::ellipsis_indicator(inner_ui, &theme);
                    }
                }

                (start..end).for_each(|page| {
                    let label = format!("{}", page.saturating_add(1));
                    let is_current = page == *current;
                    let btn = Self::page_button(inner_ui, &theme, &label, is_current, true);
                    if btn.clicked() && !is_current {
                        *current = page;
                        inner_ui.ctx().request_repaint();
                    }
                });

                if anchored && end < total {
                    if end.saturating_add(1) < total {
                        Self::ellipsis_indicator(inner_ui, &theme);
                    }
                    let label = format!("{total}");
                    let btn = Self::page_button(inner_ui, &theme, &label, false, true);
                    if btn.clicked() {
                        *current = last_page;
                        inner_ui.ctx().request_repaint();
                    }
                }

                let next_enabled = *current < last_page;
                let next = Self::icon_button(
                    inner_ui,
                    &theme,
                    crate::icons::lucide_icon::LucideIcon::ChevronRight,
                    next_enabled,
                );
                if next.clicked() && next_enabled {
                    *current = (*current).saturating_add(1).min(last_page);
                    inner_ui.ctx().request_repaint();
                }
            })
            .response
        }
    }

    fn icon_button(
        ui: &mut egui::Ui,
        theme: &crate::theme::shadcn_theme::ShadcnTheme,
        icon: crate::icons::lucide_icon::LucideIcon,
        enabled: bool,
    ) -> egui::Response {
        let size = crate::responsive::responsive_ext::ResponsiveExt::responsive_spacing(ui.ctx())
            .touch_height;
        let icon_size: f32 = 14.0;

        let (rect, response) = ui.allocate_exact_size(egui::vec2(size, size), egui::Sense::click());

        if ui.is_rect_visible(rect) {
            let painter = ui.painter();
            let cr = egui::CornerRadius::same(crate::utils::f32_to_u8_clamped(theme.radius));

            let bg = if response.is_pointer_button_down_on() && enabled {
                crate::paint::interpolate_color::interpolate_color(
                    theme.accent,
                    theme.primary,
                    0.12,
                )
            } else if response.hovered() && enabled {
                theme.accent
            } else {
                egui::Color32::TRANSPARENT
            };

            let _ = painter.rect_filled(rect, cr, bg);
            let _ = painter.rect_stroke(
                rect,
                cr,
                egui::Stroke::new(1.0, theme.border),
                egui::epaint::StrokeKind::Inside,
            );

            let fg = if enabled {
                theme.foreground
            } else {
                theme.muted_foreground
            };

            let icon_rect =
                egui::Rect::from_center_size(rect.center(), egui::vec2(icon_size, icon_size));
            crate::icons::paint_icon::paint_icon(painter, icon_rect, &icon, fg);
        }

        if response.hovered() && enabled {
            ui.ctx().set_cursor_icon(egui::CursorIcon::PointingHand);
        }

        response
    }

    fn ellipsis_indicator(ui: &mut egui::Ui, theme: &crate::theme::shadcn_theme::ShadcnTheme) {
        let size = crate::responsive::responsive_ext::ResponsiveExt::responsive_spacing(ui.ctx())
            .touch_height;
        let icon_size: f32 = 14.0;
        let (rect, _) = ui.allocate_exact_size(egui::vec2(size, size), egui::Sense::hover());
        if ui.is_rect_visible(rect) {
            let icon_rect =
                egui::Rect::from_center_size(rect.center(), egui::vec2(icon_size, icon_size));
            crate::icons::paint_icon::paint_icon(
                ui.painter(),
                icon_rect,
                &crate::icons::lucide_icon::LucideIcon::Ellipsis,
                theme.muted_foreground,
            );
        }
    }

    fn page_button(
        ui: &mut egui::Ui,
        theme: &crate::theme::shadcn_theme::ShadcnTheme,
        label: &str,
        is_active: bool,
        enabled: bool,
    ) -> egui::Response {
        let size = crate::responsive::responsive_ext::ResponsiveExt::responsive_spacing(ui.ctx())
            .touch_height;
        let font_size: f32 = 14.0;

        let fg = if !enabled {
            theme.muted_foreground
        } else if is_active {
            theme.primary_foreground
        } else {
            theme.foreground
        };

        let galley = ui.painter().layout_no_wrap(
            label.to_owned(),
            egui::FontId::proportional(font_size),
            fg,
        );

        let (rect, response) = ui.allocate_exact_size(egui::vec2(size, size), egui::Sense::click());

        if ui.is_rect_visible(rect) {
            let painter = ui.painter();
            let cr = egui::CornerRadius::same(crate::utils::f32_to_u8_clamped(theme.radius));

            let bg = if is_active {
                theme.primary
            } else if response.is_pointer_button_down_on() && enabled {
                crate::paint::interpolate_color::interpolate_color(
                    theme.accent,
                    theme.primary,
                    0.12,
                )
            } else if response.hovered() && enabled {
                theme.accent
            } else {
                egui::Color32::TRANSPARENT
            };

            let _ = painter.rect_filled(rect, cr, bg);

            if !is_active {
                let _ = painter.rect_stroke(
                    rect,
                    cr,
                    egui::Stroke::new(1.0, theme.border),
                    egui::epaint::StrokeKind::Inside,
                );
            }

            let text_pos = egui::pos2(
                rect.center().x - galley.size().x / 2.0,
                rect.center().y - galley.size().y / 2.0,
            );
            painter.galley(text_pos, galley, fg);
        }

        if response.hovered() && enabled && !is_active {
            ui.ctx().set_cursor_icon(egui::CursorIcon::PointingHand);
        }

        response
    }
}
