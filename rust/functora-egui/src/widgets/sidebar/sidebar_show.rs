//! Show method for Sidebar -- renders a fixed sidebar panel.

impl super::widget::Sidebar {
    /// Shows the sidebar. `collapsed` controls collapsed state if collapsible.
    /// By default, on mobile it renders as a slide-in overlay drawer that is
    /// hidden while collapsed and covers the screen when open; use
    /// [`Sidebar::static_`] to keep the inline panel on all viewports.
    pub fn show(
        self,
        ui: &mut egui::Ui,
        collapsed: &mut bool,
        content: impl FnOnce(&mut egui::Ui),
    ) -> egui::Response {
        let ctx = ui.ctx();
        let spacing = crate::responsive::responsive_ext::ResponsiveExt::responsive_spacing(ctx);

        if self.responsive && spacing.is_mobile() {
            self.show_mobile_overlay(ui, collapsed, content)
        } else {
            self.show_inline(ui, collapsed, content)
        }
    }

    /// Toggle button for opening/closing a responsive sidebar: a hamburger
    /// menu. Place it in a top bar next to the sidebar. On desktop the
    /// sidebar header uses a distinct expand-collapse icon so the two
    /// toggles do not duplicate; this button always shows the hamburger and
    /// is sized to match the sidebar header toggle.
    pub fn toggle_button(ui: &mut egui::Ui, collapsed: &mut bool) -> egui::Response {
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());
        let spacing =
            crate::responsive::responsive_ext::ResponsiveExt::responsive_spacing(ui.ctx());
        let icon = crate::icons::lucide_icon::LucideIcon::Menu;

        let size = spacing.touch_height;
        let icon_size = size * 0.5;
        let (rect, response) = ui.allocate_exact_size(egui::vec2(size, size), egui::Sense::click());

        if ui.is_rect_visible(rect) {
            let painter = ui.painter();
            let cr = egui::CornerRadius::same(crate::utils::f32_to_u8_clamped(theme.radius));
            if response.hovered() || response.is_pointer_button_down_on() {
                let _ = painter.rect_filled(rect, cr, theme.muted);
            }
            let icon_rect =
                egui::Rect::from_center_size(rect.center(), egui::vec2(icon_size, icon_size));
            crate::icons::paint_icon::paint_icon(painter, icon_rect, &icon, theme.foreground);
        }

        if response.clicked() {
            *collapsed = !*collapsed;
            ui.ctx().request_repaint();
        }

        if response.hovered() {
            ui.ctx().set_cursor_icon(egui::CursorIcon::PointingHand);
        }

        response
    }

    fn collapse_button(ui: &mut egui::Ui, collapsed: &mut bool) -> egui::Response {
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());
        let spacing =
            crate::responsive::responsive_ext::ResponsiveExt::responsive_spacing(ui.ctx());
        let icon = if *collapsed {
            crate::icons::lucide_icon::LucideIcon::PanelRightOpen
        } else {
            crate::icons::lucide_icon::LucideIcon::PanelRightClose
        };

        let size = spacing.touch_height;
        let icon_size = size * 0.5;
        let (rect, response) = ui.allocate_exact_size(egui::vec2(size, size), egui::Sense::click());

        if ui.is_rect_visible(rect) {
            let painter = ui.painter();
            let cr = egui::CornerRadius::same(crate::utils::f32_to_u8_clamped(theme.radius));
            if response.hovered() || response.is_pointer_button_down_on() {
                let _ = painter.rect_filled(rect, cr, theme.muted);
            }
            let icon_rect =
                egui::Rect::from_center_size(rect.center(), egui::vec2(icon_size, icon_size));
            crate::icons::paint_icon::paint_icon(painter, icon_rect, &icon, theme.foreground);
        }

        if response.clicked() {
            *collapsed = !*collapsed;
            ui.ctx().request_repaint();
        }

        if response.hovered() {
            ui.ctx().set_cursor_icon(egui::CursorIcon::PointingHand);
        }

        response
    }

    fn show_inline(
        self,
        ui: &mut egui::Ui,
        collapsed: &mut bool,
        content: impl FnOnce(&mut egui::Ui),
    ) -> egui::Response {
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());
        let spacing =
            crate::responsive::responsive_ext::ResponsiveExt::responsive_spacing(ui.ctx());
        let is_rail = self.collapsible && *collapsed;

        let effective_width = if is_rail {
            spacing.touch_height
        } else {
            self.width
        };

        let fill = if is_rail {
            theme.background
        } else {
            theme.card
        };
        let margin = egui::Margin {
            left: 8,
            right: 8,
            top: 6,
            bottom: 6,
        };

        let frame = egui::Frame::NONE.fill(fill).inner_margin(margin);

        let inner = frame.show(ui, |inner_ui| {
            inner_ui.set_min_width(effective_width);
            inner_ui.set_max_width(effective_width);
            inner_ui.set_min_height(inner_ui.available_height());

            if self.collapsible && is_rail {
                let _ = inner_ui.vertical(|content_ui| {
                    let _ = content_ui.horizontal(|inner_ui3| {
                        let _ = inner_ui3.with_layout(
                            egui::Layout::right_to_left(egui::Align::Center),
                            |inner_ui4| {
                                _ = Self::collapse_button(inner_ui4, collapsed);
                            },
                        );
                    });
                });
            } else if self.collapsible {
                let available = inner_ui.available_rect_before_wrap();
                let button_size = spacing.touch_height;
                let button_rect = egui::Rect::from_min_size(
                    egui::pos2(available.max.x - button_size, available.min.y),
                    egui::vec2(button_size, button_size),
                );
                let _ = egui::ScrollArea::vertical()
                    .auto_shrink([false; 2])
                    .max_height(available.height())
                    .show(inner_ui, |content_ui| {
                        content(content_ui);
                    });
                let response = inner_ui.allocate_rect(button_rect, egui::Sense::click());
                if inner_ui.is_rect_visible(button_rect) {
                    let painter = inner_ui.painter();
                    let cr =
                        egui::CornerRadius::same(crate::utils::f32_to_u8_clamped(theme.radius));
                    if response.hovered() || response.is_pointer_button_down_on() {
                        let _ = painter.rect_filled(button_rect, cr, theme.muted);
                    }
                    let icon = if *collapsed {
                        crate::icons::lucide_icon::LucideIcon::PanelRightOpen
                    } else {
                        crate::icons::lucide_icon::LucideIcon::PanelRightClose
                    };
                    let icon_rect = egui::Rect::from_center_size(
                        button_rect.center(),
                        egui::vec2(button_size * 0.5, button_size * 0.5),
                    );
                    crate::icons::paint_icon::paint_icon(
                        painter,
                        icon_rect,
                        &icon,
                        theme.foreground,
                    );
                }
                if response.clicked() {
                    *collapsed = !*collapsed;
                    inner_ui.ctx().request_repaint();
                }
                if response.hovered() {
                    inner_ui
                        .ctx()
                        .set_cursor_icon(egui::CursorIcon::PointingHand);
                }
            } else {
                let _ = egui::ScrollArea::vertical().auto_shrink([false; 2]).show(
                    inner_ui,
                    |content_ui| {
                        content(content_ui);
                    },
                );
            }
        });
        _ = ui.painter().vline(
            inner.response.rect.min.x + 0.5,
            inner.response.rect.y_range(),
            egui::Stroke::new(1.0, theme.border),
        );
        inner.response
    }

    fn show_mobile_overlay(
        self,
        ui: &mut egui::Ui,
        collapsed: &mut bool,
        content: impl FnOnce(&mut egui::Ui),
    ) -> egui::Response {
        let ctx = ui.ctx();
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ctx);
        let spacing = crate::responsive::responsive_ext::ResponsiveExt::responsive_spacing(ctx);
        let screen = ctx.input(egui::InputState::viewport_rect);
        let max_allowed_width = (screen.width() - spacing.page_padding * 2.0).max(0.0);
        let panel_width = self.width.min(max_allowed_width);
        let panel_height = screen.height();

        let anim_id = egui::Id::new("sidebar_overlay_anim");
        let anim_t = ctx.animate_bool_with_time(anim_id, !*collapsed, 0.2);

        if anim_t <= 0.0 {
            return ui.allocate_response(egui::Vec2::ZERO, egui::Sense::hover());
        }

        let ease_t = ease_out_cubic(anim_t);

        // Animated backdrop
        let backdrop_alpha = crate::utils::f32_to_u8_clamped(60.0 * ease_t);
        let backdrop_layer =
            egui::LayerId::new(egui::Order::Middle, egui::Id::new("sidebar_backdrop"));
        let _ = ctx.layer_painter(backdrop_layer).rect_filled(
            screen,
            egui::CornerRadius::ZERO,
            egui::Color32::from_black_alpha(backdrop_alpha),
        );

        // Backdrop click to close
        let backdrop_response = egui::Area::new(egui::Id::new("sidebar_backdrop_sense"))
            .order(egui::Order::Middle)
            .anchor(egui::Align2::LEFT_TOP, egui::Vec2::ZERO)
            .show(ctx, |inner_ui| {
                let (_, response) =
                    inner_ui.allocate_exact_size(screen.size(), egui::Sense::click());
                response
            });

        if backdrop_response.inner.clicked() {
            *collapsed = true;
            ctx.request_repaint();
        }

        let slide_offset_x = (1.0 - ease_t) * panel_width;

        let _ = egui::Area::new(egui::Id::new("sidebar_panel"))
            .order(egui::Order::Foreground)
            .anchor(egui::Align2::RIGHT_TOP, egui::vec2(slide_offset_x, 0.0))
            .show(ctx, |inner_ui| {
                let frame = egui::Frame::NONE
                    .fill(theme.card)
                    .inner_margin(egui::Margin {
                        left: 8,
                        right: 8,
                        top: 6,
                        bottom: 6,
                    })
                    .shadow(egui::Shadow {
                        offset: [-4, 0],
                        blur: 12,
                        spread: 0,
                        color: egui::Color32::from_black_alpha(16),
                    });

                let inner = frame.show(inner_ui, |content_ui| {
                    content_ui
                        .set_min_size(egui::vec2(panel_width, (panel_height - 12.0).max(0.0)));
                    content_ui.set_max_width(panel_width);

                    let available = content_ui.available_rect_before_wrap();
                    let button_size = spacing.touch_height;
                    let button_rect = egui::Rect::from_min_size(
                        egui::pos2(available.max.x - button_size, available.min.y),
                        egui::vec2(button_size, button_size),
                    );
                    let _ = egui::ScrollArea::vertical()
                        .auto_shrink([false; 2])
                        .max_height(available.height())
                        .show(content_ui, |inner_ui3| {
                            content(inner_ui3);
                        });
                    let response = content_ui.allocate_rect(button_rect, egui::Sense::click());
                    if content_ui.is_rect_visible(button_rect) {
                        let painter = content_ui.painter();
                        let cr =
                            egui::CornerRadius::same(crate::utils::f32_to_u8_clamped(theme.radius));
                        if response.hovered() || response.is_pointer_button_down_on() {
                            let _ = painter.rect_filled(button_rect, cr, theme.muted);
                        }
                        let icon_size = button_size * 0.5;
                        let icon_rect = egui::Rect::from_center_size(
                            button_rect.center(),
                            egui::vec2(icon_size, icon_size),
                        );
                        crate::icons::paint_icon::paint_icon(
                            painter,
                            icon_rect,
                            &crate::icons::lucide_icon::LucideIcon::X,
                            theme.foreground,
                        );
                    }
                    if response.clicked() {
                        *collapsed = true;
                        ctx.request_repaint();
                    }
                    if response.hovered() {
                        content_ui
                            .ctx()
                            .set_cursor_icon(egui::CursorIcon::PointingHand);
                    }
                });
                _ = inner_ui.painter().vline(
                    inner.response.rect.min.x + 0.5,
                    inner.response.rect.y_range(),
                    egui::Stroke::new(1.0, theme.border),
                );
            });

        ctx.request_repaint();
        ui.allocate_response(egui::Vec2::ZERO, egui::Sense::hover())
    }
}

fn ease_out_cubic(t: f32) -> f32 {
    1.0 - (1.0 - t).powi(3)
}
