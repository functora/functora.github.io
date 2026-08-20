//! Show method for Sidebar -- renders a fixed sidebar panel.

impl super::sidebar::Sidebar {
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
    /// menu on mobile, a panel toggle on desktop. Place it in a top bar next
    /// to the sidebar.
    pub fn toggle_button(ui: &mut egui::Ui, collapsed: &mut bool) -> egui::Response {
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());
        let spacing =
            crate::responsive::responsive_ext::ResponsiveExt::responsive_spacing(ui.ctx());
        let on_mobile = spacing.is_mobile();
        let icon = if on_mobile {
            crate::icons::lucide_icon::LucideIcon::Menu
        } else if *collapsed {
            crate::icons::lucide_icon::LucideIcon::PanelLeftOpen
        } else {
            crate::icons::lucide_icon::LucideIcon::PanelLeftClose
        };

        let size = spacing.touch_height;
        let icon_size = size * 0.5;
        let (rect, response) = ui.allocate_exact_size(egui::vec2(size, size), egui::Sense::click());

        if ui.is_rect_visible(rect) {
            let painter = ui.painter();
            let cr = egui::CornerRadius::same(theme.radius.round() as u8);
            if response.hovered() || response.is_pointer_button_down_on() {
                painter.rect_filled(rect, cr, theme.muted);
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

        let effective_width = if self.collapsible && *collapsed {
            48.0
        } else {
            self.width
        };

        let frame = egui::Frame::NONE
            .fill(theme.card)
            .inner_margin(egui::Margin {
                left: 12,
                right: 12,
                top: 12,
                bottom: 12,
            })
            .stroke(egui::Stroke::new(1.0, theme.border));

        frame
            .show(ui, |ui| {
                ui.set_min_width(effective_width);
                ui.set_max_width(effective_width);
                ui.set_min_height(ui.available_height());

                ui.vertical(|ui| {
                    if self.collapsible {
                        let toggle_icon = if *collapsed {
                            crate::icons::lucide_icon::LucideIcon::PanelLeftOpen
                        } else {
                            crate::icons::lucide_icon::LucideIcon::PanelLeftClose
                        };
                        let icon_size: f32 = 16.0;
                        let (icon_rect, toggle_resp) = ui.allocate_exact_size(
                            egui::vec2(icon_size, icon_size),
                            egui::Sense::click(),
                        );
                        if ui.is_rect_visible(icon_rect) {
                            crate::icons::paint_icon::paint_icon(
                                ui.painter(),
                                icon_rect,
                                &toggle_icon,
                                theme.muted_foreground,
                            );
                        }
                        if toggle_resp.clicked() {
                            *collapsed = !*collapsed;
                            ui.ctx().request_repaint();
                        }
                        ui.add_space(8.0);
                    }

                    if !*collapsed || !self.collapsible {
                        content(ui);
                    }
                });
            })
            .response
    }

    fn show_mobile_overlay(
        self,
        ui: &mut egui::Ui,
        collapsed: &mut bool,
        content: impl FnOnce(&mut egui::Ui),
    ) -> egui::Response {
        let ctx = ui.ctx();
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ctx);
        let screen = ctx.input(|i| i.viewport_rect());
        // Frame = content + 2*12 inner margin + 2*1 stroke; keep it inside the
        // screen, full height on mobile.
        let panel_width = self.width.min((screen.width() - 26.0).max(0.0));
        let panel_height = (screen.height() - 26.0).max(0.0);

        let anim_id = egui::Id::new("sidebar_overlay_anim");
        let anim_t = ctx.animate_bool_with_time(anim_id, !*collapsed, 0.2);

        if anim_t <= 0.0 {
            return ui.allocate_response(egui::Vec2::ZERO, egui::Sense::hover());
        }

        let ease_t = ease_out_cubic(anim_t);

        // Animated backdrop
        let backdrop_alpha = (60.0 * ease_t) as u8;
        let backdrop_layer =
            egui::LayerId::new(egui::Order::Middle, egui::Id::new("sidebar_backdrop"));
        ctx.layer_painter(backdrop_layer).rect_filled(
            screen,
            egui::CornerRadius::ZERO,
            egui::Color32::from_black_alpha(backdrop_alpha),
        );

        // Backdrop click to close
        let backdrop_response = egui::Area::new(egui::Id::new("sidebar_backdrop_sense"))
            .order(egui::Order::Middle)
            .anchor(egui::Align2::LEFT_TOP, egui::Vec2::ZERO)
            .show(ctx, |ui| {
                let (_, response) = ui.allocate_exact_size(screen.size(), egui::Sense::click());
                response
            });

        if backdrop_response.inner.clicked() {
            *collapsed = true;
            ctx.request_repaint();
        }

        let slide_offset_x = -(1.0 - ease_t) * panel_width;

        egui::Area::new(egui::Id::new("sidebar_panel"))
            .order(egui::Order::Foreground)
            .anchor(egui::Align2::LEFT_TOP, egui::vec2(slide_offset_x, 0.0))
            .show(ctx, |ui| {
                let frame = egui::Frame::NONE
                    .fill(theme.card)
                    .inner_margin(egui::Margin {
                        left: 12,
                        right: 12,
                        top: 12,
                        bottom: 12,
                    })
                    .stroke(egui::Stroke::new(1.0, theme.border));

                frame.show(ui, |ui| {
                    ui.set_min_size(egui::vec2(panel_width, panel_height));
                    ui.set_max_width(panel_width);

                    ui.vertical(|ui| {
                        // Close toggle
                        let icon_size: f32 = 16.0;
                        let (icon_rect, toggle_resp) = ui.allocate_exact_size(
                            egui::vec2(icon_size, icon_size),
                            egui::Sense::click(),
                        );
                        if ui.is_rect_visible(icon_rect) {
                            crate::icons::paint_icon::paint_icon(
                                ui.painter(),
                                icon_rect,
                                &crate::icons::lucide_icon::LucideIcon::X,
                                theme.muted_foreground,
                            );
                        }
                        if toggle_resp.clicked() {
                            *collapsed = true;
                            ctx.request_repaint();
                        }
                        ui.add_space(8.0);

                        content(ui);
                    });
                });
            });

        ctx.request_repaint();
        ui.allocate_response(egui::Vec2::ZERO, egui::Sense::hover())
    }
}

fn ease_out_cubic(t: f32) -> f32 {
    1.0 - (1.0 - t).powi(3)
}
