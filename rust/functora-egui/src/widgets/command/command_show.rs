//! Show method for Command — renders a command palette overlay.

impl super::widget::Command {
    /// Shows the command palette when `open` is true.
    /// `search` holds the filter text. Returns the index of selected command if any.
    pub fn show(self, ctx: &egui::Context, open: &mut bool, search: &mut String) -> Option<usize> {
        if !*open {
            return None;
        }

        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ctx);
        let mut selected = None;

        // Backdrop
        let screen = ctx.input(egui::InputState::viewport_rect);
        let backdrop_layer =
            egui::LayerId::new(egui::Order::Middle, egui::Id::new("command_backdrop"));
        let _ = ctx.layer_painter(backdrop_layer).rect_filled(
            screen,
            egui::CornerRadius::ZERO,
            egui::Color32::from_black_alpha(60),
        );

        // Backdrop click to close
        let backdrop_resp = egui::Area::new(egui::Id::new("command_backdrop_sense"))
            .order(egui::Order::Middle)
            .anchor(egui::Align2::LEFT_TOP, egui::Vec2::ZERO)
            .show(ctx, |inner_ui| {
                let (_, response) =
                    inner_ui.allocate_exact_size(screen.size(), egui::Sense::click());
                response
            });

        if backdrop_resp.inner.clicked() {
            *open = false;
            search.clear();
            ctx.request_repaint();
            return None;
        }

        // Escape to close
        if ctx.input(|i| i.key_pressed(egui::Key::Escape)) {
            *open = false;
            search.clear();
            ctx.request_repaint();
            return None;
        }

        let cr = crate::utils::f32_to_u8_clamped(theme.radius + 2.0);
        let spacing = crate::responsive::responsive_ext::ResponsiveExt::responsive_spacing(ctx);
        let is_mobile = spacing.is_mobile();
        let screen_w = screen.width();
        let screen_h = screen.height();

        let (anchor, offset) = if is_mobile {
            (egui::Align2::CENTER_TOP, egui::vec2(0.0, 24.0))
        } else {
            (egui::Align2::CENTER_CENTER, egui::vec2(0.0, -60.0))
        };

        let _ = egui::Area::new(egui::Id::new("command_palette"))
            .order(egui::Order::Foreground)
            .anchor(anchor, offset)
            .show(ctx, |inner_ui| {
                let frame = egui::Frame::NONE
                    .fill(theme.popover)
                    .inner_margin(egui::Margin::same(0))
                    .corner_radius(egui::CornerRadius::same(cr))
                    .stroke(egui::Stroke::new(1.0, theme.border))
                    .shadow(egui::Shadow {
                        offset: [0, 8],
                        blur: 24,
                        spread: 0,
                        color: egui::Color32::from_black_alpha(12),
                    });

                let _ = frame.show(inner_ui, |content_ui| {
                    let mut content_w: f32 = 0.0;
                    for item in &self.items {
                        let gw = content_ui
                            .painter()
                            .layout_no_wrap(
                                item.group.clone(),
                                egui::FontId::proportional(12.0),
                                egui::Color32::PLACEHOLDER,
                            )
                            .size()
                            .x;
                        let lw = content_ui
                            .painter()
                            .layout_no_wrap(
                                item.label.clone(),
                                egui::FontId::proportional(14.0),
                                egui::Color32::PLACEHOLDER,
                            )
                            .size()
                            .x;
                        content_w = content_w.max(gw + 16.0 + 6.0).max(lw + 16.0 + 6.0);
                    }
                    let placeholder_w = content_ui
                        .painter()
                        .layout_no_wrap(
                            self.placeholder.clone(),
                            egui::FontId::proportional(14.0),
                            egui::Color32::PLACEHOLDER,
                        )
                        .size()
                        .x;
                    content_w = content_w.max(placeholder_w);
                    content_w += 48.0;

                    let screen_avail_w = if is_mobile {
                        (screen_w - 2.0 * spacing.page_padding - 16.0).clamp(200.0, 640.0)
                    } else {
                        (screen_w * 0.5).clamp(320.0, 640.0)
                    };
                    let palette_width = if is_mobile {
                        screen_avail_w
                    } else {
                        content_w.max(320.0).min(screen_avail_w)
                    };
                    content_ui.set_min_width(palette_width);
                    content_ui.set_max_width(palette_width);

                    // Search input
                    let input_frame = egui::Frame::NONE.inner_margin(egui::Margin {
                        left: 12,
                        right: 12,
                        top: 12,
                        bottom: 12,
                    });

                    let _ = input_frame.show(content_ui, |inner_ui3| {
                        let input_resp = crate::widgets::input::widget::Input::new(search)
                            .placeholder(&self.placeholder)
                            .desired_width(inner_ui3.available_width())
                            .show(inner_ui3);
                        input_resp.request_focus();
                    });

                    // Divider
                    let avail = content_ui.available_rect_before_wrap();
                    let _ = content_ui.painter().hline(
                        avail.min.x..=avail.max.x,
                        avail.min.y,
                        egui::Stroke::new(1.0, theme.border),
                    );
                    content_ui.add_space(1.0);

                    // Command list
                    let query = search.to_lowercase();
                    let results_frame = egui::Frame::NONE.inner_margin(egui::Margin::same(8));

                    let max_h = if is_mobile {
                        (screen_h * 0.55).clamp(200.0, 380.0)
                    } else {
                        320.0_f32.min(screen_h * 0.6)
                    };
                    let _ = results_frame.show(content_ui, |inner_ui3| {
                        let _ = egui::ScrollArea::vertical()
                            .max_height(max_h)
                            .auto_shrink([false; 2])
                            .show(inner_ui3, |inner_ui4| {
                                let mut current_group = String::new();
                                let mut any_shown = false;

                                for (idx, item) in self.items.iter().enumerate() {
                                    if !query.is_empty()
                                        && !item.label.to_lowercase().contains(&query)
                                        && !item.group.to_lowercase().contains(&query)
                                    {
                                        continue;
                                    }

                                    any_shown = true;

                                    if item.group != current_group {
                                        if item.group == "Overview" {
                                            inner_ui4.add_space(8.0);
                                        } else {
                                            if !current_group.is_empty() {
                                                inner_ui4.add_space(8.0);
                                            }
                                            let _ = crate::widgets::separator::widget::Separator::horizontal()
                                                .text(&item.group)
                                                .icon(item.group_icon)
                                                .show(inner_ui4);
                                            inner_ui4.add_space(8.0);
                                        }
                                        current_group.clone_from(&item.group);
                                    }

                                    let galley = inner_ui4.painter().layout_no_wrap(
                                        item.label.clone(),
                                        egui::FontId::proportional(14.0),
                                        theme.popover_foreground,
                                    );
                                    let desired = egui::vec2(
                                        inner_ui4.available_width(),
                                        galley.size().y + 8.0,
                                    );
                                    let (rect, response_raw) = inner_ui4
                                        .allocate_exact_size(desired, egui::Sense::click());
                                    let response = response_raw
                                        .on_hover_cursor(egui::CursorIcon::PointingHand);

                                    let hovered = response.hovered();
                                    if hovered {
                                        let _ = inner_ui4.painter().rect_filled(
                                            rect,
                                            egui::CornerRadius::same(
                                                crate::utils::f32_to_u8_clamped(theme.radius),
                                            ),
                                            theme.accent,
                                        );
                                    }

                                    if inner_ui4.is_rect_visible(rect) {
                                        let icon_size = 14.0;
                                        let fg = if hovered {
                                            theme.accent_foreground
                                        } else {
                                            theme.popover_foreground
                                        };
                                        let icon_rect = egui::Rect::from_min_size(
                                            egui::pos2(
                                                rect.min.x + 8.0,
                                                rect.center().y - icon_size / 2.0,
                                            ),
                                            egui::vec2(icon_size, icon_size),
                                        );
                                        crate::icons::paint_icon::paint_icon(
                                            inner_ui4.painter(),
                                            icon_rect,
                                            &item.icon,
                                            fg,
                                        );
                                        let text_galley = inner_ui4.painter().layout_no_wrap(
                                            item.label.clone(),
                                            egui::FontId::proportional(14.0),
                                            fg,
                                        );
                                        inner_ui4.painter().galley(
                                            egui::pos2(
                                                rect.min.x + 8.0 + icon_size + 6.0,
                                                rect.center().y - text_galley.size().y / 2.0,
                                            ),
                                            text_galley,
                                            fg,
                                        );
                                    }

                                    if response.clicked() {
                                        selected = Some(idx);
                                        *open = false;
                                        search.clear();
                                        ctx.request_repaint();
                                    }
                                }

                                if !any_shown {
                                    let _ = inner_ui4.label(
                                        egui::RichText::new("No results found.")
                                            .color(theme.muted_foreground)
                                            .size(14.0),
                                    );
                                }
                            });
                    });
                });
            });

        selected
    }
}
