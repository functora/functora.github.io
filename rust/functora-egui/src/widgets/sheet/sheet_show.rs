//! Show method for Sheet — renders a slide-in panel overlay with animation.

impl super::widget::Sheet {
    pub fn show(self, ctx: &egui::Context, open: &mut bool, content: impl FnOnce(&mut egui::Ui)) {
        let anim_id = egui::Id::new("sheet_anim");
        let anim_t = ctx.animate_bool_with_time(anim_id, *open, 0.2);

        if anim_t <= 0.0 {
            return;
        }

        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ctx);
        let screen = ctx.input(egui::InputState::viewport_rect);
        let spacing = crate::responsive::responsive_ext::ResponsiveExt::responsive_spacing(ctx);
        let on_mobile = spacing.is_mobile();
        let ease_t = ease_out_cubic(anim_t);

        // Animated backdrop
        let backdrop_alpha = crate::utils::f32_to_u8_clamped(60.0 * ease_t);
        let backdrop_layer =
            egui::LayerId::new(egui::Order::Middle, egui::Id::new("sheet_backdrop"));
        let _ = ctx.layer_painter(backdrop_layer).rect_filled(
            screen,
            egui::CornerRadius::ZERO,
            egui::Color32::from_black_alpha(backdrop_alpha),
        );

        // Backdrop click to close
        let backdrop_response = egui::Area::new(egui::Id::new("sheet_backdrop_sense"))
            .order(egui::Order::Middle)
            .anchor(egui::Align2::LEFT_TOP, egui::Vec2::ZERO)
            .show(ctx, |inner_ui| {
                let (_, response) =
                    inner_ui.allocate_exact_size(screen.size(), egui::Sense::click());
                response
            });

        if backdrop_response.inner.clicked() {
            *open = false;
            ctx.request_repaint();
        }

        // Compute animated slide offset - on mobile Left/Right become bottom sheet for thumb reach
        let is_horizontal = matches!(
            self.side,
            crate::tokens::sheet_side::SheetSide::Left
                | crate::tokens::sheet_side::SheetSide::Right
        );
        let on_mobile_horizontal = on_mobile && is_horizontal;
        let slide_dist = if on_mobile_horizontal {
            400.0
        } else {
            self.width + 48.0
        };
        let (anchor, offset, is_bottom_sheet) = match self.side {
            crate::tokens::sheet_side::SheetSide::Right
            | crate::tokens::sheet_side::SheetSide::Left
                if on_mobile_horizontal =>
            {
                (
                    egui::Align2::LEFT_BOTTOM,
                    egui::vec2(0.0, (1.0 - ease_t) * slide_dist),
                    true,
                )
            }
            crate::tokens::sheet_side::SheetSide::Right => (
                egui::Align2::RIGHT_TOP,
                egui::vec2((1.0 - ease_t) * slide_dist, 0.0),
                false,
            ),
            crate::tokens::sheet_side::SheetSide::Left => (
                egui::Align2::LEFT_TOP,
                egui::vec2(-(1.0 - ease_t) * slide_dist, 0.0),
                false,
            ),
            crate::tokens::sheet_side::SheetSide::Top => (
                egui::Align2::LEFT_TOP,
                egui::vec2(0.0, -(1.0 - ease_t) * slide_dist),
                false,
            ),
            crate::tokens::sheet_side::SheetSide::Bottom => (
                egui::Align2::LEFT_BOTTOM,
                egui::vec2(0.0, (1.0 - ease_t) * slide_dist),
                false,
            ),
        };

        let _ = egui::Area::new(egui::Id::new("sheet_panel"))
            .order(egui::Order::Foreground)
            .anchor(anchor, offset)
            .show(ctx, |inner_ui| {
                let frame = if is_bottom_sheet
                    || matches!(self.side, crate::tokens::sheet_side::SheetSide::Bottom)
                {
                    let cr = egui::CornerRadius {
                        nw: crate::utils::f32_to_u8_clamped(theme.radius + 2.0),
                        ne: crate::utils::f32_to_u8_clamped(theme.radius + 2.0),
                        sw: 0,
                        se: 0,
                    };
                    egui::Frame::NONE
                        .fill(theme.background)
                        .inner_margin(egui::Margin {
                            left: 24,
                            right: 24,
                            top: 16,
                            bottom: 24,
                        })
                        .corner_radius(cr)
                        .stroke(egui::Stroke::new(1.0, theme.border))
                } else if matches!(self.side, crate::tokens::sheet_side::SheetSide::Top) {
                    let cr = egui::CornerRadius {
                        nw: 0,
                        ne: 0,
                        sw: crate::utils::f32_to_u8_clamped(theme.radius + 2.0),
                        se: crate::utils::f32_to_u8_clamped(theme.radius + 2.0),
                    };
                    egui::Frame::NONE
                        .fill(theme.background)
                        .inner_margin(egui::Margin::same(24))
                        .corner_radius(cr)
                        .stroke(egui::Stroke::new(1.0, theme.border))
                } else {
                    egui::Frame::NONE
                        .fill(theme.background)
                        .inner_margin(egui::Margin::same(24))
                        .corner_radius(egui::CornerRadius::ZERO)
                        .stroke(egui::Stroke::new(1.0, theme.border))
                };

                let _ = frame.show(inner_ui, |content_ui| {
                    if is_bottom_sheet {
                        let width = (screen.width() - 50.0).clamp(0.0, 500.0);
                        content_ui.set_min_width(width);
                        content_ui.set_max_width(width);
                    } else if is_horizontal {
                        // Desktop side sheet - full height (inset-y-0)
                        let width = self.width.min((screen.width() - 98.0).max(0.0));
                        let height = screen.height().max(0.0);
                        content_ui.set_min_size(egui::vec2(width, height));
                        content_ui.set_max_width(width);
                    } else {
                        content_ui.set_min_width((screen.width() - 98.0).max(0.0));
                        content_ui
                            .set_max_height(self.width.min((screen.height() - 98.0).max(0.0)));
                    }

                    // Close button
                    let _ = content_ui.with_layout(
                        egui::Layout::right_to_left(egui::Align::TOP),
                        |inner_ui3| {
                            let close_size = 16.0;
                            let (close_rect, close_resp) = inner_ui3.allocate_exact_size(
                                egui::vec2(close_size, close_size),
                                egui::Sense::click(),
                            );
                            if inner_ui3.is_rect_visible(close_rect) {
                                crate::icons::paint_icon::paint_icon(
                                    inner_ui3.painter(),
                                    close_rect,
                                    &crate::icons::lucide_icon::LucideIcon::X,
                                    theme.muted_foreground,
                                );
                            }
                            if close_resp.clicked() {
                                *open = false;
                                ctx.request_repaint();
                            }
                        },
                    );

                    // Title & description
                    if let Some(title) = self.title {
                        let _ = content_ui.label(
                            egui::RichText::new(title)
                                .color(theme.foreground)
                                .size(18.0)
                                .strong(),
                        );
                    }

                    if let Some(desc) = self.description {
                        content_ui.add_space(4.0);
                        let _ = content_ui.label(
                            egui::RichText::new(desc)
                                .color(theme.muted_foreground)
                                .size(14.0),
                        );
                    }

                    content_ui.add_space(16.0);
                    content(content_ui);
                });
            });

        ctx.request_repaint();
    }
}

fn ease_out_cubic(t: f32) -> f32 {
    1.0 - (1.0 - t).powi(3)
}
