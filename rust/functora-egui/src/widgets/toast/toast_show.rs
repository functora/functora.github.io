//! Show method for Toast -- renders toast notifications in bottom-right.

impl super::toast_state::ToastState {
    /// Shows all active toasts. Call this once per frame from your top-level UI.
    pub fn show(&mut self, ctx: &egui::Context) {
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ctx);
        let current_time = ctx.input(|i| i.time);
        self.cleanup(current_time);

        if self.toasts.is_empty() {
            return;
        }

        let cr = egui::CornerRadius::same(crate::utils::f32_to_u8_clamped(theme.radius + 2.0));
        let viewport_width = ctx.viewport_rect().width();
        let is_narrow = viewport_width > 0.0 && viewport_width < 400.0;
        let side_margin: f32 = 16.0;
        let max_outer_width: f32 = 388.0;
        let outer_width = if is_narrow {
            (viewport_width - side_margin * 2.0).clamp(200.0, max_outer_width)
        } else {
            max_outer_width
        };
        let toast_width: f32 = (outer_width - 32.0).max(100.0);
        let spacing: f32 = 8.0;

        let mut dismissed: Vec<usize> = Vec::new();

        let _ = egui::Area::new(egui::Id::new("toast_stack"))
            .order(egui::Order::Foreground)
            .anchor(egui::Align2::RIGHT_BOTTOM, egui::vec2(-16.0, -16.0))
            .fade_in(false)
            .show(ctx, |stack_ui| {
                stack_ui.spacing_mut().item_spacing.y = spacing;
                stack_ui.set_width(outer_width);
                for (idx, toast) in self.toasts.iter().enumerate().rev() {
                    let (border_color, accent) = toast.variant.semantic().map_or(
                        (theme.border, theme.foreground),
                        |semantic| {
                            let color = semantic.color(&theme);
                            (color, color)
                        },
                    );

                    let frame = egui::Frame::NONE
                        .fill(theme.background)
                        .inner_margin(egui::Margin::same(16))
                        .corner_radius(cr)
                        .stroke(egui::Stroke::new(1.0, border_color));

                    let _ = frame.show(stack_ui, |content_ui| {
                        content_ui.set_min_width(toast_width);
                        content_ui.set_max_width(toast_width);

                        let _ = content_ui.horizontal(|inner_ui3| {
                            let close_size = 14.0;
                            let gap = 8.0;
                            let title_width = (toast_width - close_size - gap).max(50.0);
                            let title_galley = egui::WidgetText::from(
                                egui::RichText::new(&toast.title)
                                    .color(accent)
                                    .size(14.0)
                                    .strong(),
                            )
                            .into_galley(
                                inner_ui3,
                                Some(egui::TextWrapMode::Wrap),
                                title_width,
                                egui::TextStyle::Body,
                            );
                            let row_height = title_galley.size().y.max(close_size);
                            let (row_rect, _) = inner_ui3.allocate_exact_size(
                                egui::vec2(toast_width, row_height),
                                egui::Sense::hover(),
                            );
                            inner_ui3
                                .painter()
                                .galley(row_rect.min, title_galley, accent);

                            let close_rect = egui::Rect::from_center_size(
                                egui::pos2(
                                    row_rect.max.x - close_size / 2.0,
                                    row_rect.min.y + close_size / 2.0 + 1.0,
                                ),
                                egui::vec2(close_size, close_size),
                            );
                            let close_resp =
                                inner_ui3.allocate_rect(close_rect, egui::Sense::click());
                            if inner_ui3.is_rect_visible(close_rect) {
                                let color = if close_resp.hovered() {
                                    theme.foreground
                                } else {
                                    theme.muted_foreground
                                };
                                crate::icons::paint_icon::paint_icon(
                                    inner_ui3.painter(),
                                    close_rect,
                                    &crate::icons::lucide_icon::LucideIcon::X,
                                    color,
                                );
                            }
                            if close_resp.clicked() {
                                dismissed.push(idx);
                            }
                        });

                        if let Some(desc) = &toast.description {
                            content_ui.add_space(2.0);
                            let _ = content_ui.label(
                                egui::RichText::new(desc)
                                    .color(theme.muted_foreground)
                                    .size(13.0),
                            );
                        }
                    });
                }
            });

        for idx in dismissed.into_iter().rev() {
            let _ = self.toasts.remove(idx);
        }

        ctx.request_repaint();
    }
}
