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
        let mut y_offset: f32 = -16.0;

        for (idx, toast) in self.toasts.iter().enumerate() {
            let id = egui::Id::new("toast").with(toast.id);
            let estimated_height = if toast.description.is_some() {
                72.0
            } else {
                52.0
            };
            let offset_y = y_offset;

            let _ = egui::Area::new(id)
                .order(egui::Order::Foreground)
                .anchor(egui::Align2::RIGHT_BOTTOM, egui::vec2(-16.0, offset_y))
                .fade_in(false)
                .show(ctx, |inner_ui| {
                    let (border_color, accent) = match toast.variant {
                        crate::tokens::toast_variant::ToastVariant::Default => {
                            (theme.border, theme.foreground)
                        }
                        crate::tokens::toast_variant::ToastVariant::Success => {
                            (theme.success, theme.success)
                        }
                        crate::tokens::toast_variant::ToastVariant::Error => {
                            (theme.destructive, theme.destructive)
                        }
                        crate::tokens::toast_variant::ToastVariant::Warning => {
                            (theme.warning, theme.warning)
                        }
                        crate::tokens::toast_variant::ToastVariant::Info => {
                            (theme.info, theme.info)
                        }
                    };

                    let frame = egui::Frame::NONE
                        .fill(theme.background)
                        .inner_margin(egui::Margin::same(16))
                        .corner_radius(cr)
                        .stroke(egui::Stroke::new(1.0, border_color));

                    let _ = frame.show(inner_ui, |content_ui| {
                        content_ui.set_min_width(toast_width);
                        content_ui.set_max_width(toast_width);

                        let _ = content_ui.horizontal(|inner_ui3| {
                            let _ = inner_ui3.label(
                                egui::RichText::new(&toast.title)
                                    .color(accent)
                                    .size(14.0)
                                    .strong(),
                            );

                            let _ = inner_ui3.with_layout(
                                egui::Layout::right_to_left(egui::Align::Center),
                                |inner_ui4| {
                                    let close_size = 14.0;
                                    let (close_rect, close_resp) = inner_ui4.allocate_exact_size(
                                        egui::vec2(close_size, close_size),
                                        egui::Sense::click(),
                                    );
                                    if inner_ui4.is_rect_visible(close_rect) {
                                        let color = if close_resp.hovered() {
                                            theme.foreground
                                        } else {
                                            theme.muted_foreground
                                        };
                                        crate::icons::paint_icon::paint_icon(
                                            inner_ui4.painter(),
                                            close_rect,
                                            &crate::icons::lucide_icon::LucideIcon::X,
                                            color,
                                        );
                                    }
                                    if close_resp.clicked() {
                                        dismissed.push(idx);
                                    }
                                },
                            );
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
                });

            y_offset -= estimated_height + spacing;
        }

        for idx in dismissed.into_iter().rev() {
            let _ = self.toasts.remove(idx);
        }

        ctx.request_repaint();
    }
}
