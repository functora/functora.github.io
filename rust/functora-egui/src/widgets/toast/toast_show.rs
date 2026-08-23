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
        let toast_width: f32 = 356.0;
        let spacing: f32 = 8.0;

        let mut dismissed: Vec<usize> = Vec::new();

        for (idx, toast) in self.toasts.iter().enumerate() {
            let offset_y = -(crate::utils::usize_to_f32(idx) * (72.0 + spacing)) - 16.0;

            let _ = egui::Area::new(egui::Id::new("toast").with(idx))
                .order(egui::Order::Foreground)
                .anchor(egui::Align2::RIGHT_BOTTOM, egui::vec2(-16.0, offset_y))
                .show(ctx, |inner_ui| {
                    let (border_color, accent) = match toast.variant {
                        crate::tokens::toast_variant::ToastVariant::Default => {
                            (theme.border, theme.foreground)
                        }
                        crate::tokens::toast_variant::ToastVariant::Success => {
                            let c = egui::Color32::from_rgb(34, 197, 94);
                            (c, c)
                        }
                        crate::tokens::toast_variant::ToastVariant::Error => {
                            (theme.destructive, theme.destructive)
                        }
                        crate::tokens::toast_variant::ToastVariant::Warning => {
                            let c = egui::Color32::from_rgb(234, 179, 8);
                            (c, c)
                        }
                        crate::tokens::toast_variant::ToastVariant::Info => {
                            let c = egui::Color32::from_rgb(59, 130, 246);
                            (c, c)
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

                        // Title row: title on left, close X on right
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
        }

        // Remove dismissed toasts (reverse order to keep indices valid)
        for idx in dismissed.into_iter().rev() {
            let _ = self.toasts.remove(idx);
        }

        ctx.request_repaint();
    }
}
