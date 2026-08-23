//! Show method for `AlertDialog` — renders a confirmation modal.

/// Result of an alert dialog interaction.
pub enum AlertDialogResult {
    /// Dialog is still open, no action taken.
    Open,
    /// User cancelled.
    Cancelled,
    /// User confirmed the action.
    Confirmed,
}

impl super::widget::AlertDialog {
    /// Shows the alert dialog when `open` is true.
    /// Returns the result of the interaction.
    pub fn show(self, ctx: &egui::Context, open: &mut bool) -> AlertDialogResult {
        if !*open {
            return AlertDialogResult::Open;
        }

        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ctx);
        let spacing = crate::responsive::responsive_ext::ResponsiveExt::responsive_spacing(ctx);
        let mut result = AlertDialogResult::Open;

        // Backdrop
        let screen = ctx.viewport_rect();
        let backdrop_layer =
            egui::LayerId::new(egui::Order::Middle, egui::Id::new("alert_dialog_backdrop"));
        let painter = ctx.layer_painter(backdrop_layer);
        let _ = painter.rect_filled(
            screen,
            egui::CornerRadius::ZERO,
            egui::Color32::from_black_alpha(60),
        );

        // On mobile the alert becomes a bottom sheet; on desktop it stays a
        // centered window.
        let on_mobile = spacing.is_mobile();
        let (anchor, cr) = if on_mobile {
            (
                egui::Align2::CENTER_BOTTOM,
                egui::CornerRadius {
                    nw: crate::utils::f32_to_u8_clamped(theme.radius + 2.0),
                    ne: crate::utils::f32_to_u8_clamped(theme.radius + 2.0),
                    sw: 0,
                    se: 0,
                },
            )
        } else {
            (
                egui::Align2::CENTER_CENTER,
                egui::CornerRadius::same(crate::utils::f32_to_u8_clamped(theme.radius + 2.0)),
            )
        };

        // Frame = content + 2*24 inner margin + 2*1 stroke, so the content
        // width must leave room for the frame to stay inside the screen.
        // Clamp on desktop as well so narrow viewports never overflow.
        let max_panel_width = (screen.width() - 2.0 * spacing.page_padding - 50.0).max(0.0);
        let panel_width = if on_mobile {
            max_panel_width
        } else {
            420.0_f32.clamp(0.0, max_panel_width)
        };

        // Dialog panel
        let _ = egui::Area::new(egui::Id::new("alert_dialog_panel"))
            .order(egui::Order::Foreground)
            .anchor(anchor, egui::Vec2::ZERO)
            .show(ctx, |inner_ui| {
                let frame = egui::Frame::NONE
                    .fill(theme.background)
                    .inner_margin(egui::Margin::same(24))
                    .corner_radius(cr)
                    .stroke(egui::Stroke::new(1.0, theme.border))
                    .shadow(egui::Shadow {
                        offset: [0, 8],
                        blur: 24,
                        spread: 0,
                        color: egui::Color32::from_black_alpha(12),
                    });

                let _ = frame.show(inner_ui, |content_ui| {
                    content_ui.set_max_width(panel_width);
                    if on_mobile {
                        content_ui.set_max_height(
                            (screen.height() - 2.0 * spacing.page_padding - 50.0).max(0.0),
                        );
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
                                result = AlertDialogResult::Cancelled;
                                ctx.request_repaint();
                            }
                        },
                    );

                    let _ = content_ui.label(
                        egui::RichText::new(&self.title)
                            .color(theme.foreground)
                            .size(18.0)
                            .strong(),
                    );

                    content_ui.add_space(4.0);
                    let _ = content_ui.label(
                        egui::RichText::new(&self.description)
                            .color(theme.muted_foreground)
                            .size(14.0),
                    );

                    content_ui.add_space(20.0);

                    // Button row aligned to right
                    let _ = content_ui.with_layout(
                        egui::Layout::right_to_left(egui::Align::Center),
                        |inner_ui3| {
                            // Action button (shows first from right)
                            let action_variant = if self.destructive {
                                crate::tokens::button_variant::ButtonVariant::Destructive
                            } else {
                                crate::tokens::button_variant::ButtonVariant::Default
                            };

                            let action_btn =
                                crate::widgets::button::widget::Button::new(&self.action_text)
                                    .variant(action_variant)
                                    .show(inner_ui3);

                            if action_btn.clicked() {
                                *open = false;
                                result = AlertDialogResult::Confirmed;
                                inner_ui3.ctx().request_repaint();
                            }

                            inner_ui3.add_space(8.0);

                            // Cancel button
                            let cancel_btn =
                                crate::widgets::button::widget::Button::new(&self.cancel_text)
                                    .variant(crate::tokens::button_variant::ButtonVariant::Outline)
                                    .show(inner_ui3);

                            if cancel_btn.clicked() {
                                *open = false;
                                result = AlertDialogResult::Cancelled;
                                inner_ui3.ctx().request_repaint();
                            }
                        },
                    );
                });
            });

        result
    }
}
