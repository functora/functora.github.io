//! Show method for Dialog — renders a modal overlay.

impl super::widget::Dialog {
    /// Shows the dialog when `open` is true. Content closure receives a `&mut Ui`.
    pub fn show(self, ctx: &egui::Context, open: &mut bool, content: impl FnOnce(&mut egui::Ui)) {
        if !*open {
            return;
        }

        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ctx);
        let spacing = crate::responsive::responsive_ext::ResponsiveExt::responsive_spacing(ctx);

        // Backdrop
        let screen = ctx.input(egui::InputState::viewport_rect);
        let backdrop_response = crate::widgets::overlay_common::paint_backdrop(
            ctx,
            "dialog_backdrop",
            crate::widgets::overlay_common::BACKDROP_ALPHA,
        );

        if backdrop_response.clicked() {
            *open = false;
            ctx.request_repaint();
            return;
        }

        // On mobile the dialog becomes a bottom sheet with side margins;
        // on desktop it stays a centered window.
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
        // Clamp on desktop as well so narrow viewports (e.g. mobile
        // emulation reporting desktop due to DPI) never overflow.
        let max_panel_width = (screen.width() - 2.0 * spacing.page_padding - 50.0).max(0.0);
        let panel_width = if on_mobile {
            max_panel_width
        } else {
            self.width.clamp(0.0, max_panel_width)
        };

        let _ = egui::Area::new(egui::Id::new("dialog_panel"))
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

                    // Close button
                    crate::widgets::overlay_common::close_button(content_ui, ctx, &theme, open);

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
                    let _ = content_ui.with_layout(
                        egui::Layout::from_main_dir_and_cross_align(
                            egui::Direction::TopDown,
                            egui::Align::Min,
                        )
                        .with_main_align(egui::Align::Min),
                        |ui| {
                            content(ui);
                        },
                    );
                });
            });
    }
}
