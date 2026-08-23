//! Show method for `HoverCard` — renders a popup on hover.

impl super::widget::HoverCard {
    /// Shows a hover card when `trigger_response` is hovered.
    pub fn show(self, trigger_response: &egui::Response, content: impl FnOnce(&mut egui::Ui)) {
        let theme =
            crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(&trigger_response.ctx);
        let cr = crate::utils::f32_to_u8_clamped(theme.radius + 2.0);

        let themed_frame = egui::Frame::NONE
            .fill(theme.popover)
            .inner_margin(egui::Margin::same(16))
            .corner_radius(egui::CornerRadius::same(cr))
            .stroke(egui::Stroke::new(1.0, theme.border))
            .shadow(egui::Shadow {
                offset: [0, 4],
                blur: 12,
                spread: 0,
                color: egui::Color32::from_black_alpha(8),
            });

        let mut tooltip = egui::Tooltip::for_enabled(trigger_response);
        tooltip.popup = tooltip.popup.at_pointer().gap(12.0).frame(themed_frame);

        let _ = tooltip.show(|popup_ui| {
            popup_ui.style_mut().visuals.override_text_color = Some(theme.popover_foreground);
            let spacing = crate::responsive::responsive_ext::ResponsiveExt::responsive_spacing(
                popup_ui.ctx(),
            );
            let screen_w = popup_ui.ctx().input(|i| i.viewport_rect().width());
            let w = if spacing.is_mobile() {
                (screen_w - 2.0 * spacing.page_padding - 16.0).max(200.0)
            } else {
                self.width.min(screen_w * 0.6).max(200.0)
            };
            popup_ui.set_min_width(w);
            popup_ui.set_max_width((screen_w - 2.0 * spacing.page_padding).max(w));
            content(popup_ui);
        });
    }
}
