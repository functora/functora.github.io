//! Show method for Card — renders content inside a bordered container.

impl super::widget::Card {
    /// Renders the card container and calls `content` inside it.
    pub fn show(self, ui: &mut egui::Ui, content: impl FnOnce(&mut egui::Ui)) -> egui::Response {
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());

        let ring_color = egui::Color32::from_rgba_unmultiplied(
            theme.foreground.r(),
            theme.foreground.g(),
            theme.foreground.b(),
            26, // ~10% of foreground
        );
        let cr = theme.radius + 2.0; // rounded-xl = radius + 2

        let frame = egui::Frame::NONE
            .fill(theme.card)
            .inner_margin(egui::Margin {
                left: 16,
                right: 16,
                top: 16,
                bottom: 16,
            })
            .corner_radius(egui::CornerRadius::same(crate::utils::f32_to_u8_clamped(
                cr,
            )))
            .stroke(egui::Stroke::new(1.0, ring_color));

        let heading = self.heading;
        frame
            .show(ui, |inner_ui| {
                if let Some(heading_val) = heading {
                    let _ = inner_ui.label(
                        egui::RichText::new(heading_val)
                            .color(theme.foreground)
                            .size(16.0)
                            .strong(),
                    );
                    inner_ui.add_space(12.0);
                }
                content(inner_ui);
            })
            .response
    }
}
