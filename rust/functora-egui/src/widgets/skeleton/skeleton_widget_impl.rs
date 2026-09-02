//! Widget trait implementation for Skeleton.

impl egui::Widget for super::widget::Skeleton {
    fn ui(self, ui: &mut egui::Ui) -> egui::Response {
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());

        let desired = if self.circle {
            egui::vec2(self.height, self.height)
        } else {
            egui::vec2(self.width, self.height)
        };

        let (rect, response) = ui.allocate_exact_size(desired, egui::Sense::hover());

        if ui.is_rect_visible(rect) {
            let painter = ui.painter();
            let t = crate::utils::f64_to_f32(ui.ctx().input(|i| i.time));
            let pulse = 0.5 + 0.5 * (t * 2.0).sin(); // 0..1 pulse
            let alpha = crate::utils::f32_to_u8_clamped(26.0 + pulse * 26.0); // 10-20% opacity range

            let color = egui::Color32::from_rgba_unmultiplied(
                theme.primary.r(),
                theme.primary.g(),
                theme.primary.b(),
                alpha,
            );

            let cr = if self.circle {
                egui::CornerRadius::same(255) // rounded-full
            } else {
                egui::CornerRadius::same(crate::utils::f32_to_u8_clamped(theme.radius))
            };

            let _ = painter.rect_filled(rect, cr, color);
            ui.ctx().request_repaint(); // Keep animating
        }

        response
    }
}
