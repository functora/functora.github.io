//! Show method for `ScrollArea` — renders a themed scrollable region.

impl super::widget::ScrollArea {
    pub fn show(self, ui: &mut egui::Ui, content: impl FnOnce(&mut egui::Ui)) -> egui::Response {
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());
        let cr = egui::CornerRadius::same(crate::utils::f32_to_u8_clamped(theme.radius));

        let frame = egui::Frame::NONE
            .fill(egui::Color32::TRANSPARENT)
            .corner_radius(cr)
            .stroke(egui::Stroke::new(1.0, theme.border));

        frame
            .show(ui, |inner_ui| {
                if self.horizontal {
                    let _ = egui::ScrollArea::horizontal()
                        .max_height(self.max_height)
                        .show(inner_ui, content);
                } else {
                    let _ = egui::ScrollArea::vertical()
                        .max_height(self.max_height)
                        .show(inner_ui, content);
                }
            })
            .response
    }
}
