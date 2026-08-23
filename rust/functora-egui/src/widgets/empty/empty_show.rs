//! Show method for Empty — renders dashed-border container.

impl super::widget::Empty {
    /// Renders a dashed-border container with content inside.
    pub fn show(ui: &mut egui::Ui, content: impl FnOnce(&mut egui::Ui)) -> egui::InnerResponse<()> {
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());
        let cr = crate::utils::f32_to_u8_clamped(theme.radius + 2.0); // rounded-xl

        let frame = egui::Frame::NONE
            .inner_margin(egui::Margin::same(24)) // p-6
            .corner_radius(egui::CornerRadius::same(cr));

        let result = frame.show(ui, |inner_ui| {
            let _ = inner_ui.vertical_centered(|content_ui| {
                content(content_ui);
            });
        });

        // Paint dashed border over the frame
        let rect = result.response.rect;
        if ui.is_rect_visible(rect) {
            crate::paint::paint_dashed_rect::paint_dashed_rect(
                ui.painter(),
                rect,
                theme.radius + 2.0,
                egui::Stroke::new(1.0, theme.border),
                6.0,
                4.0,
            );
        }

        egui::InnerResponse::new((), result.response)
    }
}
