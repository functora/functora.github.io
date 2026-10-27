//! Widget trait implementation for Typography.

impl egui::Widget for super::widget::Typography {
    fn ui(self, ui: &mut egui::Ui) -> egui::Response {
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());

        let galley = crate::text::layout(
            ui,
            self.text,
            egui::FontId::proportional(self.variant.font_size()),
            self.variant.foreground(&theme),
            ui.available_width(),
            None,
        );
        let mut response = ui.label(galley.clone());
        let intrinsic = galley.intrinsic_size();
        response.set_intrinsic_size(egui::vec2(
            intrinsic.x,
            intrinsic.y.max(response.rect.height()),
        ));
        response
    }
}
