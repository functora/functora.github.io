//! Widget trait implementation for Kbd.

impl egui::Widget for super::widget::Kbd {
    fn ui(self, ui: &mut egui::Ui) -> egui::Response {
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());
        let fg = theme.foreground;
        let fill =
            crate::paint::interpolate_color::interpolate_color(theme.background, theme.muted, 0.75);

        let h_padding: f32 = 6.0;
        let v_padding: f32 = 2.0;
        let galley = crate::text::layout(
            ui,
            self.text,
            egui::FontId::monospace(11.0),
            fg,
            (ui.available_width() - h_padding * 2.0).max(0.0),
            None,
        );
        let desired = egui::vec2(
            galley.size().x + h_padding * 2.0,
            galley.size().y + v_padding * 2.0,
        );

        let (rect, mut response) = ui.allocate_exact_size(desired, egui::Sense::hover());
        response.set_intrinsic_size(egui::vec2(
            galley.intrinsic_size().x + h_padding * 2.0,
            galley.intrinsic_size().y.max(galley.size().y) + v_padding * 2.0,
        ));

        if ui.is_rect_visible(rect) {
            let painter = ui.painter();
            let cr = egui::CornerRadius::same(6);

            let _ = painter.rect_filled(rect, cr, fill);
            let _ = painter.rect_stroke(
                rect,
                cr,
                egui::Stroke::new(1.0, theme.input),
                egui::epaint::StrokeKind::Inside,
            );

            let text_pos = egui::pos2(
                rect.center().x - galley.size().x / 2.0,
                rect.center().y - galley.size().y / 2.0,
            );
            painter.galley(text_pos, galley, fg);
        }

        response
    }
}
