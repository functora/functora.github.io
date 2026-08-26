//! Widget trait implementation for Textarea.

use crate::responsive::responsive_ext::ResponsiveExt;

impl egui::Widget for super::widget::Textarea<'_> {
    fn ui(self, ui: &mut egui::Ui) -> egui::Response {
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());

        let h_padding: f32 = 10.0; // px-2.5
        let v_padding: f32 = 8.0; // py-2
        let width = self.desired_width.unwrap_or_else(|| {
            if ui.on_mobile() {
                ui.available_width()
            } else {
                ui.available_width().min(240.0)
            }
        });
        let corner_radius = theme.radius;
        let cr = egui::CornerRadius::same(crate::utils::f32_to_u8_clamped(corner_radius));

        let desired = egui::vec2(width, self.min_height);
        let (outer_rect, outer_response) = ui.allocate_exact_size(desired, egui::Sense::click());
        let outer_hovered = outer_response.hovered() || ui.rect_contains_pointer(outer_rect);

        // Background and border
        let mut bg =
            crate::paint::interpolate_color::interpolate_color(theme.background, theme.muted, 0.4);
        if outer_hovered {
            bg = crate::paint::interpolate_color::interpolate_color(bg, theme.accent, 0.35);
        }
        let _ = ui.painter().rect_filled(outer_rect, cr, bg);
        let _ = ui.painter().rect_stroke(
            outer_rect,
            cr,
            egui::Stroke::new(
                1.0,
                if outer_hovered {
                    theme.input
                } else {
                    theme.border
                },
            ),
            egui::epaint::StrokeKind::Inside,
        );

        // Inner area with scroll for overflow
        let inner_rect = outer_rect.shrink2(egui::vec2(h_padding, v_padding));
        let mut child_ui = ui.new_child(
            egui::UiBuilder::new()
                .max_rect(inner_rect)
                .layout(egui::Layout::top_down(egui::Align::LEFT)),
        );

        let scroll_resp = egui::ScrollArea::vertical()
            .max_height(inner_rect.height())
            .show(&mut child_ui, |inner_ui| {
                let text_edit = egui::TextEdit::multiline(self.text)
                    .frame(egui::Frame::NONE)
                    .hint_text(&self.placeholder)
                    .text_color(theme.foreground)
                    .desired_width(inner_rect.width())
                    .desired_rows(3);

                inner_ui.add(text_edit)
            });

        let response = scroll_resp.inner;

        if outer_response.clicked() && !response.has_focus() {
            response.request_focus();
        }

        // Focus ring
        if response.has_focus() {
            let _ = ui.painter().rect_stroke(
                outer_rect,
                cr,
                egui::Stroke::new(1.0, theme.ring),
                egui::epaint::StrokeKind::Inside,
            );
            crate::paint::paint_focus_ring::paint_focus_ring(
                ui.painter(),
                outer_rect,
                corner_radius,
                theme.ring,
            );
        }

        response
    }
}
