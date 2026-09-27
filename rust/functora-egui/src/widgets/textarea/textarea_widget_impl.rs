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
            .min_scrolled_height(inner_rect.height())
            .show(&mut child_ui, |inner_ui| {
                let mut wrap = wrap_anywhere_layouter;
                let text_edit = egui::TextEdit::multiline(self.text)
                    .frame(egui::Frame::NONE)
                    .hint_text(&self.placeholder)
                    .text_color(theme.foreground)
                    .desired_width(inner_rect.width())
                    .desired_rows(8)
                    .layouter(&mut wrap);

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

/// A `TextEdit` layouter that wraps at `wrap_width` and breaks anywhere
/// inside a token, so a long unbroken URL/base64 payload never paints
/// outside the field's border. Falls back to egui's default word wrapping
/// for normal text; the break-anywhere bit only matters for single-token
/// overflows.
pub(crate) fn wrap_anywhere_layouter(
    ui: &egui::Ui,
    text: &dyn egui::TextBuffer,
    wrap_width: f32,
) -> std::sync::Arc<egui::Galley> {
    let mut job = egui::text::LayoutJob::single_section(
        text.as_str().to_owned(),
        egui::TextFormat {
            font_id: egui::FontId::proportional(14.0),
            color: ui.visuals().text_color(),
            ..Default::default()
        },
    );
    job.wrap.max_width = wrap_width;
    job.wrap.max_rows = usize::MAX;
    job.wrap.break_anywhere = true;
    ui.fonts_mut(|fonts| fonts.layout_job(job))
}
