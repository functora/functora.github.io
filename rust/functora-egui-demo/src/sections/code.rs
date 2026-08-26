//! Code-snippet renderer: themed monospace block shown under live examples,
//! mirroring the `<pre><code>` usage blocks on the functora-css site.

/// Renders a fenced-style code block with the exact builder calls for the
/// example above it.
pub(crate) fn snippet(ui: &mut egui::Ui, code: &str) {
    let theme = functora_egui::ShadcnThemeExt::shadcn_theme(ui.ctx());
    let galley = ui.painter().layout_no_wrap(
        code.to_owned(),
        egui::FontId::monospace(11.5),
        theme.foreground,
    );
    let width = galley.size().x.max(ui.available_width());
    let response = ui
        .allocate_response(
            egui::vec2(width, galley.size().y + 20.0),
            egui::Sense::hover(),
        )
        .on_hover_text("Copy this pattern");
    let _ = ui.painter().rect_filled(
        response.rect,
        egui::CornerRadius::same(functora_egui::utils::f32_to_u8_clamped(theme.radius)),
        theme.muted,
    );
    let _ = ui.painter().rect_stroke(
        response.rect,
        egui::CornerRadius::same(functora_egui::utils::f32_to_u8_clamped(theme.radius)),
        egui::Stroke::new(1.0, theme.border),
        egui::epaint::StrokeKind::Inside,
    );
    ui.painter().galley(
        response.rect.min + egui::vec2(10.0, 10.0),
        galley,
        theme.foreground,
    );
}
