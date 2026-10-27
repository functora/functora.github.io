//! Code snippet widget — themed monospace block for displaying code examples.
//! Mirrors the `<pre><code>` usage blocks on the functora-css site.

use egui::{FontId, Sense, Stroke, StrokeKind, vec2};

/// Renders a fenced-style code block with the exact builder calls for the
/// example above it. Text wraps at the available width (normal wrapping).
pub fn snippet(ui: &mut egui::Ui, code: &str) {
    snippet_impl(ui, code, false);
}

/// Renders a code snippet that breaks long words anywhere.
/// Use for code with very long unbroken strings (e.g. base64, minified JSON).
pub fn snippet_break_long_words(ui: &mut egui::Ui, code: &str) {
    snippet_impl(ui, code, true);
}

fn snippet_impl(ui: &mut egui::Ui, code: &str, break_long_words: bool) {
    ui.add_space(24.0);
    let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());
    let available = ui.available_width();
    let wrap_width = (available - 20.0).max(0.0);

    let galley = crate::text::layout(
        ui,
        code.to_owned(),
        FontId::monospace(11.5),
        theme.foreground,
        wrap_width,
        Some(if break_long_words {
            crate::text::Wrap::Any
        } else {
            crate::text::Wrap::Word
        }),
    );
    let height = galley.size().y + 20.0;
    let response = ui
        .allocate_response(vec2(available, height), Sense::hover())
        .on_hover_text("Copy this pattern");
    let _ = ui.painter().rect_filled(
        response.rect,
        crate::paint::corner_radius(theme.radius),
        theme.muted,
    );
    let _ = ui.painter().rect_stroke(
        response.rect,
        crate::paint::corner_radius(theme.radius),
        Stroke::new(1.0, theme.border),
        StrokeKind::Inside,
    );
    ui.painter().galley(
        response.rect.min + vec2(10.0, 10.0),
        galley,
        theme.foreground,
    );
}
