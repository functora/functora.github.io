//! `FieldLegend` — `font-medium text-base` heading for a field set.

/// A field legend heading.
pub struct FieldLegend;

impl FieldLegend {
    /// Paints a legend heading.
    pub fn show(ui: &mut egui::Ui, text: &str) {
        let theme = crate::theme::shadcn_theme_ext::ShadcnThemeExt::shadcn_theme(ui.ctx());
        let galley = crate::text::layout(
            ui,
            text.to_owned(),
            egui::FontId::proportional(16.0), // text-base
            theme.foreground,
            ui.available_width(),
            None,
        );
        let (rect, _) = ui.allocate_exact_size(galley.size(), egui::Sense::hover());
        if ui.is_rect_visible(rect) {
            ui.painter().galley(rect.min, galley, theme.foreground);
        }
    }
}
