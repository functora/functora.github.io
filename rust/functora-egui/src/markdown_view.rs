use crate::theme::shadcn_theme_ext::ShadcnThemeExt;

/// Render markdown with shadcn theme tokens mapped onto the egui visuals that
/// `egui_commonmark` consumes, restoring the previous visuals afterwards so
/// no global state leaks.
pub fn show(
    ui: &mut egui::Ui,
    cache: &mut crate::CommonMarkCache,
    text: &str,
) -> egui::InnerResponse<()> {
    let theme = ui.ctx().shadcn_theme();
    let preserved = ui.visuals().clone();
    let visuals = ui.visuals_mut();
    visuals.override_text_color = Some(theme.foreground);
    visuals.hyperlink_color = theme.primary;
    visuals.weak_text_color = Some(theme.muted_foreground);
    visuals.extreme_bg_color = theme.muted;
    visuals.widgets.noninteractive.bg_stroke = egui::Stroke::new(1.0, theme.border);
    visuals.widgets.noninteractive.bg_fill = theme.muted;
    visuals.widgets.active.bg_fill = theme.muted;
    visuals.widgets.inactive.bg_fill = theme.muted;
    visuals.code_bg_color = theme.muted;
    visuals.widgets.active.fg_stroke.color = theme.foreground;
    visuals.widgets.inactive.fg_stroke.color = theme.foreground;
    visuals.widgets.noninteractive.fg_stroke.color = theme.foreground;
    let response = crate::CommonMarkViewer::new().show(ui, cache, text);
    *ui.visuals_mut() = preserved;
    response
}
