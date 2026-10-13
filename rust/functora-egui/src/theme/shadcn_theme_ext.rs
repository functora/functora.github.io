//! Extension trait for accessing the `ShadcnTheme` from `egui::Context`.

/// Extension trait that adds shadcn theme access to `egui::Context`.
pub trait ShadcnThemeExt {
    /// Returns a clone of the current shadcn theme (light by default).
    fn shadcn_theme(&self) -> super::shadcn_theme::ShadcnTheme;

    /// Sets the shadcn theme for all subsequent frames.
    fn set_shadcn_theme(&self, theme: super::shadcn_theme::ShadcnTheme);
}

impl ShadcnThemeExt for egui::Context {
    fn shadcn_theme(&self) -> super::shadcn_theme::ShadcnTheme {
        self.data(|d| d.get_temp::<super::shadcn_theme::ShadcnTheme>(egui::Id::NULL))
            .unwrap_or_default()
    }

    fn set_shadcn_theme(&self, theme: super::shadcn_theme::ShadcnTheme) {
        self.all_styles_mut(|style| {
            style.visuals.panel_fill = theme.background;
            style.visuals.window_fill = theme.popover;
            style.visuals.window_stroke = egui::Stroke::new(1.0, theme.border);
            style.visuals.widgets.noninteractive.bg_stroke = egui::Stroke::new(1.0, theme.border);
            style.visuals.override_text_color = Some(theme.foreground);
            style.visuals.weak_text_color = Some(theme.muted_foreground);
            style.visuals.hyperlink_color = theme.primary;
            style.visuals.selection.bg_fill = theme.primary;
            style.visuals.selection.stroke = egui::Stroke::new(1.0, theme.primary_foreground);
            style.visuals.extreme_bg_color = theme.secondary;
            style.visuals.code_bg_color = theme.accent;
            style.visuals.faint_bg_color = theme.secondary;
            style.visuals.widgets.noninteractive.fg_stroke.color = theme.foreground;
            style.visuals.widgets.inactive.fg_stroke.color = theme.foreground;
            style.visuals.widgets.active.fg_stroke.color = theme.foreground;
            style.visuals.window_shadow = egui::Shadow {
                offset: [0, 4],
                blur: 12,
                spread: 0,
                color: egui::Color32::from_black_alpha(8),
            };
        });
        self.data_mut(|d| {
            let _ = d.insert_temp::<super::shadcn_theme::ShadcnTheme>(egui::Id::NULL, theme);
        });
    }
}
