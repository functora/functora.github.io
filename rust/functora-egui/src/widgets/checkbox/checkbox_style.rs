//! Maps checkbox state to concrete style values.

/// Resolves checkbox colors based on checked state and interaction.
#[must_use]
pub fn resolve_checkbox_style(
    theme: &crate::theme::shadcn_theme::ShadcnTheme,
    checked: bool,
    hovered: bool,
    disabled: bool,
) -> super::resolved_checkbox_style::ResolvedCheckboxStyle {
    if disabled {
        return super::resolved_checkbox_style::ResolvedCheckboxStyle {
            box_bg: if checked {
                with_alpha(theme.primary, 110)
            } else {
                egui::Color32::TRANSPARENT
            },
            box_border: with_alpha(theme.border, 110),
            check_color: with_alpha(theme.primary_foreground, 110),
            text_color: theme.muted_foreground,
        };
    }
    let (box_bg, box_border, check_color) = if checked {
        (theme.primary, theme.primary, theme.primary_foreground)
    } else if hovered {
        (
            egui::Color32::TRANSPARENT,
            theme.ring,
            egui::Color32::TRANSPARENT,
        )
    } else {
        (
            egui::Color32::TRANSPARENT,
            theme.border,
            egui::Color32::TRANSPARENT,
        )
    };

    super::resolved_checkbox_style::ResolvedCheckboxStyle {
        box_bg,
        box_border,
        check_color,
        text_color: theme.foreground,
    }
}

fn with_alpha(c: egui::Color32, a: u8) -> egui::Color32 {
    crate::utils::with_alpha(c, a)
}
