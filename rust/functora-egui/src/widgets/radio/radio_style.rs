//! Maps radio state to concrete style values.

/// Resolves radio button colors based on selection state and interaction.
#[must_use]
pub fn resolve_radio_style(
    theme: &crate::theme::shadcn_theme::ShadcnTheme,
    selected: bool,
    hovered: bool,
    disabled: bool,
) -> super::resolved_radio_style::ResolvedRadioStyle {
    if disabled {
        return super::resolved_radio_style::ResolvedRadioStyle {
            circle_border: with_alpha(theme.input, 110),
            dot_color: if selected {
                with_alpha(theme.primary, 110)
            } else {
                egui::Color32::TRANSPARENT
            },
            text_color: theme.muted_foreground,
        };
    }
    let circle_border = if selected {
        theme.primary
    } else if hovered {
        theme.ring
    } else {
        theme.input // Nova: border-input instead of border
    };

    let dot_color = if selected {
        theme.primary
    } else {
        egui::Color32::TRANSPARENT
    };

    super::resolved_radio_style::ResolvedRadioStyle {
        circle_border,
        dot_color,
        text_color: theme.foreground,
    }
}

fn with_alpha(c: egui::Color32, a: u8) -> egui::Color32 {
    crate::utils::with_alpha(c, a)
}
