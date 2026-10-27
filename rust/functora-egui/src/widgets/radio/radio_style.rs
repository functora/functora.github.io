//! Maps radio state to concrete style values.

use crate::paint::interpolate_color::with_alpha;
/// Resolves radio button colors based on selection state and interaction.
#[must_use]
pub fn resolve_radio_style(
    theme: &crate::theme::shadcn_theme::ShadcnTheme,
    selected: bool,
    interaction: &crate::widgets::interaction::Interaction,
) -> super::resolved_radio_style::ResolvedRadioStyle {
    if interaction.disabled {
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
    } else if interaction.hovered {
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
