//! Maps switch state to concrete style values.

/// Resolves switch colors based on on/off state. Uses iOS-style white thumb
/// for enabled and muted transparent look for disabled.
#[must_use]
pub fn resolve_switch_style(
    theme: &crate::theme::shadcn_theme::ShadcnTheme,
    on: bool,
    anim_t: f32,
    disabled: bool,
) -> super::resolved_switch_style::ResolvedSwitchStyle {
    if disabled {
        let track_off = with_alpha(theme.muted, 110);
        let track_on = with_alpha(theme.primary, 110);
        let track_color =
            crate::paint::interpolate_color::interpolate_color(track_off, track_on, anim_t);
        let track_border = Some(with_alpha(theme.border, 110));
        let thumb_color = with_alpha(egui::Color32::WHITE, 200);
        return super::resolved_switch_style::ResolvedSwitchStyle {
            track_color,
            track_border,
            thumb_color,
        };
    }

    let track_off = theme.input;
    let track_on = theme.primary;
    let track_color =
        crate::paint::interpolate_color::interpolate_color(track_off, track_on, anim_t);

    // In dark mode the off-track (input at 15% white) is very faint.
    // Add a border to define the track shape, matching shadcn's shadow-xs.
    let track_border = if on { None } else { Some(theme.border) };

    let thumb_color = egui::Color32::WHITE;

    super::resolved_switch_style::ResolvedSwitchStyle {
        track_color,
        track_border,
        thumb_color,
    }
}

fn with_alpha(c: egui::Color32, a: u8) -> egui::Color32 {
    egui::Color32::from_rgba_unmultiplied(c.r(), c.g(), c.b(), a)
}
