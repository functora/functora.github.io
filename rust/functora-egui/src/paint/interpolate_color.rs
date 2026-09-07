//! Color interpolation utility for animated transitions.

/// Linearly interpolates between two colors by factor `t` (0.0 = a, 1.0 = b).
#[must_use]
pub fn interpolate_color(a: egui::Color32, b: egui::Color32, t: f32) -> egui::Color32 {
    let clamped_t = t.clamp(0.0, 1.0);
    let hue_a = a.to_srgba_unmultiplied();
    let hue_b = b.to_srgba_unmultiplied();
    let lerp = |x: u8, y: u8| -> u8 {
        let v = f32::from(x) * (1.0 - clamped_t) + f32::from(y) * clamped_t;
        crate::utils::f32_to_u8_clamped(v)
    };
    egui::Color32::from_rgba_unmultiplied(
        lerp(hue_a[0], hue_b[0]),
        lerp(hue_a[1], hue_b[1]),
        lerp(hue_a[2], hue_b[2]),
        lerp(hue_a[3], hue_b[3]),
    )
}
