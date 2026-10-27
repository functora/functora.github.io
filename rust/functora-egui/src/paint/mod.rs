//! Shared painting utilities.

pub mod interpolate_color;
pub mod paint_arc;
pub mod paint_dashed_rect;
pub mod paint_focus_ring;

/// A uniform corner radius in points, rounded to whole pixels, as the theme
/// values are fractional.
#[must_use]
pub fn corner_radius(radius: f32) -> egui::CornerRadius {
    egui::CornerRadius::same(crate::utils::f32_to_u8_clamped(radius))
}

pub use crate::utils::f32_to_u8_clamped;
pub use crate::utils::f32_to_usize_clamped;
pub use crate::utils::f64_to_f32;
pub use crate::utils::f64_to_i32;
pub use crate::utils::i32_to_f32;
pub use crate::utils::usize_to_f32;
pub use crate::utils::usize_to_u32;
