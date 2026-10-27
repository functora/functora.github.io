//! Compatibility façade over the domain modules the former grab-bag
//! helpers moved into: numeric casts ([`crate::cast`]), camera frame
//! conversion ([`crate::camera`]), async spawning ([`crate::worker::spawn_async`]),
//! color helpers ([`crate::paint`]) and overlay clamping
//! ([`crate::widgets::overlay_common`]).

pub use crate::cast::cap_to_available;
pub use crate::cast::f32_to_u8_clamped;
pub use crate::cast::f32_to_usize_clamped;
pub use crate::cast::f64_to_f32;
pub use crate::cast::f64_to_i32;
pub use crate::cast::f64_to_u64_clamped;
pub use crate::cast::i32_to_f32;
pub use crate::cast::scaled_px;
pub use crate::cast::u32_to_f32;
pub use crate::cast::u64_to_f64_js;
pub use crate::cast::usize_to_f32;
pub use crate::cast::usize_to_u32;
pub use crate::frame::FrameRotation;
pub use crate::frame::fit_preview_size;
pub use crate::frame::fps_to_interval_ms;
pub use crate::frame::nv21_luma;
pub use crate::frame::nv21_to_rgba;
pub use crate::frame::pixel_area_len;
pub use crate::frame::responsive_preview_size;
pub use crate::frame::rotate_luma;
pub use crate::frame::rotate_rgba;
pub use crate::frame::rotated_dimensions;
#[cfg(feature = "runtime")]
pub use crate::worker::spawn_async;

pub use crate::frame::RESPONSIVE_PREVIEW_FALLBACK_ASPECT;
pub use crate::frame::RESPONSIVE_PREVIEW_FALLBACK_WIDTH;
pub use crate::frame::RESPONSIVE_PREVIEW_MAX_SIDE;
