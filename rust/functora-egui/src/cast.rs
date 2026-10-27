//! Saturating numeric conversions for egui, JS and pixel boundaries.
//!
//! Every cast here is total: hostile values clamp or saturate instead of
//! panicking or wrapping.

#[must_use]
pub fn f32_to_u8_clamped(value: f32) -> u8 {
    use num_traits::ToPrimitive as _;
    value.round().clamp(0.0, 255.0).to_u8().unwrap_or(0)
}

#[must_use]
pub fn f32_to_usize_clamped(value: f32) -> usize {
    use num_traits::ToPrimitive as _;
    value
        .round()
        .clamp(0.0, 1_000_000.0)
        .to_usize()
        .unwrap_or(0)
}

#[must_use]
pub fn usize_to_f32(value: usize) -> f32 {
    use num_traits::ToPrimitive as _;
    value.to_f32().unwrap_or(0.0)
}

#[must_use]
pub fn i32_to_f32(value: i32) -> f32 {
    use num_traits::ToPrimitive as _;
    value.to_f32().unwrap_or(0.0)
}

#[must_use]
pub fn f64_to_f32(value: f64) -> f32 {
    use num_traits::ToPrimitive as _;
    value.to_f32().unwrap_or(0.0)
}

#[must_use]
pub fn f64_to_i32(value: f64) -> i32 {
    use num_traits::ToPrimitive as _;
    value.to_i32().unwrap_or(0)
}

#[must_use]
pub fn usize_to_u32(value: usize) -> u32 {
    u32::try_from(value).unwrap_or(u32::MAX)
}

/// Saturating `f64` -> `u64` conversion used for JS `double` sizes.
#[must_use]
pub fn f64_to_u64_clamped(value: f64) -> u64 {
    use num_traits::ToPrimitive as _;
    value.max(0.0).to_u64().unwrap_or(u64::MAX)
}

/// Loss-of-precision-tolerant `u64` -> `f64` for JS interop boundaries.
#[must_use]
pub fn u64_to_f64_js(value: u64) -> f64 {
    use num_traits::ToPrimitive as _;
    value.to_f64().unwrap_or(f64::NAN)
}

/// `u32` -> `f32` for scaling maths at JS/egui boundaries.
#[must_use]
pub fn u32_to_f32(value: u32) -> f32 {
    use num_traits::ToPrimitive as _;
    value.to_f32().unwrap_or(0.0)
}

#[must_use]
pub fn cap_to_available(desired: f32, available: f32) -> f32 {
    desired.min(available.max(0.0))
}

/// Scales a pixel dimension by `scale`, rounded and saturated to `u32`.
#[must_use]
pub fn scaled_px(value_px: u32, scale: f32) -> u32 {
    use num_traits::ToPrimitive as _;
    (u32_to_f32(value_px) * scale)
        .round()
        .to_u32()
        .unwrap_or(1)
        .max(1)
}
