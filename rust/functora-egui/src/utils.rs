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
