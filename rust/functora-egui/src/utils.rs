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

#[must_use]
pub fn nv21_luma(nv21: &[u8], width: u32, height: u32) -> Vec<u8> {
    let len = (width * height) as usize;
    nv21.get(..len).map_or_else(Vec::new, <[u8]>::to_vec)
}

/// BT.601 integer NV21 -> RGBA8888 conversion.
#[must_use]
pub fn nv21_to_rgba(nv21: &[u8], width: u32, height: u32) -> Vec<u8> {
    let cols = width.max(1);
    let rows = height.max(1);
    let y_len = (cols * rows) as usize;
    let clamp = |value: i32| u8::try_from(value.clamp(0, 255)).unwrap_or(u8::MAX);
    let uv_offset = |row: usize, plane: usize, stride: u32| plane + (row / 2) * stride as usize;
    (0..rows as usize)
        .flat_map(|row| {
            let row_uv = uv_offset(row, y_len, cols);
            (0..cols as usize).map(move |col| {
                let y_index = row * cols as usize + col;
                let uv_index = row_uv + (col / 2) * 2;
                let luma = i32::from(nv21.get(y_index).copied().unwrap_or(235)).saturating_sub(16);
                let cr = i32::from(nv21.get(uv_index).copied().unwrap_or(128)) - 128;
                let cb = i32::from(nv21.get(uv_index + 1).copied().unwrap_or(128)) - 128;
                (
                    clamp((298 * luma + 402 * cr + 128) >> 8),
                    clamp((298 * luma - 100 * cb - 208 * cr + 128) >> 8),
                    clamp((298 * luma + 516 * cb + 128) >> 8),
                    255_u8,
                )
            })
        })
        .flat_map(|(red, green, blue, alpha)| [red, green, blue, alpha])
        .collect()
}

#[cfg(test)]
mod tests {
    #[test]
    fn nv21_luma_copies_y_plane() {
        let nv21 = vec![1, 2, 3, 4, 200, 210];
        assert_eq!(super::nv21_luma(&nv21, 2, 2), vec![1, 2, 3, 4]);
        assert!(super::nv21_luma(&nv21[..4], 4, 4).is_empty());
    }

    #[test]
    fn nv21_to_rgba_neutral_chroma_keeps_gray() {
        // Y=180 with neutral chroma -> luma' = 164 ->
        // R=G=B = (298*164 + 128) >> 8 = 191.
        let mut nv21 = vec![180_u8; 4];
        nv21.extend_from_slice(&[128, 128]);
        let rgba = super::nv21_to_rgba(&nv21, 2, 2);
        for px in rgba.chunks_exact(4) {
            assert_eq!((px[0], px[1], px[2], px[3]), (191, 191, 191, 255));
        }
    }

    #[test]
    fn nv21_to_rgba_saturated_blue() {
        // Y=41, neutral Cr, max Cb -> B clamps to 255, R stays small, G -> 0.
        let mut nv21 = vec![41_u8; 4];
        nv21.extend_from_slice(&[128, 240]);
        let rgba = super::nv21_to_rgba(&nv21, 2, 2);
        let px = &rgba[0..4];
        assert_eq!(px[2], 255);
        assert!(px[0] < 32);
        assert_eq!(px[1], 0);
        assert_eq!(px[3], 255);
    }

    #[test]
    fn nv21_to_rgba_handles_odd_height_without_panic() {
        // Odd height has a truncated chroma plane; missing samples default to
        // neutral instead of panicking.
        let nv21 = vec![128_u8; 6 * 3 + 3];
        let rgba = super::nv21_to_rgba(&nv21, 6, 3);
        assert_eq!(rgba.len(), 6 * 3 * 4);
    }

    #[test]
    fn nv21_to_rgba_full_even_frame() {
        let nv21 = vec![100_u8; 4 * 4 + 4 * 2];
        let rgba = super::nv21_to_rgba(&nv21, 4, 4);
        assert_eq!(rgba.len(), 4 * 4 * 4);
        assert!(rgba.chunks_exact(4).all(|px| px[3] == 255));
    }
}
