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

/// Converts a frames-per-second rate into a millisecond capture interval,
/// clamped to the 1..=60 fps range with a 16 ms floor.
#[must_use]
pub fn fps_to_interval_ms(fps: f32) -> u64 {
    use num_traits::ToPrimitive as _;
    let clamped = fps.clamp(1.0, 60.0);
    let millis = (1000.0 / clamped).round().max(16.0);
    millis.to_u64().unwrap_or(66)
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

/// Spawns an async task and returns a receiver for the result.
/// Works on both wasm32 (using `wasm_bindgen_futures::spawn_local`)
/// and native (using `std::thread::spawn` with `pollster::block_on`).
#[cfg(target_arch = "wasm32")]
pub fn spawn_async<F, T>(future: F) -> std::sync::mpsc::Receiver<T>
where
    F: std::future::Future<Output = T> + 'static,
    T: 'static,
{
    let (tx, rx) = std::sync::mpsc::channel();
    wasm_bindgen_futures::spawn_local(async move {
        let res = future.await;
        drop(tx.send(res));
    });
    rx
}

#[cfg(not(target_arch = "wasm32"))]
pub fn spawn_async<F, T>(future: F) -> std::sync::mpsc::Receiver<T>
where
    F: std::future::Future<Output = T> + Send + 'static,
    T: Send + 'static,
{
    let (tx, rx) = std::sync::mpsc::channel();
    drop(std::thread::spawn(move || {
        let res = pollster::block_on(future);
        drop(tx.send(res));
    }));
    rx
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

    #[test]
    fn fps_interval_floor_and_clamp() {
        assert_eq!(super::fps_to_interval_ms(15.0), 67);
        assert_eq!(super::fps_to_interval_ms(120.0), 17); // 1000/60 rounded
        assert_eq!(super::fps_to_interval_ms(0.0), 1000);
    }
}
