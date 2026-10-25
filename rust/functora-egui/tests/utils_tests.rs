//! Camera/utility helper behavior.

use functora_egui::utils::{fps_to_interval_ms, nv21_luma, nv21_to_rgba, pixel_area_len};

#[test]
fn nv21_luma_copies_y_plane() {
    let nv21 = vec![1, 2, 3, 4, 200, 210];
    assert_eq!(nv21_luma(&nv21, 2, 2), vec![1, 2, 3, 4]);
    assert!(nv21_luma(&nv21[..4], 4, 4).is_empty());
}

#[test]
fn nv21_to_rgba_neutral_chroma_keeps_gray() {
    // Y=180 with neutral chroma -> luma' = 164 ->
    // R=G=B = (298*164 + 128) >> 8 = 191.
    let mut nv21 = vec![180_u8; 4];
    nv21.extend_from_slice(&[128, 128]);
    let rgba = nv21_to_rgba(&nv21, 2, 2);
    for px in rgba.as_chunks::<4>().0 {
        assert_eq!((px[0], px[1], px[2], px[3]), (191, 191, 191, 255));
    }
}

#[test]
fn nv21_to_rgba_saturated_blue() {
    // Y=41, neutral Cr, max Cb -> B clamps to 255, R stays small, G -> 0.
    let mut nv21 = vec![41_u8; 4];
    nv21.extend_from_slice(&[128, 240]);
    let rgba = nv21_to_rgba(&nv21, 2, 2);
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
    let rgba = nv21_to_rgba(&nv21, 6, 3);
    assert_eq!(rgba.len(), 6 * 3 * 4);
}

#[test]
fn nv21_to_rgba_full_even_frame() {
    let nv21 = vec![100_u8; 4 * 4 + 4 * 2];
    let rgba = nv21_to_rgba(&nv21, 4, 4);
    assert_eq!(rgba.len(), 4 * 4 * 4);
    assert!(rgba.as_chunks::<4>().0.iter().all(|px| px[3] == 255));
}

#[test]
fn fps_interval_floor_and_clamp() {
    assert_eq!(fps_to_interval_ms(15.0), 67);
    assert_eq!(fps_to_interval_ms(120.0), 17); // 1000/60 rounded
    assert_eq!(fps_to_interval_ms(0.0), 1000);
}

#[test]
fn pixel_area_len_multiplies_without_overflow() {
    assert_eq!(pixel_area_len(2, 2), 4);
    assert_eq!(pixel_area_len(0, u32::MAX), 0);
    assert_eq!(
        pixel_area_len(u32::MAX, u32::MAX),
        usize::try_from(u64::from(u32::MAX) * u64::from(u32::MAX)).unwrap_or(usize::MAX)
    );
}

#[test]
fn nv21_luma_hostile_dimensions_return_empty() {
    assert!(nv21_luma(&[1, 2, 3], u32::MAX, u32::MAX).is_empty());
}
