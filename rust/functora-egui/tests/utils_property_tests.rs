//! Camera and conversion helpers must preserve their size, clamping and
//! format invariants for arbitrary inputs.
#![cfg(not(target_arch = "wasm32"))]

use functora_egui::utils::{
    f64_to_u64_clamped, fps_to_interval_ms, nv21_luma, nv21_to_rgba, pixel_area_len,
};
use proptest::prelude::*;
use proptest::test_runner::RngSeed;

proptest! {
    #![proptest_config(ProptestConfig {
        rng_seed: RngSeed::Fixed(0x00C0_FFEE),
        ..ProptestConfig::default()
    })]

    #[test]
    fn nv21_luma_is_empty_when_the_frame_is_short(data in proptest::collection::vec(any::<u8>(), 0..64),
        width in 0..16_u32, height in 0..16_u32) {
        let luma = nv21_luma(&data, width, height);
        let needed = pixel_area_len(width, height);
        let expected = if data.len() >= needed { needed } else { 0 };
        prop_assert_eq!(luma.len(), expected);
    }

    #[test]
    fn nv21_to_rgba_emits_four_bytes_per_pixel(data in proptest::collection::vec(any::<u8>(), 0..64),
        width in 0..16_u32, height in 0..16_u32) {
        let rgba = nv21_to_rgba(&data, width, height);
        let pixels = usize::try_from(width.max(1) * height.max(1)).unwrap_or(usize::MAX);
        prop_assert_eq!(rgba.len(), pixels.saturating_mul(4));
        prop_assert!(rgba.as_chunks::<4>().0.iter().all(|pixel| pixel[3] == 255));
    }

    #[test]
    fn nv21_luma_copies_the_y_plane_verbatim(data in proptest::collection::vec(any::<u8>(), 0..64),
        width in 1..16_u32, height in 1..16_u32) {
        let needed = pixel_area_len(width, height);
        prop_assume!(data.len() >= needed);
        prop_assert_eq!(&nv21_luma(&data, width, height), &data[..needed]);
    }

    #[test]
    fn fps_interval_stays_within_the_clamped_range(fps in -100.0_f32..1000.0) {
        let interval = fps_to_interval_ms(fps);
        prop_assert!((16..=1000).contains(&interval), "interval {interval} out of range");
    }

    #[test]
    fn f64_to_u64_clamped_clamps_negatives_to_zero(value in -1.0e12_f64..0.0) {
        prop_assert_eq!(f64_to_u64_clamped(value), 0);
    }
}
