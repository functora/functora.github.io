#![cfg(all(feature = "thumbnail", not(target_arch = "wasm32")))]

use functora_core::thumbnail::planar_yuv_to_rgb;

#[test]
fn mid_gray_planes_convert_to_mid_gray_pixels() {
    let rgb = planar_yuv_to_rgb(8, 4, &[128; 32], &[128; 8], &[128; 8])
        .unwrap_or_else(|| panic!("valid planes must convert"));
    assert_eq!(rgb.len(), 8 * 4 * 3);
    assert!(rgb.chunks(3).all(|px| px == [128, 128, 128]));
}

#[test]
fn black_and_white_luma_with_neutral_chroma_matches_bt601_helpers() {
    let black = planar_yuv_to_rgb(2, 2, &[16, 16, 16, 16], &[128; 1], &[128; 1])
        .unwrap_or_else(|| panic!("valid planes must convert"));
    assert!(black.chunks(3).all(|px| px == [16, 16, 16]));
    let white = planar_yuv_to_rgb(2, 2, &[235, 235, 235, 235], &[128; 1], &[128; 1])
        .unwrap_or_else(|| panic!("valid planes must convert"));
    assert!(white.chunks(3).all(|px| px == [235, 235, 235]));
}

#[test]
fn oversized_padded_planes_are_accepted() {
    assert!(planar_yuv_to_rgb(2, 2, &[8; 8], &[9; 4], &[7; 4]).is_some());
}

#[test]
fn short_planes_are_rejected_instead_of_panicking() {
    assert!(planar_yuv_to_rgb(2, 2, &[0, 0, 0], &[1], &[1]).is_none());
    assert!(planar_yuv_to_rgb(2, 2, &[0; 4], &[], &[1]).is_none());
    assert!(planar_yuv_to_rgb(2, 2, &[0; 4], &[1], &[]).is_none());
}

#[test]
fn odd_420_dimensions_are_rejected() {
    assert!(planar_yuv_to_rgb(3, 2, &[0; 6], &[0; 1], &[0; 1]).is_none());
    assert!(planar_yuv_to_rgb(2, 3, &[0; 6], &[0; 1], &[0; 1]).is_none());
}

#[test]
fn empty_frames_are_rejected() {
    assert!(planar_yuv_to_rgb(0, 4, &[], &[], &[]).is_none());
    assert!(planar_yuv_to_rgb(4, 0, &[], &[], &[]).is_none());
}
