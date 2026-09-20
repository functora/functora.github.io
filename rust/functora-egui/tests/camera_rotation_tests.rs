#![allow(clippy::unwrap_used, clippy::expect_used)]
use functora_egui::camera::FrameData;
use functora_egui::utils::{FrameRotation, fit_preview_size, rotate_luma, rotate_rgba};

#[test]
fn luma_90cw_swaps_dimensions_and_order() {
    let (rotated, w, h) = rotate_luma(&[0, 1, 2, 3, 4, 5], 3, 2, FrameRotation::Cw90);
    assert_eq!((w, h), (2, 3));
    assert_eq!(rotated, vec![3, 0, 4, 1, 5, 2]);
}

#[test]
fn luma_90cw_then_270cw_is_identity() {
    let src: Vec<u8> = (0..12).collect();
    let (once, w1, h1) = rotate_luma(&src, 4, 3, FrameRotation::Cw90);
    assert_eq!((w1, h1), (3, 4));
    let (back, w2, h2) = rotate_luma(&once, w1, h1, FrameRotation::Cw270);
    assert_eq!((w2, h2), (4, 3));
    assert_eq!(back, src);
}

#[test]
fn luma_four_quarter_turns_is_identity() {
    let src: Vec<u8> = (0..20).collect();
    let (r1, w1, h1) = rotate_luma(&src, 5, 4, FrameRotation::Cw90);
    let (r2, w2, h2) = rotate_luma(&r1, w1, h1, FrameRotation::Cw90);
    let (r3, w3, h3) = rotate_luma(&r2, w2, h2, FrameRotation::Cw90);
    let (r4, w4, h4) = rotate_luma(&r3, w3, h3, FrameRotation::Cw90);
    assert_eq!((w4, h4), (5, 4));
    assert_eq!(r4, src);
}

#[test]
fn luma_180_has_same_dimensions() {
    let (rotated, w, h) = rotate_luma(&[0, 1, 2, 3, 4, 5], 3, 2, FrameRotation::Cw180);
    assert_eq!((w, h), (3, 2));
    assert_eq!(rotated, vec![5, 4, 3, 2, 1, 0]);
}

#[test]
fn rgba_90cw_rotates_pixels_and_swaps_dimensions() {
    let src: Vec<u8> = vec![
        10, 11, 12, 255, 20, 21, 22, 255, 30, 31, 32, 255, 40, 41, 42, 255,
    ];
    let (rotated, w, h) = rotate_rgba(&src, 2, 2, FrameRotation::Cw90);
    assert_eq!((w, h), (2, 2));
    assert_eq!(
        rotated,
        vec![
            30, 31, 32, 255, 10, 11, 12, 255, 40, 41, 42, 255, 20, 21, 22, 255
        ]
    );
}

#[test]
fn rotation_from_degrees_maps_known_values() {
    assert_eq!(FrameRotation::from_degrees_cw(0), FrameRotation::Zero);
    assert_eq!(FrameRotation::from_degrees_cw(90), FrameRotation::Cw90);
    assert_eq!(FrameRotation::from_degrees_cw(180), FrameRotation::Cw180);
    assert_eq!(FrameRotation::from_degrees_cw(270), FrameRotation::Cw270);
    assert_eq!(FrameRotation::from_degrees_cw(45), FrameRotation::Zero);
}

#[test]
fn fit_preserves_portrait_frame_without_stretch() {
    let desired = egui::vec2(320.0, 240.0);
    let fitted = fit_preview_size(desired, 240, 320);
    assert!((fitted.x - 180.0).abs() < 0.5, "got {fitted:?}");
    assert!((fitted.y - 240.0).abs() < 0.5, "got {fitted:?}");
}

#[test]
fn fit_keeps_landscape_frame_full_box() {
    let desired = egui::vec2(320.0, 240.0);
    let fitted = fit_preview_size(desired, 640, 480);
    assert!((fitted.x - 320.0).abs() < 0.5, "got {fitted:?}");
    assert!((fitted.y - 240.0).abs() < 0.5, "got {fitted:?}");
}

#[test]
fn fit_falls_back_on_empty_frame() {
    let desired = egui::vec2(320.0, 240.0);
    assert_eq!(fit_preview_size(desired, 0, 0), desired);
}

#[test]
fn frame_upright_rotates_luma_and_preview_together() {
    let frame = FrameData {
        data: vec![0, 1, 2, 3, 4, 5],
        width: 3,
        height: 2,
        preview_rgba: Some(vec![
            0, 0, 0, 255, 1, 1, 1, 255, 2, 2, 2, 255, 3, 3, 3, 255, 4, 4, 4, 255, 5, 5, 5, 255,
        ]),
    };
    let upright = frame.upright(FrameRotation::Cw90);
    assert_eq!((upright.width, upright.height), (2, 3));
    assert_eq!(upright.data, vec![3, 0, 4, 1, 5, 2]);
    let preview = upright.preview_rgba.expect("preview must survive");
    assert_eq!(preview.len(), 2 * 3 * 4);
    assert_eq!(&preview[0..4], &[3, 3, 3, 255]);
}
