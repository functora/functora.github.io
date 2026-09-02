#![allow(clippy::unwrap_used, clippy::expect_used)]

use functora_core::qr::{decode_qr_luma, qr_rgba};

fn luma_of(rgba: &[u8]) -> Vec<u8> {
    rgba.chunks_exact(4)
        .map(|px| if px[0] == 0 { 0 } else { 0xFF })
        .collect()
}

#[test]
fn qr_rgba_produces_square_image() {
    let (w, h, rgba) = qr_rgba("https://functora.github.io/apps/cryptonote/", 256).expect("qr");
    assert_eq!(w, h);
    assert_eq!(w, 256);
    assert_eq!(rgba.len(), usize::try_from(w * h * 4).expect("len"));
}

#[test]
fn qr_rgba_has_black_modules_and_white_quiet_zone() {
    let (_w, _h, rgba) = qr_rgba("https://functora.github.io/apps/cryptonote/", 256).expect("qr");
    assert!(rgba.chunks_exact(4).any(|px| px[0] == 0));
    assert!(rgba.chunks_exact(4).any(|px| px[0] == 0xFF));
    let corner = &rgba[..4];
    assert_eq!(corner, &[0xFF, 0xFF, 0xFF, 0xFF]);
}

#[test]
fn qr_rgba_fails_on_empty_url() {
    assert!(qr_rgba("", 64).is_none());
}

#[test]
fn qr_rgba_roundtrips_through_decode_qr_luma() {
    let url = "https://functora.github.io/apps/cryptonote/?note=SGVsbG8%3D";
    let (w, h, rgba) = qr_rgba(url, 512).expect("qr");
    let luma = luma_of(&rgba);
    assert_eq!(decode_qr_luma(&luma, w, h).as_deref(), Some(url));
}
