#![allow(clippy::unwrap_used, clippy::expect_used)]

use functora_core::qr::{decode_qr_luma, decode_qr_luma_fast, qr_rgba};

#[cfg(feature = "qr")]
fn luma_of(rgba: &[u8]) -> Vec<u8> {
    rgba.as_chunks::<4>()
        .0
        .iter()
        .map(|px| if px[0] == 0 { 0 } else { 0xFF })
        .collect()
}

#[test]
#[cfg(feature = "qr")]
fn qr_rgba_produces_square_image() {
    let (w, h, rgba) = qr_rgba("https://functora.github.io/apps/cryptonote/", 256).expect("qr");
    assert_eq!(w, h);
    assert_eq!(w, 256);
    assert_eq!(rgba.len(), usize::try_from(w * h * 4).expect("len"));
}

#[test]
#[cfg(feature = "qr")]
fn qr_rgba_has_black_modules_and_white_quiet_zone() {
    let (_w, _h, rgba) = qr_rgba("https://functora.github.io/apps/cryptonote/", 256).expect("qr");
    assert!(rgba.as_chunks::<4>().0.iter().any(|px| px[0] == 0));
    assert!(rgba.as_chunks::<4>().0.iter().any(|px| px[0] == 0xFF));
    let corner = &rgba[..4];
    assert_eq!(corner, &[0xFF, 0xFF, 0xFF, 0xFF]);
}

#[test]
fn qr_rgba_fails_on_empty_url() {
    assert!(qr_rgba("", 64).is_none());
}

#[test]
#[cfg(feature = "qr")]
fn qr_rgba_roundtrips_through_decode_qr_luma() {
    let url = "https://functora.github.io/apps/cryptonote/?note=SGVsbG8%3D";
    let (w, h, rgba) = qr_rgba(url, 512).expect("qr");
    let luma = luma_of(&rgba);
    assert_eq!(decode_qr_luma(&luma, w, h).as_deref(), Some(url));
}

#[test]
#[cfg(feature = "qr")]
fn qr_rgba_roundtrips_through_decode_qr_luma_fast() {
    let url = "https://functora.github.io/apps/cryptonote/?note=SGVsbG8%3D";
    let (w, h, rgba) = qr_rgba(url, 512).expect("qr");
    let luma = luma_of(&rgba);
    assert_eq!(decode_qr_luma_fast(&luma, w, h).as_deref(), Some(url));
}

#[test]
#[cfg(not(feature = "qr"))]
fn qr_disabled_returns_no_result() {
    assert!(qr_rgba("https://example.com", 64).is_none());
    assert!(decode_qr_luma(&[0; 16], 4, 4).is_none());
    assert!(decode_qr_luma_fast(&[0; 16], 4, 4).is_none());
    assert!(functora_core::qr::decode_qr_rgba(&[0; 64], 4, 4).is_none());
}
