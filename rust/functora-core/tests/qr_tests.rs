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
    let (w, h, rgba) =
        qr_rgba("https://functora.github.io/apps/cryptonote/", 256).unwrap_or_else(|| panic!("qr"));
    assert_eq!(w, h);
    assert_eq!(w, 256);
    assert_eq!(
        rgba.len(),
        usize::try_from(w * h * 4).unwrap_or_else(|err| panic!("len: {err}"))
    );
}

#[test]
#[cfg(feature = "qr")]
fn qr_rgba_has_black_modules_and_white_quiet_zone() {
    let (_w, _h, rgba) =
        qr_rgba("https://functora.github.io/apps/cryptonote/", 256).unwrap_or_else(|| panic!("qr"));
    assert!(rgba.as_chunks::<4>().0.iter().any(|px| px[0] == 0));
    assert!(rgba.as_chunks::<4>().0.iter().any(|px| px[0] == 0xFF));
    let corner = &rgba[..4];
    assert_eq!(corner, &[0xFF, 0xFF, 0xFF, 0xFF]);
}

#[test]
#[cfg(feature = "qr")]
fn qr_rgba_rejects_output_beyond_byte_budget() {
    // Regression: the RGBA length was multiplied in u32, so a large requested
    // size overflowed (panicking in debug builds, wrapping in release).
    // The output budget must turn such requests into None instead.
    assert!(qr_rgba("https://functora.github.io/apps/cryptonote/", 2_000_000).is_none());
}

#[test]
#[cfg(feature = "qr")]
fn qr_rgba_scales_small_requests_within_budget() {
    assert!(qr_rgba("https://functora.github.io/apps/cryptonote/", 2000).is_some());
}

#[test]
fn qr_rgba_fails_on_empty_url() {
    assert!(qr_rgba("", 64).is_none());
}

#[test]
#[cfg(feature = "qr")]
fn qr_rgba_roundtrips_through_decode_qr_luma() {
    let url = "https://functora.github.io/apps/cryptonote/?note=SGVsbG8%3D";
    let (w, h, rgba) = qr_rgba(url, 512).unwrap_or_else(|| panic!("qr"));
    let luma = luma_of(&rgba);
    assert_eq!(decode_qr_luma(&luma, w, h).as_deref(), Some(url));
}

#[test]
#[cfg(feature = "qr")]
fn qr_rgba_roundtrips_through_decode_qr_luma_fast() {
    let url = "https://functora.github.io/apps/cryptonote/?note=SGVsbG8%3D";
    let (w, h, rgba) = qr_rgba(url, 512).unwrap_or_else(|| panic!("qr"));
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
