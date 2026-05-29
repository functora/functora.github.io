#[cfg(feature = "qr")]
pub use rxing::common::HybridBinarizer;
#[cfg(feature = "qr")]
use rxing::qrcode::QRCodeReader;
#[cfg(feature = "qr")]
use rxing::{BinaryBitmap, DecodeHints, ImmutableReader, Luma8LuminanceSource};

#[cfg(feature = "qr")]
#[must_use]
pub fn decode_hints() -> DecodeHints {
    DecodeHints {
        TryHarder: Some(true),
        ..Default::default()
    }
}

#[cfg(feature = "qr")]
#[must_use]
#[allow(clippy::cast_possible_truncation)]
pub fn decode_qr_rgba(rgba: &[u8], w: u32, h: u32) -> Option<String> {
    if w == 0 || h == 0 {
        return None;
    }
    let luma: Vec<u8> = rgba
        .chunks_exact(4)
        .map(|px| {
            if px[3] == 0 {
                0xFF
            } else {
                ((306 * u64::from(px[0]) + 601 * u64::from(px[1]) + 117 * u64::from(px[2]) + 0x200) >> 10) as u8
            }
        })
        .collect();
    decode_qr_luma(&luma, w, h)
}

#[cfg(feature = "qr")]
#[must_use]
pub fn decode_qr_luma(luma: &[u8], w: u32, h: u32) -> Option<String> {
    if w == 0 || h == 0 {
        return None;
    }
    QRCodeReader::new()
        .immutable_decode_with_hints(
            &mut BinaryBitmap::new(HybridBinarizer::new(Luma8LuminanceSource::new(luma.to_vec(), w, h))),
            &decode_hints(),
        )
        .ok()
        .map(|r| r.getText().to_owned())
}

#[cfg(not(feature = "qr"))]
pub fn decode_qr_rgba(_rgba: &[u8], _w: u32, _h: u32) -> Option<String> {
    None
}

#[cfg(not(feature = "qr"))]
pub fn decode_qr_luma(_luma: &[u8], _w: u32, _h: u32) -> Option<String> {
    None
}
