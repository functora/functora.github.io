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
pub fn decode_qr_rgba(rgba: &[u8], w: u32, h: u32) -> Option<String> {
    (w != 0 && h != 0).then_some(())?;
    let luma: Vec<u8> = rgba
        .as_chunks::<4>()
        .0
        .iter()
        .map(|px| {
            if px[3] == 0 {
                0xFF
            } else {
                u8::try_from(
                    (306 * u64::from(px[0])
                        + 601 * u64::from(px[1])
                        + 117 * u64::from(px[2])
                        + 0x200)
                        >> 10,
                )
                .unwrap_or(u8::MAX)
            }
        })
        .collect();
    decode_qr_luma(&luma, w, h)
}

#[cfg(feature = "qr")]
fn decode_luma_with_hints(luma: &[u8], w: u32, h: u32, hints: &DecodeHints) -> Option<String> {
    (w != 0 && h != 0).then_some(())?;
    QRCodeReader::new()
        .immutable_decode_with_hints(
            &mut BinaryBitmap::new(HybridBinarizer::new(
                Luma8LuminanceSource::new(luma.to_vec(), w, h).ok()?,
            )),
            hints,
        )
        .ok()
        .map(|r| r.getText().to_owned())
}

#[cfg(feature = "qr")]
#[must_use]
pub fn decode_qr_luma(luma: &[u8], w: u32, h: u32) -> Option<String> {
    decode_luma_with_hints(luma, w, h, &decode_hints())
}

#[cfg(feature = "qr")]
#[must_use]
pub fn decode_qr_luma_fast(luma: &[u8], w: u32, h: u32) -> Option<String> {
    decode_luma_with_hints(luma, w, h, &DecodeHints::default())
}

#[cfg(not(feature = "qr"))]
#[must_use]
pub fn decode_qr_rgba(_rgba: &[u8], _w: u32, _h: u32) -> Option<String> {
    None
}

#[cfg(not(feature = "qr"))]
#[must_use]
pub fn decode_qr_luma(_luma: &[u8], _w: u32, _h: u32) -> Option<String> {
    None
}

#[cfg(not(feature = "qr"))]
#[must_use]
pub fn decode_qr_luma_fast(_luma: &[u8], _w: u32, _h: u32) -> Option<String> {
    None
}

#[cfg(feature = "qr")]
/// Budget for rxing's internal square matrix, which is allocated in full up
/// front for the requested dimensions; larger requests must not reach it.
const MATRIX_BYTE_BUDGET: u64 = 16 << 20;

#[cfg(feature = "qr")]
/// Budget for the RGBA output buffer produced from the matrix.
const RGBA_BYTE_BUDGET: usize = 64 << 20;

#[cfg(feature = "qr")]
#[must_use]
pub fn qr_rgba(url: &str, size: u32) -> Option<(u32, u32, Vec<u8>)> {
    use rxing::qrcode::QRCodeWriter;
    use rxing::{BarcodeFormat, EncodeHints, Writer};
    let hints = EncodeHints {
        Margin: Some("2".to_string()),
        ..Default::default()
    };
    (u64::from(size) * u64::from(size) <= MATRIX_BYTE_BUDGET * 8).then_some(())?;
    let side = i32::try_from(size).ok()?;
    let matrix = QRCodeWriter
        .encode_with_hints(url, &BarcodeFormat::QR_CODE, side, side, &hints)
        .ok()?;
    let w = matrix.getWidth();
    let h = matrix.getHeight();
    let scale = size.div_ceil(w.max(h));
    let out_w = w.checked_mul(scale)?;
    let out_h = h.checked_mul(scale)?;
    let len = (u64::from(out_w) * u64::from(out_h))
        .checked_mul(4)
        .and_then(|bytes| usize::try_from(bytes).ok())?;
    (len <= RGBA_BYTE_BUDGET).then_some(())?;
    let mut rgba = vec![0xFF; len];
    for y in 0..h {
        for x in 0..w {
            if matrix.get(x, y) {
                for sy in 0..scale {
                    let row =
                        (u64::from(y) * u64::from(scale) + u64::from(sy)) * u64::from(out_w) * 4;
                    for sx in 0..scale {
                        let idx = usize::try_from(
                            row + (u64::from(x) * u64::from(scale) + u64::from(sx)) * 4,
                        )
                        .ok()?;
                        rgba[idx] = 0;
                        rgba[idx + 1] = 0;
                        rgba[idx + 2] = 0;
                    }
                }
            }
        }
    }
    Some((out_w, out_h, rgba))
}

#[cfg(not(feature = "qr"))]
#[must_use]
pub fn qr_rgba(_url: &str, _size: u32) -> Option<(u32, u32, Vec<u8>)> {
    None
}
