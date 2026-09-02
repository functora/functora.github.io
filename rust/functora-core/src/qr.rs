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
        .chunks_exact(4)
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
#[must_use]
pub fn decode_qr_luma(luma: &[u8], w: u32, h: u32) -> Option<String> {
    (w != 0 && h != 0).then_some(())?;
    QRCodeReader::new()
        .immutable_decode_with_hints(
            &mut BinaryBitmap::new(HybridBinarizer::new(
                Luma8LuminanceSource::new(luma.to_vec(), w, h).ok()?,
            )),
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

#[cfg(feature = "qr")]
#[must_use]
pub fn qr_rgba(url: &str, size: u32) -> Option<(u32, u32, Vec<u8>)> {
    use rxing::qrcode::QRCodeWriter;
    use rxing::{BarcodeFormat, EncodeHints, Writer};
    let hints = EncodeHints {
        Margin: Some("2".to_string()),
        ..Default::default()
    };
    let side = i32::try_from(size).ok()?;
    let matrix = QRCodeWriter
        .encode_with_hints(url, &BarcodeFormat::QR_CODE, side, side, &hints)
        .ok()?;
    let w = matrix.getWidth();
    let h = matrix.getHeight();
    let scale = size.div_ceil(w.max(h));
    let out_w = w * scale;
    let out_h = h * scale;
    let mut rgba = vec![0xFF; usize::try_from(out_w * out_h * 4).ok()?];
    for y in 0..h {
        for x in 0..w {
            if matrix.get(x, y) {
                let row_start = usize::try_from(y * out_w * 4 * scale).ok()?;
                for sy in 0..scale {
                    let row = row_start + usize::try_from(sy * out_w * 4).ok()?;
                    for sx in 0..scale {
                        let idx = row + usize::try_from((x * scale + sx) * 4).ok()?;
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
