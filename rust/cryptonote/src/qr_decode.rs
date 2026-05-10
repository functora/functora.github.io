use rxing::common::HybridBinarizer;
use rxing::qrcode::QRCodeReader;
use rxing::{
    BinaryBitmap, ImmutableReader, Luma8LuminanceSource,
};

pub fn decode_qr_rgba(
    rgba: &[u8],
    w: u32,
    h: u32,
) -> Option<String> {
    let luma: Vec<u8> = rgba
        .chunks_exact(4)
        .map(|px| {
            if px[3] == 0 {
                0xFF
            } else {
                ((306 * u64::from(px[0])
                    + 601 * u64::from(px[1])
                    + 117 * u64::from(px[2])
                    + 0x200)
                    >> 10) as u8
            }
        })
        .collect();
    QRCodeReader::new()
        .immutable_decode(&mut BinaryBitmap::new(
            HybridBinarizer::new(
                Luma8LuminanceSource::new(luma, w, h),
            ),
        ))
        .ok()
        .map(|r| r.getText().to_owned())
}

pub fn decode_qr_luma(
    luma: &[u8],
    w: u32,
    h: u32,
) -> Option<String> {
    QRCodeReader::new()
        .immutable_decode(&mut BinaryBitmap::new(
            HybridBinarizer::new(
                Luma8LuminanceSource::new(
                    luma.to_vec(),
                    w,
                    h,
                ),
            ),
        ))
        .ok()
        .map(|r| r.getText().to_owned())
}
