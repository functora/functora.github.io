use cryptonote::qr_decode::{
    decode_qr_luma, decode_qr_rgba,
};
use rxing::qrcode::QRCodeWriter;
use rxing::{BarcodeFormat, EncodeHints, Writer};

fn generate_qr_luma(data: &str) -> (Vec<u8>, u32, u32) {
    let matrix = QRCodeWriter
        .encode_with_hints(
            data,
            &BarcodeFormat::QR_CODE,
            200,
            200,
            &EncodeHints {
                Margin: Some("0".to_string()),
                ..Default::default()
            },
        )
        .unwrap();
    let w = matrix.getWidth();
    let h = matrix.getHeight();
    let mut luma = Vec::with_capacity((w * h) as usize);
    for y in 0..h {
        for x in 0..w {
            luma.push(if matrix.get(x, y) {
                0
            } else {
                255
            });
        }
    }
    (luma, w, h)
}

fn luma_to_rgba(luma: &[u8], w: u32, h: u32) -> Vec<u8> {
    let mut rgba = Vec::with_capacity((w * h * 4) as usize);
    for &v in luma {
        rgba.extend_from_slice(&[v, v, v, 255]);
    }
    rgba
}

#[test]
fn test_decode_qr_luma_simple() {
    let (luma, w, h) = generate_qr_luma("Hello, World!");
    let result = decode_qr_luma(&luma, w, h);
    assert_eq!(result, Some("Hello, World!".to_string()));
}

#[test]
fn test_decode_qr_rgba_simple() {
    let (luma, w, h) = generate_qr_luma("Hello, World!");
    let rgba = luma_to_rgba(&luma, w, h);
    let result = decode_qr_rgba(&rgba, w, h);
    assert_eq!(result, Some("Hello, World!".to_string()));
}

#[test]
fn test_decode_qr_luma_url() {
    let url = "https://functora.github.io/?screen=view&note=test123";
    let (luma, w, h) = generate_qr_luma(url);
    let result = decode_qr_luma(&luma, w, h);
    assert_eq!(result, Some(url.to_string()));
}

#[test]
fn test_decode_qr_luma_minimal() {
    let (luma, w, h) = generate_qr_luma("a");
    let result = decode_qr_luma(&luma, w, h);
    assert_eq!(result, Some("a".to_string()));
}

#[test]
fn test_decode_qr_rgba_all_opaque() {
    let (luma, w, h) = generate_qr_luma("test");
    let rgba = luma_to_rgba(&luma, w, h);
    let result = decode_qr_rgba(&rgba, w, h);
    assert_eq!(result, Some("test".to_string()));
}

#[test]
fn test_decode_qr_luma_noise_returns_none() {
    let noise: Vec<u8> = (0..10000)
        .map(|i| ((i * 7919) % 256) as u8)
        .collect();
    let result = decode_qr_luma(&noise, 100, 100);
    assert_eq!(result, None);
}

#[test]
fn test_decode_qr_luma_unicode() {
    let text = "Привет мир! ¡Hola! こんにちは";
    let (luma, w, h) = generate_qr_luma(text);
    let result = decode_qr_luma(&luma, w, h);
    assert_eq!(result, Some(text.to_string()));
}
