use crate::crypto::EncryptedData;
use crate::error::AppError;
use base64::{
    Engine, engine::general_purpose::URL_SAFE_NO_PAD,
};
use qrcode::{QrCode, render::svg};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum NoteData {
    PlainText(String),
    CipherText(EncryptedData),
}

pub fn encode_note(
    note: &NoteData,
) -> Result<String, AppError> {
    serde_json::to_vec(note)
        .map(|bytes| URL_SAFE_NO_PAD.encode(bytes))
        .map_err(Into::into)
}

pub fn decode_note(
    encoded: &str,
) -> Result<NoteData, AppError> {
    let bytes = URL_SAFE_NO_PAD.decode(encoded)?;
    serde_json::from_slice(&bytes).map_err(Into::into)
}

pub fn build_url(
    base_url: &str,
    note: &NoteData,
) -> Result<String, AppError> {
    encode_note(note).map(|encoded| {
        format!("{}?note={}", base_url, encoded)
    })
}



pub fn generate_qr_code(
    url: &str,
) -> Result<String, AppError> {
    Ok(QrCode::new(url).map(|code| {
        code.render::<svg::Color>()
            .min_dimensions(200, 200)
            .build()
    })?)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_encode_decode_plaintext() {
        let note =
            NoteData::PlainText("hello world".to_string());
        let encoded = encode_note(&note).unwrap();
        let decoded = decode_note(&encoded).unwrap();
        match decoded {
            NoteData::PlainText(s) => {
                assert_eq!(s, "hello world")
            }
            _ => panic!("Expected PlainText"),
        }
    }

    #[test]
    fn test_build_url_format() {
        let note = NoteData::PlainText("test".to_string());
        let url = build_url("https://example.com", &note)
            .unwrap();
        assert!(url.contains("?note="));
        assert!(
            url.starts_with("https://example.com?note=")
        );
    }


}
