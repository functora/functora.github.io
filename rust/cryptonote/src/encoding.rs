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
        format!("{}#note={}", base_url, encoded)
    })
}

pub fn parse_url(url: &str) -> Result<NoteData, AppError> {
    url.split("#note=")
        .nth(1)
        .ok_or_else(|| AppError::InvalidUrl)
        .and_then(decode_note)
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
    use crate::crypto::{CipherType, encrypt_symmetric};

    #[test]
    fn test_encode_decode_plaintext() {
        let note = NoteData::PlainText(
            "Hello, World!".to_string(),
        );
        let encoded =
            encode_note(&note).expect("Encoding failed");
        let decoded =
            decode_note(&encoded).expect("Decoding failed");
        match decoded {
            NoteData::PlainText(text) => {
                assert_eq!(text, "Hello, World!")
            }
            _ => panic!("Expected PlainText"),
        }
    }

    #[test]
    fn test_encode_decode_encrypted() {
        let plaintext = b"Secret message";
        let encrypted = encrypt_symmetric(
            plaintext,
            "password",
            CipherType::ChaCha20Poly1305,
        )
        .expect("Encryption failed");
        let note = NoteData::CipherText(encrypted);
        let encoded =
            encode_note(&note).expect("Encoding failed");
        let decoded =
            decode_note(&encoded).expect("Decoding failed");

        match decoded {
            NoteData::CipherText(enc_data) => {
                assert_eq!(
                    note.as_ciphertext()
                        .unwrap()
                        .ciphertext,
                    enc_data.ciphertext
                );
            }
            _ => panic!("Expected CipherText"),
        }
    }

    #[test]
    fn test_build_parse_url() {
        let note =
            NoteData::PlainText("Test note".to_string());
        let url =
            build_url("https://example.com/view", &note)
                .expect("URL build failed");
        assert!(
            url.starts_with(
                "https://example.com/view#note="
            )
        );
        let parsed =
            parse_url(&url).expect("URL parse failed");
        match parsed {
            NoteData::PlainText(text) => {
                assert_eq!(text, "Test note")
            }
            _ => panic!("Expected PlainText"),
        }
    }

    #[test]
    fn test_qr_code_generation() {
        let url = "https://example.com/test";
        let svg = generate_qr_code(url)
            .expect("QR generation failed");
        assert!(svg.contains("<svg"));
        assert!(svg.contains("</svg>"));
    }

    #[test]
    fn test_invalid_base64() {
        let result = decode_note("not-valid-base64!!!");
        assert!(result.is_err());
    }

    #[test]
    fn test_invalid_url_format() {
        let result =
            parse_url("https://example.com/no-note-param");
        assert!(result.is_err());
    }
}

impl NoteData {
    pub fn as_ciphertext(&self) -> Option<&EncryptedData> {
        match self {
            NoteData::CipherText(data) => Some(data),
            _ => None,
        }
    }
}
