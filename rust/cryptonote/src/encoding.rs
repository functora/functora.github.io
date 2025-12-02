use crate::crypto::EncryptedData;
use base64::{
    Engine, engine::general_purpose::URL_SAFE_NO_PAD,
};
use qrcode::{QrCode, render::svg};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NoteData {
    pub content: Vec<u8>,
    pub encrypted: Option<EncryptedData>,
}

pub fn encode_note(
    note: &NoteData,
) -> Result<String, String> {
    serde_json::to_vec(note)
        .map_err(|e| format!("Serialization failed: {}", e))
        .map(|bytes| URL_SAFE_NO_PAD.encode(bytes))
}

pub fn decode_note(
    encoded: &str,
) -> Result<NoteData, String> {
    URL_SAFE_NO_PAD
        .decode(encoded)
        .map_err(|e| format!("Base64 decode failed: {}", e))
        .and_then(|bytes| {
            serde_json::from_slice(&bytes).map_err(|e| {
                format!("Deserialization failed: {}", e)
            })
        })
}

pub fn build_url(
    base_url: &str,
    note: &NoteData,
) -> Result<String, String> {
    encode_note(note).map(|encoded| {
        format!("{}#note={}", base_url, encoded)
    })
}

pub fn parse_url(url: &str) -> Result<NoteData, String> {
    url.split("#note=")
        .nth(1)
        .ok_or_else(|| "Invalid URL format".to_string())
        .and_then(decode_note)
}

pub fn generate_qr_code(
    url: &str,
) -> Result<String, String> {
    QrCode::new(url)
        .map_err(|e| {
            format!("QR code generation failed: {}", e)
        })
        .map(|code| {
            code.render::<svg::Color>()
                .min_dimensions(200, 200)
                .build()
        })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::crypto::{CipherType, encrypt_symmetric};

    #[test]
    fn test_encode_decode_plaintext() {
        let note = NoteData {
            content: b"Hello, World!".to_vec(),
            encrypted: None,
        };
        let encoded =
            encode_note(&note).expect("Encoding failed");
        let decoded =
            decode_note(&encoded).expect("Decoding failed");
        assert_eq!(note.content, decoded.content);
        assert!(decoded.encrypted.is_none());
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
        let note = NoteData {
            content: vec![],
            encrypted: Some(encrypted),
        };
        let encoded =
            encode_note(&note).expect("Encoding failed");
        let decoded =
            decode_note(&encoded).expect("Decoding failed");
        assert_eq!(
            note.encrypted.unwrap().ciphertext,
            decoded.encrypted.unwrap().ciphertext
        );
    }

    #[test]
    fn test_build_parse_url() {
        let note = NoteData {
            content: b"Test note".to_vec(),
            encrypted: None,
        };
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
        assert_eq!(note.content, parsed.content);
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
