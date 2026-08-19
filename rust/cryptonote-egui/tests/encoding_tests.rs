#![allow(clippy::unwrap_used, clippy::expect_used)]
mod common;
use cryptonote_egui::crypto::{encrypt_symmetric, CipherType};
use cryptonote_egui::encoding::{
    build_url, decode_note, encode_note, extract_note_param, generate_qr, NoteData,
};

#[test]
fn test_encode_decode_plaintext() {
    let note = NoteData::PlainText("Hello, World!".to_string());
    let encoded = encode_note(&note).expect("Encoding failed");
    let decoded = decode_note(&encoded).expect("Decoding failed");
    match decoded {
        NoteData::PlainText(text) => assert_eq!(text, "Hello, World!"),
        NoteData::CipherText(_) => panic!("Expected a PlainText note"),
    }
}

#[test]
fn test_encode_decode_plaintext_unicode() {
    let note = NoteData::PlainText("Привет мир! ¡Hola! こんにちは".to_string());
    let encoded = encode_note(&note).expect("Encoding failed");
    let decoded = decode_note(&encoded).expect("Decoding failed");
    match decoded {
        NoteData::PlainText(text) => assert_eq!(text, "Привет мир! ¡Hola! こんにちは"),
        NoteData::CipherText(_) => panic!("Expected a PlainText note"),
    }
}

#[test]
fn test_encode_decode_plaintext_empty() {
    let note = NoteData::PlainText(String::new());
    let encoded = encode_note(&note).expect("Encoding failed");
    let decoded = decode_note(&encoded).expect("Decoding failed");
    match decoded {
        NoteData::PlainText(text) => assert!(text.is_empty()),
        NoteData::CipherText(_) => panic!("Expected a PlainText note"),
    }
}

#[test]
fn test_encode_decode_encrypted() {
    common::fast_kdf();
    let plaintext = b"Secret message";
    let encrypted = encrypt_symmetric(plaintext, "password", CipherType::ChaCha20Poly1305)
        .expect("Encryption failed");
    let note = NoteData::CipherText(encrypted);
    let encoded = encode_note(&note).expect("Encoding failed");
    let decoded = decode_note(&encoded).expect("Decoding failed");

    if let NoteData::CipherText(original) = note {
        match decoded {
            NoteData::CipherText(enc_data) => {
                assert_eq!(original.ciphertext, enc_data.ciphertext);
            }
            NoteData::PlainText(_) => panic!("Expected a CipherText note"),
        }
    }
}

#[test]
fn test_encode_decode_aes_encrypted() {
    common::fast_kdf();
    let plaintext = b"AES encrypted message";
    let encrypted =
        encrypt_symmetric(plaintext, "password", CipherType::Aes256Gcm).expect("Encryption failed");
    let note = NoteData::CipherText(encrypted);
    let encoded = encode_note(&note).expect("Encoding failed");
    let decoded = decode_note(&encoded).expect("Decoding failed");
    if let NoteData::CipherText(original) = note {
        match decoded {
            NoteData::CipherText(enc_data) => {
                assert_eq!(original.ciphertext, enc_data.ciphertext);
            }
            NoteData::PlainText(_) => panic!("Expected a CipherText note"),
        }
    }
}

#[test]
fn test_build_url_includes_note_param() {
    let note = NoteData::PlainText("Hello from URL".to_string());
    let url = build_url("https://example.com/?screen=open", &note).expect("URL build failed");
    assert!(url.starts_with("https://example.com/?screen=open&note="));
}

#[test]
fn test_build_url_encrypted_note() {
    common::fast_kdf();
    let encrypted = encrypt_symmetric(b"data", "password", CipherType::ChaCha20Poly1305)
        .expect("Encryption failed");
    let note = NoteData::CipherText(encrypted);
    let url = build_url("https://example.com/", &note).expect("URL build failed");
    assert!(url.starts_with("https://example.com/?note="));
}

#[test]
fn test_extract_note_param_roundtrip() {
    let note = NoteData::PlainText("Roundtrip me".to_string());
    let url = build_url("https://example.com/", &note).expect("URL build failed");
    let param = extract_note_param(&url).expect("Param extraction failed");
    let decoded = decode_note(&param).expect("Decoding failed");
    match decoded {
        NoteData::PlainText(text) => assert_eq!(text, "Roundtrip me"),
        NoteData::CipherText(_) => panic!("Expected a PlainText note"),
    }
}

#[test]
fn test_extract_note_param_missing() {
    assert!(extract_note_param("https://example.com/?screen=open").is_err());
}

#[test]
fn test_decode_garbage_fails() {
    assert!(decode_note("not-base64-at-all-%%%").is_err());
}

#[test]
fn test_generate_qr_code_plaintext() {
    let note = NoteData::PlainText("QR me".to_string());
    let url = build_url("https://example.com/", &note).expect("URL build failed");
    let qr = generate_qr(&url).expect("QR generation failed");
    assert!(qr.starts_with("<svg"));
}

#[test]
fn test_generate_qr_code_encrypted() {
    common::fast_kdf();
    let encrypted =
        encrypt_symmetric(b"data", "password", CipherType::Aes256Gcm).expect("Encryption failed");
    let note = NoteData::CipherText(encrypted);
    let url = build_url("https://example.com/", &note).expect("URL build failed");
    let qr = generate_qr(&url).expect("QR generation failed");
    assert!(qr.starts_with("<svg"));
}

#[test]
fn test_build_url_keeps_existing_params() {
    let note = NoteData::PlainText("keep params".to_string());
    let url =
        build_url("https://example.com/?screen=open&lang=es", &note).expect("URL build failed");
    assert!(url.contains("lang=es"));
    assert!(url.contains("note="));
}
