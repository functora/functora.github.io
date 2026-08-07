#![allow(clippy::unwrap_used, clippy::expect_used)]
mod common;
use cryptonote::crypto::{decrypt_symmetric, encrypt_symmetric, CipherType};
use cryptonote::encoding::{build_url, decode_note, encode_note, extract_note_param, generate_qr_code, NoteData};

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
    let encrypted = encrypt_symmetric(plaintext, "password", CipherType::ChaCha20Poly1305).expect("Encryption failed");
    let note = NoteData::CipherText(encrypted);
    let encoded = encode_note(&note).expect("Encoding failed");
    let decoded = decode_note(&encoded).expect("Decoding failed");

    if let NoteData::CipherText(original) = note {
        match decoded {
            NoteData::CipherText(enc_data) => assert_eq!(original.ciphertext, enc_data.ciphertext),
            NoteData::PlainText(_) => panic!("Expected a CipherText note"),
        }
    }
}

#[test]
fn test_encode_decode_aes_encrypted() {
    common::fast_kdf();
    let plaintext = b"AES encrypted message";
    let encrypted = encrypt_symmetric(plaintext, "password", CipherType::Aes256Gcm).expect("Encryption failed");
    let note = NoteData::CipherText(encrypted);
    let encoded = encode_note(&note).expect("Encoding failed");
    let decoded = decode_note(&encoded).expect("Decoding failed");

    if let NoteData::CipherText(original) = note {
        match decoded {
            NoteData::CipherText(enc_data) => {
                assert_eq!(original.ciphertext, enc_data.ciphertext);
                assert_eq!(original.cipher, enc_data.cipher);
                assert_eq!(original.nonce, enc_data.nonce);
                assert_eq!(original.salt, enc_data.salt);
            }
            NoteData::PlainText(_) => panic!("Expected a CipherText note"),
        }
    }
}

#[test]
fn test_build_url_format() {
    let note = NoteData::PlainText("Test note".to_string());
    let url = build_url("https://example.com/view", &note).expect("URL build failed");
    assert!(url.starts_with("https://example.com/view?note="));
}

#[test]
fn test_build_url_format2() {
    let note = NoteData::PlainText("test".to_string());
    let url = build_url("https://example.com", &note).unwrap();
    assert!(url.contains("?note="));
    assert!(url.starts_with("https://example.com?note="));

    let url_with_query = build_url("https://example.com/?screen=view", &note).unwrap();
    assert!(url_with_query.contains("&note="));
    assert!(url_with_query.starts_with("https://example.com/?screen=view&note="));
}

#[test]
fn test_build_url_roundtrip() {
    let note = NoteData::PlainText("Round trip test".to_string());
    let url = build_url("https://example.com", &note).unwrap();
    let extracted = extract_note_param(&url).expect("Extraction failed");
    let decoded = decode_note(&extracted).expect("Decoding failed");
    match decoded {
        NoteData::PlainText(text) => assert_eq!(text, "Round trip test"),
        NoteData::CipherText(_) => panic!("Expected a PlainText note"),
    }
}

#[test]
fn test_build_url_roundtrip_encrypted() {
    common::fast_kdf();
    let plaintext = b"Encrypted round trip";
    let encrypted = encrypt_symmetric(plaintext, "pw", CipherType::Aes256Gcm).expect("Encryption failed");
    let note = NoteData::CipherText(encrypted);
    let url = build_url("https://example.com", &note).unwrap();
    let extracted = extract_note_param(&url).expect("Extraction failed");
    let decoded = decode_note(&extracted).expect("Decoding failed");
    let decrypted = match decoded {
        NoteData::CipherText(enc) => decrypt_symmetric(&enc, "pw").expect("Decryption failed"),
        NoteData::PlainText(_) => panic!("Expected a CipherText note"),
    };
    assert_eq!(plaintext.to_vec(), decrypted);
}

#[test]
fn test_qr_code_generation() {
    let url = "https://example.com/test";
    let svg = generate_qr_code(url).expect("QR generation failed");
    assert!(svg.contains("<svg"));
    assert!(svg.contains("</svg>"));
}

#[test]
fn test_invalid_base64() {
    let result = decode_note("not-valid-base64!!!");
    assert!(result.is_err());
}

#[test]
fn test_extract_note_param_basic() {
    let url = "https://example.com/?screen=view&note=abc123";
    let result = extract_note_param(url).expect("Extraction failed");
    assert_eq!(result, "abc123");
}

#[test]
fn test_extract_note_param_no_query() {
    let url = "https://example.com/";
    let result = extract_note_param(url);
    assert!(result.is_err());
}

#[test]
fn test_extract_note_param_no_note_param() {
    let url = "https://example.com/?screen=view";
    let result = extract_note_param(url);
    assert!(result.is_err());
}

#[test]
fn test_extract_note_param_first_param() {
    let url = "https://example.com/?note=abc&screen=view";
    let result = extract_note_param(url).expect("Extraction failed");
    assert_eq!(result, "abc");
}

#[test]
fn test_extract_note_param_url_encoded() {
    let url = "https://example.com/?note=abc%2Bdef";
    let result = extract_note_param(url).expect("Extraction failed");
    assert_eq!(result, "abc+def");
}

#[test]
fn test_extract_note_param_url_encoded_value_with_equals() {
    let url = "https://example.com/?note=abc%3Ddef";
    let result = extract_note_param(url).expect("Extraction failed");
    assert_eq!(result, "abc=def");
}

#[test]
fn test_build_url_and_extract_roundtrip() {
    let note = NoteData::PlainText("Test with special chars: +/=".to_string());
    let url = build_url("https://example.com", &note).unwrap();
    let extracted = extract_note_param(&url).expect("Extraction failed");
    let decoded = decode_note(&extracted).expect("Decoding failed");
    match decoded {
        NoteData::PlainText(text) => assert_eq!(text, "Test with special chars: +/="),
        NoteData::CipherText(_) => panic!("Expected a PlainText note"),
    }
}

#[test]
fn test_empty_note_param() {
    let url = "https://example.com/?note=";
    let result = extract_note_param(url).expect("Extraction failed");
    assert_eq!(result, "");
}

#[test]
fn test_note_data_serde_plaintext() {
    let note = NoteData::PlainText("hello".into());
    let json = serde_json::to_string(&note).unwrap();
    let back: NoteData = serde_json::from_str(&json).unwrap();
    match back {
        NoteData::PlainText(t) => assert_eq!(t, "hello"),
        NoteData::CipherText(_) => panic!("wrong variant"),
    }
}

#[test]
fn test_note_data_serde_ciphertext() {
    let ed = cryptonote::crypto::EncryptedNote {
        cipher: cryptonote::crypto::CipherType::Aes256Gcm,
        nonce: vec![1; 12],
        ciphertext: vec![2; 16],
        salt: vec![3; 32],
        kdf: cryptonote::crypto::Kdf::Argon2id,
    };
    let note = NoteData::CipherText(ed);
    let json = serde_json::to_string(&note).unwrap();
    let back: NoteData = serde_json::from_str(&json).unwrap();
    match back {
        NoteData::CipherText(e) => assert_eq!(e.ciphertext, vec![2; 16]),
        NoteData::PlainText(_) => panic!("wrong variant"),
    }
}

#[test]
fn test_extract_note_param_multiple_note_params_takes_first() {
    let url = "https://example.com/?note=first&note=second";
    let result = extract_note_param(url).expect("Extraction failed");
    assert_eq!(result, "first");
}

#[test]
fn test_extract_note_param_with_fragment() {
    let url = "https://example.com/?note=abc#section";
    let result = extract_note_param(url).expect("Extraction failed");
    assert_eq!(result, "abc#section");
}

#[test]
fn test_generate_qr_code_svg_structure() {
    let svg = cryptonote::encoding::generate_qr_code("https://test.com").expect("QR gen failed");
    assert!(svg.starts_with("<svg"));
    assert!(svg.ends_with("</svg>\n") || svg.ends_with("</svg>"));
    assert!(svg.contains("viewBox"));
}

#[test]
fn test_generate_qr_code_long_url() {
    let long = "https://example.com/?screen=view&note=abc123def456ghi789jkl012";
    let svg = cryptonote::encoding::generate_qr_code(long).expect("QR gen failed");
    assert!(svg.contains("<svg"));
}
