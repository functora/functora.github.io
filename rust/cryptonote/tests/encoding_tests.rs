use cryptonote::crypto::{CipherType, encrypt_symmetric};
use cryptonote::encoding::{
    NoteData, build_url, decode_note, encode_note,
    generate_qr_code, parse_url,
};

#[test]
fn test_encode_decode_plaintext() {
    let note =
        NoteData::PlainText("Hello, World!".to_string());
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

    if let NoteData::CipherText(original) = note {
        match decoded {
            NoteData::CipherText(enc_data) => {
                assert_eq!(
                    original.ciphertext, enc_data.ciphertext
                );
            }
            _ => panic!("Expected CipherText"),
        }
    }
}

#[test]
fn test_build_parse_url() {
    let note = NoteData::PlainText("Test note".to_string());
    let url = build_url("https://example.com/view", &note)
        .expect("URL build failed");
    assert!(
        url.starts_with("https://example.com/view?note=")
    );
    let parsed = parse_url(&url).expect("URL parse failed");
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
