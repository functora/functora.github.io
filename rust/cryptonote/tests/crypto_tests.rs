#![allow(clippy::unwrap_used, clippy::expect_used)]
mod common;
use cryptonote::crypto::{
    decrypt_symmetric, derive_key, encrypt_symmetric, stream_decrypt_symmetric, stream_encrypt_symmetric, CipherType,
    EncryptedNote, Kdf, KEY_SIZE,
};

#[test]
fn test_symmetric_chacha20_roundtrip() {
    common::fast_kdf();
    let plaintext = b"Hello, Cryptonote!";
    let password = "test_password_123";
    let encrypted = encrypt_symmetric(plaintext, password, CipherType::ChaCha20Poly1305).expect("Encryption failed");
    let decrypted = decrypt_symmetric(&encrypted, password).expect("Decryption failed");
    assert_eq!(plaintext.to_vec(), decrypted);
}

#[test]
fn test_symmetric_aes_roundtrip() {
    common::fast_kdf();
    let plaintext = b"Secret message with AES";
    let password = "strong_password";
    let encrypted = encrypt_symmetric(plaintext, password, CipherType::Aes256Gcm).expect("Encryption failed");
    let decrypted = decrypt_symmetric(&encrypted, password).expect("Decryption failed");
    assert_eq!(plaintext.to_vec(), decrypted);
}

#[test]
fn test_symmetric_wrong_password() {
    common::fast_kdf();
    let plaintext = b"Test data";
    let encrypted = encrypt_symmetric(plaintext, "correct", CipherType::ChaCha20Poly1305).expect("Encryption failed");
    let result = decrypt_symmetric(&encrypted, "wrong");
    assert!(result.is_err());
}

#[test]
fn test_key_derivation_consistency() {
    common::fast_kdf();
    let password = "test";
    let salt = vec![1u8; KEY_SIZE];
    let key1 = derive_key(password, &salt, Kdf::Argon2id).expect("Key derivation failed");
    let key2 = derive_key(password, &salt, Kdf::Argon2id).expect("Key derivation failed");
    assert_eq!(key1, key2);
}

#[test]
fn test_derive_key_different_salts() {
    common::fast_kdf();
    let password = "test_password";
    let salt1 = vec![1u8; KEY_SIZE];
    let salt2 = vec![2u8; KEY_SIZE];
    let key1 = derive_key(password, &salt1, Kdf::Argon2id).expect("Key derivation failed");
    let key2 = derive_key(password, &salt2, Kdf::Argon2id).expect("Key derivation failed");
    assert_ne!(key1, key2);
}

#[test]
fn test_derive_key_empty_password() {
    common::fast_kdf();
    let salt = vec![1u8; KEY_SIZE];
    let key = derive_key("", &salt, Kdf::Argon2id);
    assert!(key.is_ok());
    assert_eq!(key.unwrap().len(), 32);
}

#[test]
fn test_encrypt_decrypt_empty_plaintext() {
    common::fast_kdf();
    let plaintext = b"";
    let password = "password";
    let encrypted = encrypt_symmetric(plaintext, password, CipherType::Aes256Gcm).expect("Encryption failed");
    let decrypted = decrypt_symmetric(&encrypted, password).expect("Decryption failed");
    assert_eq!(plaintext.to_vec(), decrypted);
}

#[test]
fn test_encrypt_decrypt_large_plaintext() {
    common::fast_kdf();
    let plaintext = vec![42u8; 10000];
    let password = "password";
    let encrypted = encrypt_symmetric(&plaintext, password, CipherType::Aes256Gcm).expect("Encryption failed");
    let decrypted = decrypt_symmetric(&encrypted, password).expect("Decryption failed");
    assert_eq!(plaintext, decrypted);
}

#[test]
fn test_different_nonces_for_same_input() {
    common::fast_kdf();
    let plaintext = b"Same message";
    let password = "password";
    let encrypted1 = encrypt_symmetric(plaintext, password, CipherType::ChaCha20Poly1305).expect("Encryption failed");
    let encrypted2 = encrypt_symmetric(plaintext, password, CipherType::ChaCha20Poly1305).expect("Encryption failed");
    assert_ne!(encrypted1.nonce, encrypted2.nonce);
    assert_ne!(encrypted1.ciphertext, encrypted2.ciphertext);
}

#[test]
fn test_derive_key_32_bytes() {
    common::fast_kdf();
    let salt = vec![1u8; 32];
    let key = derive_key("password", &salt, Kdf::Argon2id).expect("Key derivation failed");
    assert_eq!(key.len(), 32);
}

#[test]
fn test_derive_key_long_password() {
    common::fast_kdf();
    let long_pw = "a".repeat(100);
    let salt = vec![1u8; 32];
    let key = derive_key(&long_pw, &salt, Kdf::Argon2id).expect("Key derivation failed");
    assert_eq!(key.len(), 32);
}

#[test]
fn test_encrypt_decrypt_with_long_password() {
    common::fast_kdf();
    let long_pw = "x".repeat(128);
    let plaintext = b"test data with long password";
    let encrypted = encrypt_symmetric(plaintext, &long_pw, CipherType::Aes256Gcm).expect("Encryption failed");
    let decrypted = decrypt_symmetric(&encrypted, &long_pw).expect("Decryption failed");
    assert_eq!(plaintext.to_vec(), decrypted);
}

#[test]
fn test_decrypt_tampered_ciphertext_fails() {
    common::fast_kdf();
    let plaintext = b"original data";
    let mut encrypted = encrypt_symmetric(plaintext, "pw", CipherType::ChaCha20Poly1305).expect("Encryption failed");
    encrypted.ciphertext[0] ^= 0xFF;
    let result = decrypt_symmetric(&encrypted, "pw");
    assert!(result.is_err());
}

#[test]
fn test_decrypt_tampered_nonce_fails() {
    common::fast_kdf();
    let plaintext = b"data";
    let mut encrypted = encrypt_symmetric(plaintext, "pw", CipherType::Aes256Gcm).expect("Encryption failed");
    encrypted.nonce[0] ^= 0xFF;
    let result = decrypt_symmetric(&encrypted, "pw");
    assert!(result.is_err());
}

#[test]
fn test_decrypt_wrong_nonce_length_returns_error() {
    common::fast_kdf();
    let mut encrypted = encrypt_symmetric(b"data", "pw", CipherType::ChaCha20Poly1305).expect("Encryption failed");
    encrypted.nonce.truncate(5);
    let result = decrypt_symmetric(&encrypted, "pw");
    assert!(matches!(
        result,
        Err(cryptonote::AppError::FunctoraDioxus(
            functora_dioxus::Error::InvalidFormat(_)
        ))
    ));
}

#[test]
fn test_decrypt_wrong_salt_length_returns_error() {
    common::fast_kdf();
    let mut encrypted = encrypt_symmetric(b"data", "pw", CipherType::Aes256Gcm).expect("Encryption failed");
    encrypted.salt.clear();
    let result = decrypt_symmetric(&encrypted, "pw");
    assert!(matches!(
        result,
        Err(cryptonote::AppError::FunctoraDioxus(
            functora_dioxus::Error::InvalidFormat(_)
        ))
    ));
}

#[test]
fn test_encrypt_empty_password() {
    common::fast_kdf();
    let plaintext = b"data with no password";
    let encrypted = encrypt_symmetric(plaintext, "", CipherType::ChaCha20Poly1305).expect("Encryption failed");
    let decrypted = decrypt_symmetric(&encrypted, "").expect("Decryption failed");
    assert_eq!(plaintext.to_vec(), decrypted);
}

#[test]
fn test_encrypt_decrypt_large_plaintext_chacha() {
    common::fast_kdf();
    let plaintext = vec![7u8; 5000];
    let encrypted = encrypt_symmetric(&plaintext, "pw", CipherType::ChaCha20Poly1305).expect("Encryption failed");
    let decrypted = decrypt_symmetric(&encrypted, "pw").expect("Decryption failed");
    assert_eq!(plaintext, decrypted);
}

#[test]
fn test_new_notes_use_argon2id() {
    common::fast_kdf();
    let encrypted = encrypt_symmetric(b"data", "pw", CipherType::Aes256Gcm).expect("Encryption failed");
    assert_eq!(encrypted.kdf, Kdf::Argon2id);
}

#[test]
fn test_decrypt_tampered_cipher_fails() {
    common::fast_kdf();
    let encrypted = encrypt_symmetric(b"data", "pw", CipherType::ChaCha20Poly1305).expect("Encryption failed");
    let mut tampered = encrypted.clone();
    tampered.cipher = CipherType::Aes256Gcm;
    assert!(decrypt_symmetric(&tampered, "pw").is_err());
}

#[test]
fn test_ciphertext_contains_no_plaintext() {
    common::fast_kdf();
    let plaintext = b"top-secret-plaintext-marker";
    let encrypted = encrypt_symmetric(plaintext, "pw", CipherType::Aes256Gcm).expect("Encryption failed");
    assert!(!encrypted.ciphertext.windows(plaintext.len()).any(|w| w == plaintext));
}

#[test]
fn test_metadata_contains_no_password_or_key() {
    common::fast_kdf();
    let encrypted =
        encrypt_symmetric(b"data", "super-secret-password", CipherType::Aes256Gcm).expect("Encryption failed");
    let json = serde_json::to_string(&encrypted).expect("Serialize failed");
    assert!(!json.contains("super-secret-password"));
    assert_eq!(encrypted.kdf, Kdf::Argon2id);
    assert_eq!(encrypted.salt.len(), 32);
    assert_eq!(encrypted.nonce.len(), 12);
}

#[test]
fn test_old_metadata_json_rejected_without_kdf() {
    common::fast_kdf();
    let json = r#"{"cipher":"Aes256Gcm","nonce":[1,2,3],"ciphertext":[4,5,6],"salt":[7,8,9]}"#;
    let result: Result<EncryptedNote, _> = serde_json::from_str(json);
    assert!(result.is_err());
}

const STREAM_CHUNK: usize = 64 * 1024;
const STREAM_TAG: usize = 16;

fn stream_roundtrip(size: usize, cipher: CipherType) {
    let plaintext: Vec<u8> = (0..size).map(|i| u8::try_from(i % 251).unwrap()).collect();
    let encrypted = stream_encrypt_symmetric(&plaintext, "pw", cipher).expect("Stream encryption failed");
    assert_eq!(
        encrypted.ciphertext.len(),
        size + plaintext.len().div_ceil(STREAM_CHUNK) * STREAM_TAG
    );
    let decrypted = stream_decrypt_symmetric(&encrypted, "pw").expect("Stream decryption failed");
    assert_eq!(plaintext, decrypted);
}

#[test]
fn test_stream_empty_roundtrip() {
    common::fast_kdf();
    for cipher in [CipherType::Aes256Gcm, CipherType::ChaCha20Poly1305] {
        stream_roundtrip(0, cipher);
    }
}

#[test]
fn test_stream_exact_chunk_roundtrip() {
    common::fast_kdf();
    for cipher in [CipherType::Aes256Gcm, CipherType::ChaCha20Poly1305] {
        stream_roundtrip(STREAM_CHUNK, cipher);
    }
}

#[test]
fn test_stream_chunk_plus_one_roundtrip() {
    common::fast_kdf();
    for cipher in [CipherType::Aes256Gcm, CipherType::ChaCha20Poly1305] {
        stream_roundtrip(STREAM_CHUNK + 1, cipher);
    }
}

#[test]
fn test_stream_multi_chunk_roundtrip() {
    common::fast_kdf();
    for cipher in [CipherType::Aes256Gcm, CipherType::ChaCha20Poly1305] {
        stream_roundtrip(STREAM_CHUNK * 3 + 42, cipher);
    }
}

#[test]
fn test_stream_wrong_password() {
    common::fast_kdf();
    let plaintext = b"stream data across chunks!".repeat(4);
    let encrypted = stream_encrypt_symmetric(&plaintext, "pw", CipherType::Aes256Gcm).expect("Encryption failed");
    assert!(stream_decrypt_symmetric(&encrypted, "nope").is_err());
}

#[test]
fn test_stream_tampered_chunk_fails() {
    common::fast_kdf();
    let plaintext: Vec<u8> = (0..STREAM_CHUNK * 2 + 8)
        .map(|i| u8::try_from(i % 256).unwrap())
        .collect();
    let encrypted =
        stream_encrypt_symmetric(&plaintext, "pw", CipherType::ChaCha20Poly1305).expect("Encryption failed");
    let mut tampered = encrypted.clone();
    let last = tampered.ciphertext.len() - 1;
    tampered.ciphertext[last] ^= 0xff;
    assert!(stream_decrypt_symmetric(&tampered, "pw").is_err());
}

#[test]
fn test_stream_truncated_chunk_fails() {
    common::fast_kdf();
    let plaintext: Vec<u8> = (0..STREAM_CHUNK * 2 + 8)
        .map(|i| u8::try_from(i % 256).unwrap())
        .collect();
    let encrypted = stream_encrypt_symmetric(&plaintext, "pw", CipherType::Aes256Gcm).expect("Encryption failed");
    let mut truncated = encrypted.clone();
    truncated.ciphertext = truncated.ciphertext[..truncated.ciphertext.len() - STREAM_TAG].to_vec();
    assert!(stream_decrypt_symmetric(&truncated, "pw").is_err());
}

#[test]
fn test_stream_reordered_chunks_fail() {
    common::fast_kdf();
    let plaintext: Vec<u8> = (0..STREAM_CHUNK * 2).map(|i| u8::try_from(i % 256).unwrap()).collect();
    let encrypted =
        stream_encrypt_symmetric(&plaintext, "pw", CipherType::ChaCha20Poly1305).expect("Encryption failed");
    let a = encrypted.ciphertext[..STREAM_CHUNK + STREAM_TAG].to_vec();
    let b = encrypted.ciphertext[STREAM_CHUNK + STREAM_TAG..].to_vec();
    let mut reordered = encrypted.clone();
    reordered.ciphertext = b.into_iter().chain(a).collect();
    let result = stream_decrypt_symmetric(&reordered, "pw");
    assert!(result.is_err() || result.unwrap() != plaintext);
}
