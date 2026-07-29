use cryptonote::crypto::{decrypt_symmetric, derive_key, encrypt_symmetric, CipherType, KEY_SIZE};

#[test]
fn test_symmetric_chacha20_roundtrip() {
    let plaintext = b"Hello, Cryptonote!";
    let password = "test_password_123";
    let encrypted = encrypt_symmetric(plaintext, password, CipherType::ChaCha20Poly1305).expect("Encryption failed");
    let decrypted = decrypt_symmetric(&encrypted, password).expect("Decryption failed");
    assert_eq!(plaintext.to_vec(), decrypted);
}

#[test]
fn test_symmetric_aes_roundtrip() {
    let plaintext = b"Secret message with AES";
    let password = "strong_password";
    let encrypted = encrypt_symmetric(plaintext, password, CipherType::Aes256Gcm).expect("Encryption failed");
    let decrypted = decrypt_symmetric(&encrypted, password).expect("Decryption failed");
    assert_eq!(plaintext.to_vec(), decrypted);
}

#[test]
fn test_symmetric_wrong_password() {
    let plaintext = b"Test data";
    let encrypted = encrypt_symmetric(plaintext, "correct", CipherType::ChaCha20Poly1305).expect("Encryption failed");
    let result = decrypt_symmetric(&encrypted, "wrong");
    assert!(result.is_err());
}

#[test]
fn test_key_derivation_consistency() {
    let password = "test";
    let salt = vec![1u8; KEY_SIZE];
    let key1 = derive_key(password, &salt, CipherType::ChaCha20Poly1305).expect("Key derivation failed");
    let key2 = derive_key(password, &salt, CipherType::ChaCha20Poly1305).expect("Key derivation failed");
    assert_eq!(key1, key2);
}

#[test]
fn test_derive_key_different_ciphers_same_password() {
    let password = "test_password";
    let salt = vec![42u8; KEY_SIZE];
    let key_chacha = derive_key(password, &salt, CipherType::ChaCha20Poly1305).expect("Key derivation failed");
    let key_aes = derive_key(password, &salt, CipherType::Aes256Gcm).expect("Key derivation failed");
    assert_eq!(key_chacha, key_aes);
}

#[test]
fn test_derive_key_different_salts() {
    let password = "test_password";
    let salt1 = vec![1u8; KEY_SIZE];
    let salt2 = vec![2u8; KEY_SIZE];
    let key1 = derive_key(password, &salt1, CipherType::ChaCha20Poly1305).expect("Key derivation failed");
    let key2 = derive_key(password, &salt2, CipherType::ChaCha20Poly1305).expect("Key derivation failed");
    assert_ne!(key1, key2);
}

#[test]
fn test_derive_key_empty_password() {
    let salt = vec![1u8; KEY_SIZE];
    let key = derive_key("", &salt, CipherType::ChaCha20Poly1305);
    assert!(key.is_ok());
    assert_eq!(key.unwrap().len(), 32);
}

#[test]
fn test_encrypt_decrypt_empty_plaintext() {
    let plaintext = b"";
    let password = "password";
    let encrypted = encrypt_symmetric(plaintext, password, CipherType::Aes256Gcm).expect("Encryption failed");
    let decrypted = decrypt_symmetric(&encrypted, password).expect("Decryption failed");
    assert_eq!(plaintext.to_vec(), decrypted);
}

#[test]
fn test_encrypt_decrypt_large_plaintext() {
    let plaintext = vec![42u8; 10000];
    let password = "password";
    let encrypted = encrypt_symmetric(&plaintext, password, CipherType::Aes256Gcm).expect("Encryption failed");
    let decrypted = decrypt_symmetric(&encrypted, password).expect("Decryption failed");
    assert_eq!(plaintext, decrypted);
}

#[test]
fn test_different_nonces_for_same_input() {
    let plaintext = b"Same message";
    let password = "password";
    let encrypted1 = encrypt_symmetric(plaintext, password, CipherType::ChaCha20Poly1305).expect("Encryption failed");
    let encrypted2 = encrypt_symmetric(plaintext, password, CipherType::ChaCha20Poly1305).expect("Encryption failed");
    assert_ne!(encrypted1.nonce, encrypted2.nonce);
    assert_ne!(encrypted1.ciphertext, encrypted2.ciphertext);
}

#[test]
fn test_derive_key_32_bytes() {
    let salt = vec![1u8; 32];
    let key = derive_key("password", &salt, CipherType::Aes256Gcm).expect("Key derivation failed");
    assert_eq!(key.len(), 32);
}

#[test]
fn test_derive_key_long_password() {
    let long_pw = "a".repeat(100);
    let salt = vec![1u8; 32];
    let key = derive_key(&long_pw, &salt, CipherType::ChaCha20Poly1305).expect("Key derivation failed");
    assert_eq!(key.len(), 32);
}

#[test]
fn test_encrypt_decrypt_with_long_password() {
    let long_pw = "x".repeat(128);
    let plaintext = b"test data with long password";
    let encrypted = encrypt_symmetric(plaintext, &long_pw, CipherType::Aes256Gcm).expect("Encryption failed");
    let decrypted = decrypt_symmetric(&encrypted, &long_pw).expect("Decryption failed");
    assert_eq!(plaintext.to_vec(), decrypted);
}

#[test]
fn test_decrypt_tampered_ciphertext_fails() {
    let plaintext = b"original data";
    let mut encrypted = encrypt_symmetric(plaintext, "pw", CipherType::ChaCha20Poly1305).expect("Encryption failed");
    encrypted.ciphertext[0] ^= 0xFF;
    let result = decrypt_symmetric(&encrypted, "pw");
    assert!(result.is_err());
}

#[test]
fn test_decrypt_tampered_nonce_fails() {
    let plaintext = b"data";
    let mut encrypted = encrypt_symmetric(plaintext, "pw", CipherType::Aes256Gcm).expect("Encryption failed");
    encrypted.nonce[0] ^= 0xFF;
    let result = decrypt_symmetric(&encrypted, "pw");
    assert!(result.is_err());
}

#[test]
fn test_encrypt_empty_password() {
    let plaintext = b"data with no password";
    let encrypted = encrypt_symmetric(plaintext, "", CipherType::ChaCha20Poly1305).expect("Encryption failed");
    let decrypted = decrypt_symmetric(&encrypted, "").expect("Decryption failed");
    assert_eq!(plaintext.to_vec(), decrypted);
}

#[test]
fn test_encrypt_decrypt_large_plaintext_chacha() {
    let plaintext = vec![7u8; 5000];
    let encrypted = encrypt_symmetric(&plaintext, "pw", CipherType::ChaCha20Poly1305).expect("Encryption failed");
    let decrypted = decrypt_symmetric(&encrypted, "pw").expect("Decryption failed");
    assert_eq!(plaintext, decrypted);
}
