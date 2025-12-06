use cryptonote::crypto::{
    CipherType, decrypt_symmetric, derive_key,
    encrypt_symmetric,
};

const SALT_SIZE: usize = 32;

#[test]
fn test_symmetric_chacha20_roundtrip() {
    let plaintext = b"Hello, Cryptonote!";
    let password = "test_password_123";
    let encrypted = encrypt_symmetric(
        plaintext,
        password,
        CipherType::ChaCha20Poly1305,
    )
    .expect("Encryption failed");
    let decrypted = decrypt_symmetric(&encrypted, password)
        .expect("Decryption failed");
    assert_eq!(plaintext.to_vec(), decrypted);
}

#[test]
fn test_symmetric_aes_roundtrip() {
    let plaintext = b"Secret message with AES";
    let password = "strong_password";
    let encrypted = encrypt_symmetric(
        plaintext,
        password,
        CipherType::Aes256Gcm,
    )
    .expect("Encryption failed");
    let decrypted = decrypt_symmetric(&encrypted, password)
        .expect("Decryption failed");
    assert_eq!(plaintext.to_vec(), decrypted);
}

#[test]
fn test_symmetric_wrong_password() {
    let plaintext = b"Test data";
    let encrypted = encrypt_symmetric(
        plaintext,
        "correct",
        CipherType::ChaCha20Poly1305,
    )
    .expect("Encryption failed");
    let result = decrypt_symmetric(&encrypted, "wrong");
    assert!(result.is_err());
}

#[test]
fn test_key_derivation_consistency() {
    let password = "test";
    let salt = vec![1u8; SALT_SIZE];
    let key1 = derive_key(
        password,
        &salt,
        CipherType::ChaCha20Poly1305,
    );
    let key2 = derive_key(
        password,
        &salt,
        CipherType::ChaCha20Poly1305,
    );
    assert_eq!(key1, key2);
}
