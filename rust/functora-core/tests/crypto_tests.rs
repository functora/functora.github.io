use functora_core::crypto::{
    CipherType, EncryptedNote, KEY_SIZE, Kdf, decrypt_symmetric, derive_key, encrypt_symmetric,
    stream_decrypt_symmetric, stream_encrypt_symmetric,
};

fn fast_kdf() {
    if std::env::var("FUNCTORA_KDF_M_COST_KIB").is_err() {
        unsafe {
            std::env::set_var("FUNCTORA_KDF_M_COST_KIB", "1024");
            std::env::set_var("FUNCTORA_KDF_T_COST", "1");
        }
    }
}

const AAD: &[u8] = b"test-app.v1";

#[test]
fn chacha20_roundtrip() {
    fast_kdf();
    let plaintext = b"Hello, World!";
    let encrypted = encrypt_symmetric(plaintext, "pw", CipherType::ChaCha20Poly1305, AAD)
        .unwrap_or_else(|e| panic!("encrypt: {e:?}"));
    let decrypted =
        decrypt_symmetric(&encrypted, "pw", AAD).unwrap_or_else(|e| panic!("decrypt: {e:?}"));
    assert_eq!(plaintext.to_vec(), decrypted);
}

#[test]
fn aes256gcm_roundtrip() {
    fast_kdf();
    let plaintext = b"Secret message";
    let encrypted = encrypt_symmetric(plaintext, "pw", CipherType::Aes256Gcm, AAD)
        .unwrap_or_else(|e| panic!("encrypt: {e:?}"));
    let decrypted =
        decrypt_symmetric(&encrypted, "pw", AAD).unwrap_or_else(|e| panic!("decrypt: {e:?}"));
    assert_eq!(plaintext.to_vec(), decrypted);
}

#[test]
fn wrong_password_fails() {
    fast_kdf();
    let encrypted = encrypt_symmetric(b"data", "correct", CipherType::Aes256Gcm, AAD)
        .unwrap_or_else(|e| panic!("encrypt: {e:?}"));
    assert!(decrypt_symmetric(&encrypted, "wrong", AAD).is_err());
}

#[test]
fn wrong_aad_fails() {
    fast_kdf();
    let encrypted = encrypt_symmetric(b"data", "pw", CipherType::Aes256Gcm, AAD)
        .unwrap_or_else(|e| panic!("encrypt: {e:?}"));
    assert!(decrypt_symmetric(&encrypted, "pw", b"other-app.v1").is_err());
}

#[test]
fn derive_key_is_consistent_and_salt_sensitive() {
    fast_kdf();
    let salt1 = vec![1u8; KEY_SIZE];
    let salt2 = vec![2u8; KEY_SIZE];
    let k1a = derive_key("pw", &salt1, Kdf::Argon2id).unwrap_or_else(|e| panic!("derive: {e:?}"));
    let k1b = derive_key("pw", &salt1, Kdf::Argon2id).unwrap_or_else(|e| panic!("derive: {e:?}"));
    let k2 = derive_key("pw", &salt2, Kdf::Argon2id).unwrap_or_else(|e| panic!("derive: {e:?}"));
    assert_eq!(k1a, k1b);
    assert_ne!(k1a, k2);
    assert_eq!(k1a.len(), KEY_SIZE);
}

#[test]
fn empty_plaintext_roundtrip() {
    fast_kdf();
    let encrypted = encrypt_symmetric(b"", "pw", CipherType::ChaCha20Poly1305, AAD)
        .unwrap_or_else(|e| panic!("encrypt: {e:?}"));
    assert_eq!(
        b"".to_vec(),
        decrypt_symmetric(&encrypted, "pw", AAD).unwrap_or_else(|e| panic!("decrypt: {e:?}"))
    );
}

#[test]
fn stream_roundtrip_large_payload() {
    fast_kdf();
    let plaintext = vec![42u8; 300_000];
    let encrypted = stream_encrypt_symmetric(&plaintext, "pw", CipherType::Aes256Gcm, AAD)
        .unwrap_or_else(|e| panic!("encrypt: {e:?}"));
    assert_eq!(
        plaintext,
        stream_decrypt_symmetric(&encrypted, "pw", AAD)
            .unwrap_or_else(|e| panic!("decrypt: {e:?}"))
    );
}

#[test]
fn stream_tampered_ciphertext_fails() {
    fast_kdf();
    let encrypted =
        stream_encrypt_symmetric(b"payload data", "pw", CipherType::ChaCha20Poly1305, AAD)
            .unwrap_or_else(|e| panic!("encrypt: {e:?}"));
    let mut tampered = encrypted;
    tampered.ciphertext[0] ^= 0xFF;
    assert!(stream_decrypt_symmetric(&tampered, "pw", AAD).is_err());
}

#[test]
fn decrypt_rejects_bad_nonce_or_salt_sizes() {
    fast_kdf();
    let mut note = encrypt_symmetric(b"data", "pw", CipherType::Aes256Gcm, AAD)
        .unwrap_or_else(|e| panic!("encrypt: {e:?}"));
    note.nonce.truncate(5);
    assert!(decrypt_symmetric(&note, "pw", AAD).is_err());
    let bad_salt = EncryptedNote {
        nonce: vec![0; 12],
        ciphertext: vec![0; 16],
        salt: vec![0; 3],
        ..encrypt_symmetric(b"data", "pw", CipherType::Aes256Gcm, AAD)
            .unwrap_or_else(|e| panic!("encrypt: {e:?}"))
    };
    assert!(decrypt_symmetric(&bad_salt, "pw", AAD).is_err());
}

#[test]
fn stream_recover_rejects_bad_sizes() {
    fast_kdf();
    let encrypted = stream_encrypt_symmetric(b"data", "pw", CipherType::Aes256Gcm, AAD)
        .unwrap_or_else(|e| panic!("encrypt: {e:?}"));
    let mut note = encrypted;
    note.salt.clear();
    assert!(stream_decrypt_symmetric(&note, "pw", AAD).is_err());
}
