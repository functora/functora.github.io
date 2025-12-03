use crate::error::AppError;
use aes_gcm::{
    Aes256Gcm,
    aead::{Aead, KeyInit},
};
use chacha20poly1305::ChaCha20Poly1305;
use hkdf::Hkdf;
use serde::{Deserialize, Serialize};
use sha2::Sha256;

const NONCE_SIZE: usize = 12;
const SALT_SIZE: usize = 32;

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Serialize,
    Deserialize,
)]
pub enum CipherType {
    ChaCha20Poly1305,
    Aes256Gcm,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EncryptedData {
    pub cipher: CipherType,
    pub nonce: Vec<u8>,
    pub ciphertext: Vec<u8>,
    pub salt: Vec<u8>,
}

pub fn derive_key(
    password: &str,
    salt: &[u8],
    cipher: CipherType,
) -> Vec<u8> {
    let key_len = match cipher {
        CipherType::ChaCha20Poly1305 => 32,
        CipherType::Aes256Gcm => 32,
    };
    let hk = Hkdf::<Sha256>::new(
        Some(salt),
        password.as_bytes(),
    );
    (0..key_len)
        .scan(vec![0u8; key_len], |key, _| {
            hk.expand(b"cryptonote-key", key).ok()?;
            Some(key.clone())
        })
        .last()
        .unwrap_or_else(|| vec![0u8; key_len])
}

pub fn encrypt_symmetric(
    plaintext: &[u8],
    password: &str,
    cipher: CipherType,
) -> Result<EncryptedData, AppError> {
    let mut salt = vec![0u8; SALT_SIZE];
    getrandom::getrandom(&mut salt)?;
    let key = derive_key(password, &salt, cipher);
    let mut nonce = vec![0u8; NONCE_SIZE];
    getrandom::getrandom(&mut nonce)?;

    let ciphertext = match cipher {
        CipherType::ChaCha20Poly1305 => {
            let cipher =
                ChaCha20Poly1305::new_from_slice(&key)?;
            let nonce_array =
                chacha20poly1305::Nonce::from_slice(&nonce);
            cipher
                .encrypt(nonce_array, plaintext)
                .map_err(|_| AppError::Encrypt)?
        }
        CipherType::Aes256Gcm => {
            let cipher = Aes256Gcm::new_from_slice(&key)?;
            let nonce_array =
                aes_gcm::Nonce::from_slice(&nonce);
            cipher
                .encrypt(nonce_array, plaintext)
                .map_err(|_| AppError::Encrypt)?
        }
    };

    Ok(EncryptedData {
        cipher,
        nonce,
        ciphertext,
        salt,
    })
}

pub fn decrypt_symmetric(
    data: &EncryptedData,
    password: &str,
) -> Result<Vec<u8>, AppError> {
    let cipher = data.cipher;
    let salt = &data.salt;
    let key = derive_key(password, salt, cipher);

    match cipher {
        CipherType::ChaCha20Poly1305 => {
            let cipher =
                ChaCha20Poly1305::new_from_slice(&key)?;
            let nonce_array =
                chacha20poly1305::Nonce::from_slice(
                    &data.nonce,
                );
            cipher
                .decrypt(
                    nonce_array,
                    data.ciphertext.as_ref(),
                )
                .map_err(|_| AppError::Decrypt)
        }
        CipherType::Aes256Gcm => {
            let cipher = Aes256Gcm::new_from_slice(&key)?;
            let nonce_array =
                aes_gcm::Nonce::from_slice(&data.nonce);
            cipher
                .decrypt(
                    nonce_array,
                    data.ciphertext.as_ref(),
                )
                .map_err(|_| AppError::Decrypt)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
        let decrypted =
            decrypt_symmetric(&encrypted, password)
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
        let decrypted =
            decrypt_symmetric(&encrypted, password)
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
}
