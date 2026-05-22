use crate::error::*;
use aes_gcm::{
    aead::{Aead, KeyInit},
    Aes256Gcm,
};
use chacha20poly1305::ChaCha20Poly1305;
use hkdf::Hkdf;
use serde::{Deserialize, Serialize};
use sha2::Sha256;

const NONCE_SIZE: usize = 12;
const SALT_SIZE: usize = 32;
pub const KEY_SIZE: usize = 32;

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
    _: CipherType,
) -> Result<Vec<u8>, AppError> {
    let mut key = vec![0u8; KEY_SIZE];
    Hkdf::<Sha256>::new(Some(salt), password.as_bytes())
        .expand(b"cryptonote-key", &mut key)?;
    Ok(key)
}

fn random_vec(n: usize) -> Vec<u8> {
    let mut v = vec![0u8; n];
    getrandom::getrandom(&mut v).ok();
    v
}

pub fn encrypt_symmetric(
    plaintext: &[u8],
    password: &str,
    cipher: CipherType,
) -> Result<EncryptedData, AppError> {
    let salt = random_vec(SALT_SIZE);
    let key = derive_key(password, &salt, cipher)?;
    let nonce = random_vec(NONCE_SIZE);

    let ciphertext = match cipher {
        CipherType::ChaCha20Poly1305 => {
            let c = ChaCha20Poly1305::new_from_slice(&key)?;
            c.encrypt(
                chacha20poly1305::Nonce::from_slice(&nonce),
                plaintext,
            )
            .map_err(|_| AppError::Encrypt)?
        }
        CipherType::Aes256Gcm => {
            let c = Aes256Gcm::new_from_slice(&key)?;
            c.encrypt(
                aes_gcm::Nonce::from_slice(&nonce),
                plaintext,
            )
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
    let key =
        derive_key(password, &data.salt, data.cipher)?;

    match data.cipher {
        CipherType::ChaCha20Poly1305 => {
            let c = ChaCha20Poly1305::new_from_slice(&key)?;
            c.decrypt(
                chacha20poly1305::Nonce::from_slice(
                    &data.nonce,
                ),
                data.ciphertext.as_ref(),
            )
            .map_err(|_| AppError::Decrypt)
        }
        CipherType::Aes256Gcm => {
            let c = Aes256Gcm::new_from_slice(&key)?;
            c.decrypt(
                aes_gcm::Nonce::from_slice(&data.nonce),
                data.ciphertext.as_ref(),
            )
            .map_err(|_| AppError::Decrypt)
        }
    }
}
