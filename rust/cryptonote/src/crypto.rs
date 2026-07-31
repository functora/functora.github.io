use crate::error::*;
use crate::prelude::Display;
use aes_gcm::{
    aead::{Aead, KeyInit, Payload},
    Aes256Gcm,
};
use argon2::{Algorithm::Argon2id as Argon2Algo, Argon2, Params, Version::V0x13 as Argon2V};
use chacha20poly1305::ChaCha20Poly1305;
use functora_tagged::{FCrude, Tagged};
use serde::{Deserialize, Serialize};
use zeroize::Zeroizing;

const NONCE_SIZE: usize = 12;
const SALT_SIZE: usize = 32;
pub const KEY_SIZE: usize = 32;
const AAD_PREFIX: &[u8] = b"cryptonote.v1";
const ARGON2_M_COST_KIB: u32 = 65536;
const ARGON2_T_COST: u32 = 2;
const ARGON2_P_COST: u32 = 1;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Display)]
pub enum CipherType {
    ChaCha20Poly1305 = 0,
    Aes256Gcm = 1,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Display)]
pub enum Kdf {
    Argon2id = 0,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EncryptedNote {
    pub cipher: CipherType,
    pub nonce: Vec<u8>,
    pub ciphertext: Vec<u8>,
    pub salt: Vec<u8>,
    pub kdf: Kdf,
}

#[derive(Debug)]
pub enum DExternalArchive {}

pub type ExternalArchive = Tagged<Vec<u8>, DExternalArchive, FCrude>;

fn aad(cipher: CipherType, kdf: Kdf) -> Vec<u8> {
    let mut aad = AAD_PREFIX.to_vec();
    aad.push(cipher as u8);
    aad.push(kdf as u8);
    aad
}

pub fn derive_key(password: &str, salt: &[u8], kdf: Kdf) -> Result<Vec<u8>, AppError> {
    let mut key = vec![0u8; KEY_SIZE];
    match kdf {
        Kdf::Argon2id => {
            let params = Params::new(ARGON2_M_COST_KIB, ARGON2_T_COST, ARGON2_P_COST, Some(KEY_SIZE))?;
            Argon2::new(Argon2Algo, Argon2V, params).hash_password_into(password.as_bytes(), salt, &mut key)?;
        }
    }
    Ok(key)
}

fn random_vec(n: usize) -> Result<Vec<u8>, AppError> {
    let mut v = vec![0u8; n];
    getrandom::getrandom(&mut v)?;
    Ok(v)
}

pub fn encrypt_symmetric(plaintext: &[u8], password: &str, cipher: CipherType) -> Result<EncryptedNote, AppError> {
    let salt = random_vec(SALT_SIZE)?;
    let key = Zeroizing::new(derive_key(password, &salt, Kdf::Argon2id)?);
    let nonce = random_vec(NONCE_SIZE)?;
    let aad = aad(cipher, Kdf::Argon2id);

    let ciphertext = match cipher {
        CipherType::ChaCha20Poly1305 => {
            let c = ChaCha20Poly1305::new_from_slice(&key)?;
            c.encrypt(
                chacha20poly1305::Nonce::from_slice(&nonce),
                Payload {
                    msg: plaintext,
                    aad: &aad,
                },
            )
            .map_err(|e| AppError::Encrypt(e.to_string()))?
        }
        CipherType::Aes256Gcm => {
            let c = Aes256Gcm::new_from_slice(&key)?;
            c.encrypt(
                aes_gcm::Nonce::from_slice(&nonce),
                Payload {
                    msg: plaintext,
                    aad: &aad,
                },
            )
            .map_err(|e| AppError::Encrypt(e.to_string()))?
        }
    };

    Ok(EncryptedNote {
        cipher,
        nonce,
        ciphertext,
        salt,
        kdf: Kdf::Argon2id,
    })
}

fn validate_note(data: &EncryptedNote) -> Result<(), AppError> {
    (data.nonce.len() == NONCE_SIZE && data.salt.len() == SALT_SIZE)
        .then_some(())
        .ok_or_else(|| AppError::InvalidFormat("nonce or salt has invalid size".into()))
}

pub fn decrypt_symmetric(data: &EncryptedNote, password: &str) -> Result<Vec<u8>, AppError> {
    validate_note(data)?;
    let key = Zeroizing::new(derive_key(password, &data.salt, data.kdf)?);
    let aad = aad(data.cipher, data.kdf);

    match data.cipher {
        CipherType::ChaCha20Poly1305 => {
            let c = ChaCha20Poly1305::new_from_slice(&key)?;
            c.decrypt(
                chacha20poly1305::Nonce::from_slice(&data.nonce),
                Payload {
                    msg: &data.ciphertext,
                    aad: &aad,
                },
            )
            .map_err(|e| AppError::Decrypt(e.to_string()))
        }
        CipherType::Aes256Gcm => {
            let c = Aes256Gcm::new_from_slice(&key)?;
            c.decrypt(
                aes_gcm::Nonce::from_slice(&data.nonce),
                Payload {
                    msg: &data.ciphertext,
                    aad: &aad,
                },
            )
            .map_err(|e| AppError::Decrypt(e.to_string()))
        }
    }
}
