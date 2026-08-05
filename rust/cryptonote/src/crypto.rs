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
pub const STREAM_CHUNK: usize = 64 * 1024;
pub const STREAM_TAG: usize = 16;
const STREAM_NONCE_SIZE: usize = 7;
const ARGON2_M_COST_KIB: u32 = 65536;
const ARGON2_T_COST: u32 = 3;
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

#[allow(clippy::large_enum_variant)]
enum AnyCipher {
    ChaCha(ChaCha20Poly1305),
    Aes(Aes256Gcm),
}

impl AnyCipher {
    fn keyed(cipher: CipherType, key: &[u8]) -> Result<Self, AppError> {
        match cipher {
            CipherType::ChaCha20Poly1305 => Ok(Self::ChaCha(ChaCha20Poly1305::new_from_slice(key)?)),
            CipherType::Aes256Gcm => Ok(Self::Aes(Aes256Gcm::new_from_slice(key)?)),
        }
    }

    fn encrypt(&self, nonce: &[u8], aad: &[u8], msg: &[u8]) -> Result<Vec<u8>, AppError> {
        match self {
            Self::ChaCha(c) => c
                .encrypt(chacha20poly1305::Nonce::from_slice(nonce), Payload { msg, aad })
                .map_err(|e| AppError::Encrypt(e.to_string())),
            Self::Aes(c) => c
                .encrypt(aes_gcm::Nonce::from_slice(nonce), Payload { msg, aad })
                .map_err(|e| AppError::Encrypt(e.to_string())),
        }
    }

    fn decrypt(&self, nonce: &[u8], aad: &[u8], ct: &[u8]) -> Result<Vec<u8>, AppError> {
        match self {
            Self::ChaCha(c) => c
                .decrypt(chacha20poly1305::Nonce::from_slice(nonce), Payload { msg: ct, aad })
                .map_err(|e| AppError::Decrypt(e.to_string())),
            Self::Aes(c) => c
                .decrypt(aes_gcm::Nonce::from_slice(nonce), Payload { msg: ct, aad })
                .map_err(|e| AppError::Decrypt(e.to_string())),
        }
    }
}

fn stream_nonce(base: &[u8], position: u32, last: bool) -> [u8; 12] {
    let mut nonce = [0u8; 12];
    nonce[..STREAM_NONCE_SIZE].copy_from_slice(base);
    nonce[STREAM_NONCE_SIZE..11].copy_from_slice(&position.to_be_bytes());
    nonce[11] = last as u8;
    nonce
}

pub struct StreamParts {
    cipher: CipherType,
    key: Zeroizing<Vec<u8>>,
    nonce: Vec<u8>,
    salt: Vec<u8>,
    aad: Vec<u8>,
}

impl StreamParts {
    pub fn derive(password: &str, cipher: CipherType) -> Result<Self, AppError> {
        let salt = random_vec(SALT_SIZE)?;
        let nonce = random_vec(STREAM_NONCE_SIZE)?;
        let key = Zeroizing::new(derive_key(password, &salt, Kdf::Argon2id)?);
        Ok(Self {
            cipher,
            key,
            nonce,
            salt,
            aad: aad(cipher, Kdf::Argon2id),
        })
    }

    pub fn recover(password: &str, cipher: CipherType, salt: &[u8], nonce: &[u8]) -> Result<Self, AppError> {
        (salt.len() == SALT_SIZE && nonce.len() == STREAM_NONCE_SIZE)
            .then_some(())
            .ok_or_else(|| AppError::InvalidFormat("stream salt or nonce has invalid size".into()))?;
        let key = Zeroizing::new(derive_key(password, salt, Kdf::Argon2id)?);
        Ok(Self {
            cipher,
            key,
            nonce: nonce.to_vec(),
            salt: salt.to_vec(),
            aad: aad(cipher, Kdf::Argon2id),
        })
    }

    pub fn nonce(&self) -> &[u8] {
        &self.nonce
    }

    pub fn salt(&self) -> &[u8] {
        &self.salt
    }

    pub fn encrypt_chunk(&self, position: u32, last: bool, plaintext: &[u8]) -> Result<Vec<u8>, AppError> {
        let nonce = stream_nonce(&self.nonce, position, last);
        AnyCipher::keyed(self.cipher, &self.key)?.encrypt(&nonce, &self.aad, plaintext)
    }

    pub fn decrypt_chunk(&self, position: u32, last: bool, chunk: &[u8]) -> Result<Vec<u8>, AppError> {
        let nonce = stream_nonce(&self.nonce, position, last);
        AnyCipher::keyed(self.cipher, &self.key)?.decrypt(&nonce, &self.aad, chunk)
    }
}

pub fn stream_encrypt_symmetric(
    plaintext: &[u8],
    password: &str,
    cipher: CipherType,
) -> Result<EncryptedNote, AppError> {
    let parts = StreamParts::derive(password, cipher)?;
    let mut ciphertext = Vec::new();
    let mut offset = 0usize;
    let mut position = 0u32;
    while offset < plaintext.len() {
        let end = (offset + STREAM_CHUNK).min(plaintext.len());
        let last = end == plaintext.len();
        ciphertext.extend(parts.encrypt_chunk(position, last, &plaintext[offset..end])?);
        offset = end;
        position += 1;
    }
    Ok(EncryptedNote {
        cipher,
        nonce: parts.nonce().to_vec(),
        ciphertext,
        salt: parts.salt().to_vec(),
        kdf: Kdf::Argon2id,
    })
}

pub fn stream_decrypt_symmetric(data: &EncryptedNote, password: &str) -> Result<Vec<u8>, AppError> {
    let parts = StreamParts::recover(password, data.cipher, &data.salt, &data.nonce)?;
    let mut plaintext = Vec::new();
    let mut offset = 0usize;
    let mut position = 0u32;
    while offset < data.ciphertext.len() {
        let take = (data.ciphertext.len() - offset).min(STREAM_CHUNK + STREAM_TAG);
        let last = offset + take == data.ciphertext.len();
        plaintext.extend(parts.decrypt_chunk(position, last, &data.ciphertext[offset..offset + take])?);
        offset += take;
        position += 1;
    }
    Ok(plaintext)
}

fn env_cost(name: &str, default: u32) -> u32 {
    std::env::var(name).ok().and_then(|v| v.parse().ok()).unwrap_or(default)
}

fn kdf_params() -> Result<Params, AppError> {
    Ok(Params::new(
        env_cost("CRYPTONOTE_KDF_M_COST_KIB", ARGON2_M_COST_KIB),
        env_cost("CRYPTONOTE_KDF_T_COST", ARGON2_T_COST),
        ARGON2_P_COST,
        Some(KEY_SIZE),
    )?)
}

pub fn derive_key(password: &str, salt: &[u8], kdf: Kdf) -> Result<Vec<u8>, AppError> {
    let mut key = vec![0u8; KEY_SIZE];
    match kdf {
        Kdf::Argon2id => {
            let params = kdf_params()?;
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
