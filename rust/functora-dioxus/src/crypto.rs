use crate::Error;
use aes_gcm::{
    Aes256Gcm,
    aead::{Aead, KeyInit, Payload},
};
use argon2::{Algorithm::Argon2id as Argon2Algo, Argon2, Params, Version::V0x13 as Argon2V};
use chacha20poly1305::ChaCha20Poly1305;
use serde::{Deserialize, Serialize};
use zeroize::Zeroizing;

const NONCE_SIZE: usize = 12;
const SALT_SIZE: usize = 32;
pub const KEY_SIZE: usize = 32;
pub const STREAM_CHUNK: usize = 64 * 1024;
pub const STREAM_TAG: usize = 16;
const STREAM_NONCE_SIZE: usize = 7;
const ARGON2_M_COST_KIB: u32 = 65536;
const ARGON2_T_COST: u32 = 3;
const ARGON2_P_COST: u32 = 1;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, derive_more::Display)]
pub enum CipherType {
    ChaCha20Poly1305 = 0,
    Aes256Gcm = 1,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, derive_more::Display)]
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

#[allow(clippy::large_enum_variant)]
enum AnyCipher {
    ChaCha(ChaCha20Poly1305),
    Aes(Aes256Gcm),
}

impl AnyCipher {
    fn keyed(cipher: CipherType, key: &[u8]) -> Result<Self, Error> {
        match cipher {
            CipherType::ChaCha20Poly1305 => ChaCha20Poly1305::new_from_slice(key)
                .map(Self::ChaCha)
                .map_err(Error::Cipher),
            CipherType::Aes256Gcm => Aes256Gcm::new_from_slice(key).map(Self::Aes).map_err(Error::Cipher),
        }
    }

    fn encrypt(&self, nonce: &[u8], aad: &[u8], msg: &[u8]) -> Result<Vec<u8>, Error> {
        match self {
            Self::ChaCha(c) => c
                .encrypt(chacha20poly1305::Nonce::from_slice(nonce), Payload { msg, aad })
                .map_err(Error::Encrypt),
            Self::Aes(c) => c
                .encrypt(aes_gcm::Nonce::from_slice(nonce), Payload { msg, aad })
                .map_err(Error::Encrypt),
        }
    }

    fn decrypt(&self, nonce: &[u8], aad: &[u8], ct: &[u8]) -> Result<Vec<u8>, Error> {
        match self {
            Self::ChaCha(c) => c
                .decrypt(chacha20poly1305::Nonce::from_slice(nonce), Payload { msg: ct, aad })
                .map_err(Error::Decrypt),
            Self::Aes(c) => c
                .decrypt(aes_gcm::Nonce::from_slice(nonce), Payload { msg: ct, aad })
                .map_err(Error::Decrypt),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ChunkKind {
    NonLast,
    Last,
}

fn stream_nonce(base: &[u8], position: u32, kind: ChunkKind) -> [u8; 12] {
    let mut nonce = [0u8; 12];
    nonce[..STREAM_NONCE_SIZE].copy_from_slice(base);
    nonce[STREAM_NONCE_SIZE..11].copy_from_slice(&position.to_be_bytes());
    nonce[11] = match kind {
        ChunkKind::NonLast => 0,
        ChunkKind::Last => 1,
    };
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
    pub fn derive(password: &str, cipher: CipherType, aad: &[u8]) -> Result<Self, Error> {
        let salt = random_vec(SALT_SIZE)?;
        let nonce = random_vec(STREAM_NONCE_SIZE)?;
        let key = Zeroizing::new(derive_key(password, &salt, Kdf::Argon2id)?);
        Ok(Self {
            cipher,
            key,
            nonce,
            salt,
            aad: aad.to_vec(),
        })
    }

    pub fn recover(password: &str, cipher: CipherType, salt: &[u8], nonce: &[u8], aad: &[u8]) -> Result<Self, Error> {
        (salt.len() == SALT_SIZE && nonce.len() == STREAM_NONCE_SIZE)
            .then_some(())
            .ok_or_else(|| Error::InvalidFormat("stream salt or nonce has invalid size".into()))?;
        let key = Zeroizing::new(derive_key(password, salt, Kdf::Argon2id)?);
        Ok(Self {
            cipher,
            key,
            nonce: nonce.to_vec(),
            salt: salt.to_vec(),
            aad: aad.to_vec(),
        })
    }

    pub fn nonce(&self) -> &[u8] {
        &self.nonce
    }

    pub fn salt(&self) -> &[u8] {
        &self.salt
    }

    pub fn encrypt_chunk(&self, position: u32, kind: ChunkKind, plaintext: &[u8]) -> Result<Vec<u8>, Error> {
        let nonce = stream_nonce(&self.nonce, position, kind);
        AnyCipher::keyed(self.cipher, &self.key)?.encrypt(&nonce, &self.aad, plaintext)
    }

    pub fn decrypt_chunk(&self, position: u32, kind: ChunkKind, chunk: &[u8]) -> Result<Vec<u8>, Error> {
        let nonce = stream_nonce(&self.nonce, position, kind);
        AnyCipher::keyed(self.cipher, &self.key)?.decrypt(&nonce, &self.aad, chunk)
    }
}

pub fn stream_encrypt_symmetric(
    plaintext: &[u8],
    password: &str,
    cipher: CipherType,
    aad: &[u8],
) -> Result<EncryptedNote, Error> {
    let parts = StreamParts::derive(password, cipher, aad)?;
    let mut ciphertext = Vec::new();
    let mut offset = 0usize;
    let mut position = 0u32;
    while offset < plaintext.len() {
        let end = (offset + STREAM_CHUNK).min(plaintext.len());
        let kind = if end == plaintext.len() {
            ChunkKind::Last
        } else {
            ChunkKind::NonLast
        };
        ciphertext.extend(parts.encrypt_chunk(position, kind, &plaintext[offset..end])?);
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

pub fn stream_decrypt_symmetric(data: &EncryptedNote, password: &str, aad: &[u8]) -> Result<Vec<u8>, Error> {
    let parts = StreamParts::recover(password, data.cipher, &data.salt, &data.nonce, aad)?;
    let mut plaintext = Vec::new();
    let mut offset = 0usize;
    let mut position = 0u32;
    while offset < data.ciphertext.len() {
        let take = (data.ciphertext.len() - offset).min(STREAM_CHUNK + STREAM_TAG);
        let kind = if offset + take == data.ciphertext.len() {
            ChunkKind::Last
        } else {
            ChunkKind::NonLast
        };
        plaintext.extend(parts.decrypt_chunk(position, kind, &data.ciphertext[offset..offset + take])?);
        offset += take;
        position += 1;
    }
    Ok(plaintext)
}

fn env_cost(name: &str, default: u32) -> u32 {
    std::env::var(name).ok().and_then(|v| v.parse().ok()).unwrap_or(default)
}

fn kdf_params() -> Result<Params, Error> {
    Params::new(
        env_cost("FUNCTORA_KDF_M_COST_KIB", ARGON2_M_COST_KIB),
        env_cost("FUNCTORA_KDF_T_COST", ARGON2_T_COST),
        ARGON2_P_COST,
        Some(KEY_SIZE),
    )
    .map_err(Error::KeyDerive)
}

pub fn derive_key(password: &str, salt: &[u8], kdf: Kdf) -> Result<Vec<u8>, Error> {
    let mut key = vec![0u8; KEY_SIZE];
    match kdf {
        Kdf::Argon2id => {
            let params = kdf_params()?;
            Argon2::new(Argon2Algo, Argon2V, params)
                .hash_password_into(password.as_bytes(), salt, &mut key)
                .map_err(Error::KeyDerive)?;
        }
    }
    Ok(key)
}

fn random_vec(n: usize) -> Result<Vec<u8>, Error> {
    let mut v = vec![0u8; n];
    getrandom::getrandom(&mut v).map_err(Error::Getrandom)?;
    Ok(v)
}

pub fn encrypt_symmetric(
    plaintext: &[u8],
    password: &str,
    cipher: CipherType,
    aad: &[u8],
) -> Result<EncryptedNote, Error> {
    let salt = random_vec(SALT_SIZE)?;
    let key = Zeroizing::new(derive_key(password, &salt, Kdf::Argon2id)?);
    let nonce = random_vec(NONCE_SIZE)?;

    let ciphertext = match cipher {
        CipherType::ChaCha20Poly1305 => {
            let c = ChaCha20Poly1305::new_from_slice(&key).map_err(Error::Cipher)?;
            c.encrypt(
                chacha20poly1305::Nonce::from_slice(&nonce),
                Payload { msg: plaintext, aad },
            )
            .map_err(Error::Encrypt)?
        }
        CipherType::Aes256Gcm => {
            let c = Aes256Gcm::new_from_slice(&key).map_err(Error::Cipher)?;
            c.encrypt(aes_gcm::Nonce::from_slice(&nonce), Payload { msg: plaintext, aad })
                .map_err(Error::Encrypt)?
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

fn validate_note(data: &EncryptedNote) -> Result<(), Error> {
    (data.nonce.len() == NONCE_SIZE && data.salt.len() == SALT_SIZE)
        .then_some(())
        .ok_or_else(|| Error::InvalidFormat("nonce or salt has invalid size".into()))
}

pub fn decrypt_symmetric(data: &EncryptedNote, password: &str, aad: &[u8]) -> Result<Vec<u8>, Error> {
    validate_note(data)?;
    let key = Zeroizing::new(derive_key(password, &data.salt, data.kdf)?);

    match data.cipher {
        CipherType::ChaCha20Poly1305 => {
            let c = ChaCha20Poly1305::new_from_slice(&key).map_err(Error::Cipher)?;
            c.decrypt(
                chacha20poly1305::Nonce::from_slice(&data.nonce),
                Payload {
                    msg: &data.ciphertext,
                    aad,
                },
            )
            .map_err(Error::Decrypt)
        }
        CipherType::Aes256Gcm => {
            let c = Aes256Gcm::new_from_slice(&key).map_err(Error::Cipher)?;
            c.decrypt(
                aes_gcm::Nonce::from_slice(&data.nonce),
                Payload {
                    msg: &data.ciphertext,
                    aad,
                },
            )
            .map_err(Error::Decrypt)
        }
    }
}
