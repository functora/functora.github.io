use crate::error::AppError;
use functora_core::package::aad;
use tap::prelude::*;

pub use functora_core::crypto::{
    CipherType, EncryptedNote, KEY_SIZE, Kdf, STREAM_CHUNK, STREAM_TAG,
};

const AAD_PREFIX: &[u8] = b"cryptonote.v1";

#[must_use]
pub fn aad_bytes(cipher: CipherType, kdf: Kdf) -> Vec<u8> {
    aad(AAD_PREFIX, cipher, kdf)
}

pub fn derive_key(password: &str, salt: &[u8], kdf: Kdf) -> Result<Vec<u8>, AppError> {
    functora_core::crypto::derive_key(password, salt, kdf)?.pipe(Ok)
}

pub fn encrypt_symmetric(
    plaintext: &[u8],
    password: &str,
    cipher: CipherType,
) -> Result<EncryptedNote, AppError> {
    functora_core::crypto::encrypt_symmetric(
        plaintext,
        password,
        cipher,
        &aad_bytes(cipher, Kdf::Argon2id),
    )?
    .pipe(Ok)
}

pub fn decrypt_symmetric(data: &EncryptedNote, password: &str) -> Result<Vec<u8>, AppError> {
    functora_core::crypto::decrypt_symmetric(data, password, &aad_bytes(data.cipher, data.kdf))?
        .pipe(Ok)
}

pub fn stream_encrypt_symmetric(
    plaintext: &[u8],
    password: &str,
    cipher: CipherType,
) -> Result<EncryptedNote, AppError> {
    functora_core::crypto::stream_encrypt_symmetric(
        plaintext,
        password,
        cipher,
        &aad_bytes(cipher, Kdf::Argon2id),
    )?
    .pipe(Ok)
}

pub fn stream_decrypt_symmetric(data: &EncryptedNote, password: &str) -> Result<Vec<u8>, AppError> {
    functora_core::crypto::stream_decrypt_symmetric(
        data,
        password,
        &aad_bytes(data.cipher, data.kdf),
    )?
    .pipe(Ok)
}
