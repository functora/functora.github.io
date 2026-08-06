use crate::error::*;
use functora_tagged::{FCrude, Tagged};
use tap::prelude::*;

pub use functora_dioxus::crypto::{CipherType, EncryptedNote, Kdf, StreamParts, KEY_SIZE, STREAM_CHUNK, STREAM_TAG};

const AAD_PREFIX: &[u8] = b"cryptonote.v1";

pub fn aad(cipher: CipherType, kdf: Kdf) -> Vec<u8> {
    functora_dioxus::package::aad(AAD_PREFIX, cipher, kdf)
}

#[derive(Debug)]
pub enum DExternalArchive {}

pub type ExternalArchive = Tagged<Vec<u8>, DExternalArchive, FCrude>;

pub fn derive_key(password: &str, salt: &[u8], kdf: Kdf) -> Result<Vec<u8>, AppError> {
    functora_dioxus::crypto::derive_key(password, salt, kdf)?.pipe(Ok)
}

pub fn encrypt_symmetric(plaintext: &[u8], password: &str, cipher: CipherType) -> Result<EncryptedNote, AppError> {
    functora_dioxus::crypto::encrypt_symmetric(plaintext, password, cipher, &aad(cipher, Kdf::Argon2id))?.pipe(Ok)
}

pub fn decrypt_symmetric(data: &EncryptedNote, password: &str) -> Result<Vec<u8>, AppError> {
    functora_dioxus::crypto::decrypt_symmetric(data, password, &aad(data.cipher, data.kdf))?.pipe(Ok)
}

pub fn stream_encrypt_symmetric(
    plaintext: &[u8],
    password: &str,
    cipher: CipherType,
) -> Result<EncryptedNote, AppError> {
    functora_dioxus::crypto::stream_encrypt_symmetric(plaintext, password, cipher, &aad(cipher, Kdf::Argon2id))?
        .pipe(Ok)
}

pub fn stream_decrypt_symmetric(data: &EncryptedNote, password: &str) -> Result<Vec<u8>, AppError> {
    functora_dioxus::crypto::stream_decrypt_symmetric(data, password, &aad(data.cipher, data.kdf))?.pipe(Ok)
}
