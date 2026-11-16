use crate::crypto::EncryptedNote;
use crate::error::AppError;
use serde::{Deserialize, Serialize};
use tap::prelude::*;

#[derive(Clone, Serialize, Deserialize)]
pub enum NoteData {
    PlainText(String),
    CipherText(EncryptedNote),
}

/// The note body is a secret in plain mode, so it never reaches a log or a
/// panic message through the derived `Debug` implementation. The ciphertext
/// is public by design, but only its size is worth showing.
impl std::fmt::Debug for NoteData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::PlainText(_) => f.write_str("PlainText(\"[redacted]\")"),
            Self::CipherText(note) => f
                .debug_struct("CipherText")
                .field("cipher", &note.cipher)
                .field("ciphertext_len", &note.ciphertext.len())
                .finish(),
        }
    }
}

pub fn encode_note(note: &NoteData) -> Result<String, AppError> {
    functora_core::encoding::encode_payload(note)?.pipe(Ok)
}

pub fn decode_note(encoded: &str) -> Result<NoteData, AppError> {
    functora_core::encoding::decode_payload::<NoteData>(encoded)?.pipe(Ok)
}

pub const NOTE_PARAM: &str = "note";

pub fn build_url(base_url: &str, note: &NoteData) -> Result<String, AppError> {
    encode_note(note)
        .map(|encoded| functora_core::encoding::append_query_param(base_url, NOTE_PARAM, &encoded))?
        .pipe(Ok)
}

pub fn extract_note_param(url: &str) -> Result<String, AppError> {
    let bare = url.split('#').next().unwrap_or(url);
    functora_core::encoding::extract_query_param(bare, NOTE_PARAM)
        .ok_or(AppError::NoNoteParam)?
        .pipe(Ok)
}

pub fn generate_qr_code(url: &str) -> Result<String, AppError> {
    functora_core::encoding::generate_qr_code(url)?.pipe(Ok)
}
