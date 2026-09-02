use crate::crypto::EncryptedNote;
use crate::error::AppError;
use serde::{Deserialize, Serialize};
use tap::prelude::*;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum NoteData {
    PlainText(String),
    CipherText(EncryptedNote),
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
    functora_core::encoding::extract_query_param(url, NOTE_PARAM)
        .ok_or(AppError::NoNoteParam)?
        .pipe(Ok)
}

pub fn generate_qr_code(url: &str) -> Result<String, AppError> {
    functora_core::encoding::generate_qr_code(url)?.pipe(Ok)
}
