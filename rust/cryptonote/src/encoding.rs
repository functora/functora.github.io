use crate::crypto::*;
use crate::error::*;
use serde::{Deserialize, Serialize};
use tap::prelude::*;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum NoteData {
    PlainText(String),
    CipherText(EncryptedNote),
}

pub fn encode_note(note: &NoteData) -> Result<String, AppError> {
    functora_dioxus::encoding::encode_payload(note)?.pipe(Ok)
}

pub fn decode_note(encoded: &str) -> Result<NoteData, AppError> {
    functora_dioxus::encoding::decode_payload::<NoteData>(encoded)?.pipe(Ok)
}

pub fn build_url(base_url: &str, note: &NoteData) -> Result<String, AppError> {
    encode_note(note)
        .map(|encoded| functora_dioxus::encoding::append_query_param(base_url, "note", &encoded))?
        .pipe(Ok)
}

pub fn extract_note_param(url: &str) -> Result<String, AppError> {
    functora_dioxus::encoding::extract_query_param(url, "note")
        .ok_or(AppError::NoNoteParam)?
        .pipe(Ok)
}

pub use functora_dioxus::encoding::{download_script, generate_qr_code};
