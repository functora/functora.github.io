use crate::crypto::EncryptedNote;
use crate::error::AppError;
use functora_core::encoding::{
    append_query_param, decode_payload, encode_payload, extract_query_param, generate_qr_code,
};
use serde::{Deserialize, Serialize};
use tap::prelude::*;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum NoteData {
    PlainText(String),
    CipherText(EncryptedNote),
}

pub fn encode_note(note: &NoteData) -> Result<String, AppError> {
    encode_payload(note)?.pipe(Ok)
}

pub fn decode_note(encoded: &str) -> Result<NoteData, AppError> {
    decode_payload::<NoteData>(encoded)?.pipe(Ok)
}

pub fn build_url(base_url: &str, note: &NoteData) -> Result<String, AppError> {
    encode_note(note)
        .map(|encoded| append_query_param(base_url, "note", &encoded))?
        .pipe(Ok)
}

pub fn extract_note_param(url: &str) -> Result<String, AppError> {
    extract_query_param(url, "note")
        .ok_or(AppError::NoNoteParam)?
        .pipe(Ok)
}

pub fn generate_qr(url: &str) -> Result<String, AppError> {
    generate_qr_code(url)?.pipe(Ok)
}
