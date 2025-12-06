use crate::crypto::EncryptedData;
use crate::error::AppError;
use base64::{
    Engine, engine::general_purpose::URL_SAFE_NO_PAD,
};
use qrcode::{QrCode, render::svg};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum NoteData {
    PlainText(String),
    CipherText(EncryptedData),
}

pub fn encode_note(
    note: &NoteData,
) -> Result<String, AppError> {
    serde_json::to_vec(note)
        .map(|bytes| URL_SAFE_NO_PAD.encode(bytes))
        .map_err(Into::into)
}

pub fn decode_note(
    encoded: &str,
) -> Result<NoteData, AppError> {
    let bytes = URL_SAFE_NO_PAD.decode(encoded)?;
    serde_json::from_slice(&bytes).map_err(Into::into)
}

pub fn build_url(
    base_url: &str,
    note: &NoteData,
) -> Result<String, AppError> {
    encode_note(note).map(|encoded| {
        format!("{}#note={}", base_url, encoded)
    })
}

pub fn parse_url(url: &str) -> Result<NoteData, AppError> {
    url.split("#note=")
        .nth(1)
        .ok_or_else(|| AppError::Url)
        .and_then(decode_note)
}

pub fn generate_qr_code(
    url: &str,
) -> Result<String, AppError> {
    Ok(QrCode::new(url).map(|code| {
        code.render::<svg::Color>()
            .min_dimensions(200, 200)
            .build()
    })?)
}

impl NoteData {
    pub fn as_ciphertext(&self) -> Option<&EncryptedData> {
        match self {
            NoteData::CipherText(data) => Some(data),
            _ => None,
        }
    }
}
