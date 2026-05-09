use crate::crypto::*;
use crate::error::*;
use base64::{
    engine::general_purpose::URL_SAFE_NO_PAD, Engine,
};
use qrcode::{render::svg, QrCode};
use serde::{Deserialize, Serialize};
use std::borrow::Cow;

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
        let separator =
            if base_url.contains('?') { "&" } else { "?" };
        format!(
            "{}{}note={}",
            base_url,
            separator,
            urlencoding::encode(&encoded)
        )
    })
}

pub fn extract_note_param(
    url: &str,
) -> Result<String, AppError> {
    url.split('?')
        .nth(1)
        .ok_or(AppError::NoNoteParam)
        .and_then(|query| {
            query
                .split('&')
                .find_map(|param| {
                    let mut parts = param.splitn(2, '=');
                    match (parts.next(), parts.next()) {
                        (Some("note"), Some(value)) => {
                            Some(urlencoding::decode(value))
                        }
                        _ => None,
                    }
                })
                .ok_or(AppError::NoNoteParam)
        })?
        .map(Cow::into_owned)
        .map_err(Into::into)
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
