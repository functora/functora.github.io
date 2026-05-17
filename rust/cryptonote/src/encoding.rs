use crate::crypto::*;
use crate::error::*;
use base64::{
    engine::general_purpose::URL_SAFE_NO_PAD, Engine,
};
use rxing::qrcode::QRCodeWriter;
use rxing::{BarcodeFormat, EncodeHints, Writer};
use serde::{Deserialize, Serialize};
use std::borrow::Cow;
use std::fmt::Write;

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

const QR_SVG_SIZE: i32 = 200;
const QR_SVG_QUIET_ZONE: i32 = 2;

fn bitmatrix_to_svg(
    matrix: &rxing::common::BitMatrix,
) -> String {
    let w = matrix.getWidth();
    let h = matrix.getHeight();
    let mut svg =
        String::with_capacity(256 + (w * h * 2) as usize);
    write!(
        svg,
        r##"<svg xmlns="http://www.w3.org/2000/svg" version="1.1" viewBox="0 0 {} {}" shape-rendering="crispEdges"><path d=""##,
        w, h,
    )
    .ok();
    for y in 0..h {
        for x in 0..w {
            if matrix.get(x, y) {
                write!(svg, "M{} {} h1 v1 h-1 z ", x, y)
                    .ok();
            }
        }
    }
    write!(svg, r##"" fill="#000000"/></svg>"##).ok();
    svg
}

pub fn generate_qr_code(
    url: &str,
) -> Result<String, AppError> {
    let hints = EncodeHints {
        Margin: Some(QR_SVG_QUIET_ZONE.to_string()),
        ..Default::default()
    };
    QRCodeWriter
        .encode_with_hints(
            url,
            &BarcodeFormat::QR_CODE,
            QR_SVG_SIZE,
            QR_SVG_SIZE,
            &hints,
        )
        .map(|matrix| bitmatrix_to_svg(&matrix))
        .map_err(Into::into)
}
