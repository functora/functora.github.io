use crate::Error;
use base64::Engine;
use base64::engine::general_purpose::URL_SAFE_NO_PAD;
use serde::Serialize;
use serde::de::DeserializeOwned;
use std::borrow::Cow;
use tap::prelude::*;

pub fn encode_payload<T: Serialize>(value: &T) -> Result<String, Error> {
    serde_json::to_vec(value)
        .map(|bytes| URL_SAFE_NO_PAD.encode(bytes))?
        .pipe(Ok)
}

pub fn decode_payload<T: DeserializeOwned>(encoded: &str) -> Result<T, Error> {
    let bytes = URL_SAFE_NO_PAD.decode(encoded)?;
    serde_json::from_slice::<T>(&bytes)?.pipe(Ok)
}

#[must_use]
pub fn append_query_param(base_url: &str, name: &str, value: &str) -> String {
    let separator = if base_url.contains('?') { "&" } else { "?" };
    format!("{base_url}{separator}{name}={}", urlencoding::encode(value))
}

#[must_use]
pub fn extract_query_param(url: &str, name: &str) -> Option<String> {
    url.split('?')
        .nth(1)?
        .split('&')
        .find_map(|param| {
            param
                .split_once('=')
                .filter(|(actual, _)| *actual == name)
                .map(|(_, value)| value)
        })
        .map(|value| urlencoding::decode(value).map_or_else(|_| value.to_string(), Cow::into_owned))
}

#[cfg(feature = "qr")]
pub fn generate_qr_code(url: &str) -> Result<String, Error> {
    use rxing::qrcode::QRCodeWriter;
    use rxing::{BarcodeFormat, EncodeHints, Writer};
    const QR_SVG_SIZE: i32 = 200;
    const QR_SVG_QUIET_ZONE: i32 = 2;
    let hints = EncodeHints {
        Margin: Some(QR_SVG_QUIET_ZONE.to_string()),
        ..Default::default()
    };
    QRCodeWriter
        .encode_with_hints(url, &BarcodeFormat::QR_CODE, QR_SVG_SIZE, QR_SVG_SIZE, &hints)
        .map(|matrix| bitmatrix_to_svg(&matrix))?
        .pipe(Ok)
}

#[cfg(feature = "qr")]
fn bitmatrix_to_svg(matrix: &rxing::common::BitMatrix) -> String {
    let w = matrix.getWidth();
    let h = matrix.getHeight();
    let paths = (0..h)
        .flat_map(|y| {
            (0..w)
                .filter(move |&x| matrix.get(x, y))
                .map(move |x| format!("M{x} {y} h1 v1 h-1 z "))
        })
        .collect::<String>();
    format!(
        r##"<svg xmlns="http://www.w3.org/2000/svg" version="1.1" viewBox="0 0 {w} {h}" shape-rendering="crispEdges"><rect width="{w}" height="{h}" fill="#ffffff"/><path d="{paths}" fill="#000000"/></svg>"##,
    )
}

#[must_use]
pub fn download_script(filename: &str) -> String {
    let escaped = filename
        .replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t")
        .replace('\'', "\\u0027")
        .replace('<', "\\u003c")
        .replace('>', "\\u003e");
    format!(
        r#"(async function(){{const parts=[];for(;;){{const m=await dioxus.recv();if(m&&m.t==='done')break;const bin=atob(m.data);const bytes=new Uint8Array(bin.length);for(let i=0;i<bin.length;i++)bytes[i]=bin.charCodeAt(i);parts.push(bytes)}}const url=URL.createObjectURL(new Blob(parts,{{type:'application/octet-stream'}}));const a=document.createElement('a');a.href=url;a.download="{escaped}";a.style.display='none';document.body.appendChild(a);a.click();setTimeout(()=>{{document.body.removeChild(a);URL.revokeObjectURL(url)}},1000)}})()"#,
    )
}
