pub use derive_more::Display;
use qrcode::types::QrError;
use sha2::digest;
use std::string::FromUtf8Error;

#[derive(Debug, Display)]
pub enum AppError {
    Cipher(digest::InvalidLength),
    Getrandom(getrandom::Error),
    Base64(base64::DecodeError),
    Json(serde_json::Error),
    Utf8(FromUtf8Error),
    Qr(QrError),
    Encrypt,
    Decrypt,
    Url,
}

impl AppError {
    pub fn localized(
        &self,
        t: &crate::i18n::Translations,
    ) -> String {
        let msg = match self {
            AppError::Cipher(_) => t.crypto_error,
            AppError::Getrandom(_) => t.crypto_error,
            AppError::Base64(_) => t.encoding_error,
            AppError::Json(_) => t.encoding_error,
            AppError::Utf8(_) => t.utf8_error,
            AppError::Qr(_) => t.utf8_error,
            AppError::Encrypt => t.crypto_error,
            AppError::Decrypt => t.crypto_error,
            AppError::Url => t.invalid_url_error,
        };
        format!("{}: {}", msg, self)
    }
}

impl std::error::Error for AppError {}

impl From<digest::InvalidLength> for AppError {
    fn from(e: digest::InvalidLength) -> Self {
        AppError::Cipher(e)
    }
}

impl From<getrandom::Error> for AppError {
    fn from(e: getrandom::Error) -> Self {
        AppError::Getrandom(e)
    }
}

impl From<base64::DecodeError> for AppError {
    fn from(e: base64::DecodeError) -> Self {
        AppError::Base64(e)
    }
}

impl From<serde_json::Error> for AppError {
    fn from(e: serde_json::Error) -> Self {
        AppError::Json(e)
    }
}

impl From<FromUtf8Error> for AppError {
    fn from(e: FromUtf8Error) -> Self {
        AppError::Utf8(e)
    }
}

impl From<QrError> for AppError {
    fn from(e: QrError) -> Self {
        AppError::Qr(e)
    }
}
