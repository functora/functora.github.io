pub use derive_more::Display;
use qrcode::types::QrError;
use sha2::digest;

#[derive(Debug, Display)]
pub enum AppError {
    SymKey(digest::InvalidLength),
    AesGcm(aes_gcm::Error),
    ChaChaPoly1305(chacha20poly1305::Error),
    Getrandom(String),
    Json(serde_json::Error),
    Base64(String),
    Utf8(String),
    Qr(QrError),
    InvalidUrl,
}

impl AppError {
    pub fn localized(
        &self,
        t: &crate::i18n::Translations,
    ) -> String {
        match self {
            AppError::SymKey(msg) => {
                format!("{}: {}", t.crypto_error, msg)
            }
            AppError::AesGcm(msg) => {
                format!("{}: {}", t.crypto_error, msg)
            }
            AppError::ChaChaPoly1305(msg) => {
                format!("{}: {}", t.crypto_error, msg)
            }
            AppError::Getrandom(msg) => {
                format!("{}: {}", t.crypto_error, msg)
            }
            AppError::Base64(msg) => {
                format!("{}: {}", t.encoding_error, msg)
            }
            AppError::Json(msg) => {
                format!("{}: {}", t.encoding_error, msg)
            }
            AppError::Utf8(msg) => {
                format!("{}: {}", t.utf8_error, msg)
            }
            AppError::Qr(msg) => {
                format!("{}: {}", t.utf8_error, msg)
            }
            AppError::InvalidUrl => {
                t.invalid_url_error.to_string()
            }
        }
    }
}

impl std::error::Error for AppError {}

impl From<serde_json::Error> for AppError {
    fn from(e: serde_json::Error) -> Self {
        AppError::Json(e)
    }
}

impl From<base64::DecodeError> for AppError {
    fn from(err: base64::DecodeError) -> Self {
        AppError::Base64(err.to_string())
    }
}

impl From<std::string::FromUtf8Error> for AppError {
    fn from(err: std::string::FromUtf8Error) -> Self {
        AppError::Utf8(err.to_string())
    }
}

impl From<getrandom::Error> for AppError {
    fn from(err: getrandom::Error) -> Self {
        AppError::Getrandom(err.to_string())
    }
}

impl From<digest::InvalidLength> for AppError {
    fn from(e: digest::InvalidLength) -> Self {
        AppError::SymKey(e)
    }
}

impl From<QrError> for AppError {
    fn from(e: QrError) -> Self {
        AppError::Qr(e)
    }
}
