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
    PasswordRequired,
    NoNoteInUrl,
    NoNoteParam,
    #[cfg(target_arch = "wasm32")]
    MissingWindow,
    ClipboardWrite(String),
}

impl AppError {
    pub fn localized(
        &self,
        t: &crate::i18n::Translations,
    ) -> String {
        let msg = match self {
            AppError::Cipher(_) => t.cipher_error,
            AppError::Getrandom(_) => t.getrandom_error,
            AppError::Base64(_) => t.base64_error,
            AppError::Json(_) => t.json_error,
            AppError::Utf8(_) => t.invalid_utf8,
            AppError::Qr(_) => t.qr_error,
            AppError::Encrypt => t.encrypt_error,
            AppError::Decrypt => t.decrypt_error,

            AppError::PasswordRequired => {
                t.password_required
            }
            AppError::NoNoteInUrl => t.no_note_in_url,
            AppError::NoNoteParam => t.no_note_param,
            AppError::ClipboardWrite(_) => {
                t.clipboard_write_error
            }
            #[cfg(target_arch = "wasm32")]
            AppError::MissingWindow => {
                t.missing_window_error
            }
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
