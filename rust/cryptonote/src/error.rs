use crate::prelude::*;
use crate::*;
use hkdf::InvalidLength;
use sha2::digest;
use std::string::FromUtf8Error;

#[derive(Debug, Display, Error)]
pub enum AppError {
    Cipher(#[from] digest::InvalidLength),
    KeyDerive(InvalidLength),
    Getrandom(getrandom::Error),
    Base64(#[from] base64::DecodeError),
    Json(#[from] serde_json::Error),
    Utf8(#[from] FromUtf8Error),
    Qr(#[from] rxing::Exceptions),
    Encrypt,
    Decrypt,
    PasswordRequired,
    NoNoteInUrl,
    NoNoteParam,
    JsWriteClipboard(EvalError),
    CameraNotAvailable,
    CameraPermissionDenied,
    QrDecode,
}

impl AppError {
    pub fn localized(
        &self,
        t: &i18n::Translations,
    ) -> String {
        let msg = match self {
            AppError::Cipher(_) => t.cipher_error,
            AppError::KeyDerive(_) => t.cipher_error,
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
            AppError::JsWriteClipboard(_) => {
                t.clipboard_write_error
            }
            AppError::CameraNotAvailable => {
                t.qr_camera_not_available
            }
            AppError::CameraPermissionDenied => {
                t.qr_permission_denied
            }
            AppError::QrDecode => t.qr_error,
        };
        format!("{}: {}", msg, self)
    }
}

impl From<InvalidLength> for AppError {
    fn from(e: InvalidLength) -> Self {
        AppError::KeyDerive(e)
    }
}

impl From<getrandom::Error> for AppError {
    fn from(e: getrandom::Error) -> Self {
        AppError::Getrandom(e)
    }
}
