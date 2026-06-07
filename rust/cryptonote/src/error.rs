use crate::messages::*;
use crate::prelude::*;
use functora_dioxus::i18n::Language;
use functora_dioxus::i18n::I18N;
use functora_dioxus::Error as FdError;
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
    Fd(#[from] FdError),
}

impl AppError {
    pub fn localized(&self, lang: Language) -> String {
        let msg = match self {
            AppError::Cipher(_)
            | AppError::KeyDerive(_) => {
                MsgCipherError.render(lang)
            }
            AppError::Getrandom(_) => {
                MsgGetrandomError.render(lang)
            }
            AppError::Base64(_) => {
                MsgBase64Error.render(lang)
            }
            AppError::Json(_) => MsgJsonError.render(lang),
            AppError::Utf8(_) => {
                MsgInvalidUtf8.render(lang)
            }
            AppError::Qr(_) => MsgQrError.render(lang),
            AppError::Encrypt => {
                MsgEncryptError.render(lang)
            }
            AppError::Decrypt => {
                MsgDecryptError.render(lang)
            }
            AppError::PasswordRequired => {
                MsgPasswordRequired.render(lang)
            }
            AppError::NoNoteInUrl => {
                MsgNoNoteInUrl.render(lang)
            }
            AppError::NoNoteParam => {
                MsgNoNoteParam.render(lang)
            }
            AppError::Fd(FdError::CameraNotAvailable(
                _,
            )) => MsgQrCameraNotAvailable.render(lang),
            AppError::Fd(
                FdError::CameraPermissionDenied(_),
            ) => MsgQrPermissionDenied.render(lang),
            AppError::Fd(_) => {
                MsgClipboardReadError.render(lang)
            }
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
