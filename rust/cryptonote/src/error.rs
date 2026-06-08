use crate::messages::*;
use crate::prelude::*;
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

impl I18N for AppError {
    fn render_eng(&self) -> String {
        let msg = match self {
            Self::Cipher(_) | Self::KeyDerive(_) => {
                MsgCipherError.render_eng()
            }
            Self::Getrandom(_) => {
                MsgGetrandomError.render_eng()
            }
            Self::Base64(_) => MsgBase64Error.render_eng(),
            Self::Json(_) => MsgJsonError.render_eng(),
            Self::Utf8(_) => MsgInvalidUtf8.render_eng(),
            Self::Qr(_) => MsgQrError.render_eng(),
            Self::Encrypt => MsgEncryptError.render_eng(),
            Self::Decrypt => MsgDecryptError.render_eng(),
            Self::PasswordRequired => {
                MsgPasswordRequired.render_eng()
            }
            Self::NoNoteInUrl => {
                MsgNoNoteInUrl.render_eng()
            }
            Self::NoNoteParam => {
                MsgNoNoteParam.render_eng()
            }
            Self::Fd(FdError::CameraNotAvailable(_)) => {
                MsgQrCameraNotAvailable.render_eng()
            }
            Self::Fd(FdError::CameraPermissionDenied(
                _,
            )) => MsgQrPermissionDenied.render_eng(),
            Self::Fd(_) => {
                MsgClipboardReadError.render_eng()
            }
        };
        format!("{msg}: {self}")
    }

    fn render_spa(&self) -> String {
        let msg = match self {
            Self::Cipher(_) | Self::KeyDerive(_) => {
                MsgCipherError.render_spa()
            }
            Self::Getrandom(_) => {
                MsgGetrandomError.render_spa()
            }
            Self::Base64(_) => MsgBase64Error.render_spa(),
            Self::Json(_) => MsgJsonError.render_spa(),
            Self::Utf8(_) => MsgInvalidUtf8.render_spa(),
            Self::Qr(_) => MsgQrError.render_spa(),
            Self::Encrypt => MsgEncryptError.render_spa(),
            Self::Decrypt => MsgDecryptError.render_spa(),
            Self::PasswordRequired => {
                MsgPasswordRequired.render_spa()
            }
            Self::NoNoteInUrl => {
                MsgNoNoteInUrl.render_spa()
            }
            Self::NoNoteParam => {
                MsgNoNoteParam.render_spa()
            }
            Self::Fd(FdError::CameraNotAvailable(_)) => {
                MsgQrCameraNotAvailable.render_spa()
            }
            Self::Fd(FdError::CameraPermissionDenied(
                _,
            )) => MsgQrPermissionDenied.render_spa(),
            Self::Fd(_) => {
                MsgClipboardReadError.render_spa()
            }
        };
        format!("{msg}: {self}")
    }

    fn render_rus(&self) -> String {
        let msg = match self {
            Self::Cipher(_) | Self::KeyDerive(_) => {
                MsgCipherError.render_rus()
            }
            Self::Getrandom(_) => {
                MsgGetrandomError.render_rus()
            }
            Self::Base64(_) => MsgBase64Error.render_rus(),
            Self::Json(_) => MsgJsonError.render_rus(),
            Self::Utf8(_) => MsgInvalidUtf8.render_rus(),
            Self::Qr(_) => MsgQrError.render_rus(),
            Self::Encrypt => MsgEncryptError.render_rus(),
            Self::Decrypt => MsgDecryptError.render_rus(),
            Self::PasswordRequired => {
                MsgPasswordRequired.render_rus()
            }
            Self::NoNoteInUrl => {
                MsgNoNoteInUrl.render_rus()
            }
            Self::NoNoteParam => {
                MsgNoNoteParam.render_rus()
            }
            Self::Fd(FdError::CameraNotAvailable(_)) => {
                MsgQrCameraNotAvailable.render_rus()
            }
            Self::Fd(FdError::CameraPermissionDenied(
                _,
            )) => MsgQrPermissionDenied.render_rus(),
            Self::Fd(_) => {
                MsgClipboardReadError.render_rus()
            }
        };
        format!("{msg}: {self}")
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
