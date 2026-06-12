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
                Msg::CipherError.render_eng()
            }
            Self::Getrandom(_) => {
                Msg::GetrandomError.render_eng()
            }
            Self::Base64(_) => {
                Msg::Base64Error.render_eng()
            }
            Self::Json(_) => Msg::JsonError.render_eng(),
            Self::Utf8(_) => Msg::InvalidUtf8.render_eng(),
            Self::Qr(_) => Msg::QrError.render_eng(),
            Self::Encrypt => Msg::EncryptError.render_eng(),
            Self::Decrypt => Msg::DecryptError.render_eng(),
            Self::PasswordRequired => {
                Msg::PasswordRequired.render_eng()
            }
            Self::NoNoteInUrl => {
                Msg::NoNoteInUrl.render_eng()
            }
            Self::NoNoteParam => {
                Msg::NoNoteParam.render_eng()
            }
            Self::Fd(FdError::CameraNotAvailable(_)) => {
                Msg::QrCameraNotAvailable.render_eng()
            }
            Self::Fd(FdError::CameraPermissionDenied(
                _,
            )) => Msg::QrPermissionDenied.render_eng(),
            Self::Fd(_) => {
                Msg::ClipboardReadError.render_eng()
            }
        };
        format!("{msg}: {self}")
    }

    fn render_spa(&self) -> String {
        let msg = match self {
            Self::Cipher(_) | Self::KeyDerive(_) => {
                Msg::CipherError.render_spa()
            }
            Self::Getrandom(_) => {
                Msg::GetrandomError.render_spa()
            }
            Self::Base64(_) => {
                Msg::Base64Error.render_spa()
            }
            Self::Json(_) => Msg::JsonError.render_spa(),
            Self::Utf8(_) => Msg::InvalidUtf8.render_spa(),
            Self::Qr(_) => Msg::QrError.render_spa(),
            Self::Encrypt => Msg::EncryptError.render_spa(),
            Self::Decrypt => Msg::DecryptError.render_spa(),
            Self::PasswordRequired => {
                Msg::PasswordRequired.render_spa()
            }
            Self::NoNoteInUrl => {
                Msg::NoNoteInUrl.render_spa()
            }
            Self::NoNoteParam => {
                Msg::NoNoteParam.render_spa()
            }
            Self::Fd(FdError::CameraNotAvailable(_)) => {
                Msg::QrCameraNotAvailable.render_spa()
            }
            Self::Fd(FdError::CameraPermissionDenied(
                _,
            )) => Msg::QrPermissionDenied.render_spa(),
            Self::Fd(_) => {
                Msg::ClipboardReadError.render_spa()
            }
        };
        format!("{msg}: {self}")
    }

    fn render_rus(&self) -> String {
        let msg = match self {
            Self::Cipher(_) | Self::KeyDerive(_) => {
                Msg::CipherError.render_rus()
            }
            Self::Getrandom(_) => {
                Msg::GetrandomError.render_rus()
            }
            Self::Base64(_) => {
                Msg::Base64Error.render_rus()
            }
            Self::Json(_) => Msg::JsonError.render_rus(),
            Self::Utf8(_) => Msg::InvalidUtf8.render_rus(),
            Self::Qr(_) => Msg::QrError.render_rus(),
            Self::Encrypt => Msg::EncryptError.render_rus(),
            Self::Decrypt => Msg::DecryptError.render_rus(),
            Self::PasswordRequired => {
                Msg::PasswordRequired.render_rus()
            }
            Self::NoNoteInUrl => {
                Msg::NoNoteInUrl.render_rus()
            }
            Self::NoNoteParam => {
                Msg::NoNoteParam.render_rus()
            }
            Self::Fd(FdError::CameraNotAvailable(_)) => {
                Msg::QrCameraNotAvailable.render_rus()
            }
            Self::Fd(FdError::CameraPermissionDenied(
                _,
            )) => Msg::QrPermissionDenied.render_rus(),
            Self::Fd(_) => {
                Msg::ClipboardReadError.render_rus()
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
