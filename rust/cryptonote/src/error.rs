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

impl AppError {
    fn msg_variant(&self) -> Option<Msg> {
        Some(match self {
            Self::Cipher(_) | Self::KeyDerive(_) => {
                Msg::CipherError
            }
            Self::Getrandom(_) => Msg::GetrandomError,
            Self::Base64(_) => Msg::Base64Error,
            Self::Json(_) => Msg::JsonError,
            Self::Utf8(_) => Msg::InvalidUtf8,
            Self::Qr(_) => Msg::QrError,
            Self::Encrypt => Msg::EncryptError,
            Self::Decrypt => Msg::DecryptError,
            Self::PasswordRequired => Msg::PasswordRequired,
            Self::NoNoteInUrl => Msg::NoNoteInUrl,
            Self::NoNoteParam => Msg::NoNoteParam,
            Self::Fd(_) => return None,
        })
    }
}

impl I18N for AppError {
    fn render_eng(&self) -> String {
        match self {
            Self::Fd(e) => e.render_eng(),
            _ => self
                .msg_variant()
                .map(|m| m.render_eng())
                .unwrap_or_default(),
        }
    }

    fn render_spa(&self) -> String {
        match self {
            Self::Fd(e) => e.render_spa(),
            _ => self
                .msg_variant()
                .map(|m| m.render_spa())
                .unwrap_or_default(),
        }
    }

    fn render_rus(&self) -> String {
        match self {
            Self::Fd(e) => e.render_rus(),
            _ => self
                .msg_variant()
                .map(|m| m.render_rus())
                .unwrap_or_default(),
        }
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
