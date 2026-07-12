use crate::prelude::*;
use functora_dioxus::i18n::I18N;
use sha2::digest;
use std::string::FromUtf8Error;

#[derive(Debug, Clone, PartialEq, Display, Error)]
pub enum AppError {
    Cipher(#[from] digest::InvalidLength),
    KeyDerive(String),
    Getrandom(getrandom::Error),
    Base64(#[from] base64::DecodeError),
    Json(String),
    Utf8(#[from] FromUtf8Error),
    Qr(#[from] rxing::Exceptions),
    Encrypt(String),
    Decrypt(String),
    PasswordRequired,
    NoNoteInUrl,
    NoNoteParam,
    Fd(#[from] functora_dioxus::Error),
}

impl I18N for AppError {
    fn render_eng(&self) -> String {
        match self {
            Self::Cipher(e) => format!("Cipher initialization error: {e}"),
            Self::KeyDerive(e) => format!("Key derivation error: {e}"),
            Self::Getrandom(e) => format!("Random number generation error: {e}"),
            Self::Base64(e) => format!("Base64 decoding error: {e}"),
            Self::Json(e) => format!("JSON parsing error: {e}"),
            Self::Utf8(e) => format!("Decrypted data is not valid UTF-8: {e}"),
            Self::Qr(e) => format!("QR code generation error: {e}"),
            Self::Encrypt(e) => format!("Encryption failed: {e}"),
            Self::Decrypt(e) => format!("Decryption failed: {e}"),
            Self::PasswordRequired => "Password is required".into(),
            Self::NoNoteInUrl => "No note found in URL".into(),
            Self::NoNoteParam => "URL does not contain a note parameter".into(),
            Self::Fd(e) => e.render_eng(),
        }
    }

    fn render_spa(&self) -> String {
        match self {
            Self::Cipher(e) => format!("Error de inicialización de cifrado: {e}"),
            Self::KeyDerive(e) => format!("Error de derivación de clave: {e}"),
            Self::Getrandom(e) => format!("Error de generación de números aleatorios: {e}"),
            Self::Base64(e) => format!("Error de decodificación Base64: {e}"),
            Self::Json(e) => format!("Error de análisis JSON: {e}"),
            Self::Utf8(e) => format!("Los datos descifrados no son UTF-8 válidos: {e}"),
            Self::Qr(e) => format!("Error de generación de código QR: {e}"),
            Self::Encrypt(e) => format!("Falló el cifrado: {e}"),
            Self::Decrypt(e) => format!("Falló el descifrado: {e}"),
            Self::PasswordRequired => "Se requiere contraseña".into(),
            Self::NoNoteInUrl => "No se encontró nota en la URL".into(),
            Self::NoNoteParam => "La URL no contiene un parámetro de nota".into(),
            Self::Fd(e) => e.render_spa(),
        }
    }

    fn render_rus(&self) -> String {
        match self {
            Self::Cipher(e) => format!("Ошибка инициализации шифра: {e}"),
            Self::KeyDerive(e) => format!("Ошибка вывода ключа: {e}"),
            Self::Getrandom(e) => format!("Ошибка генерации случайных чисел: {e}"),
            Self::Base64(e) => format!("Ошибка декодирования Base64: {e}"),
            Self::Json(e) => format!("Ошибка разбора JSON: {e}"),
            Self::Utf8(e) => format!("Расшифрованные данные не являются допустимым UTF-8: {e}"),
            Self::Qr(e) => format!("Ошибка генерации QR-кода: {e}"),
            Self::Encrypt(e) => format!("Ошибка шифрования: {e}"),
            Self::Decrypt(e) => format!("Ошибка расшифровки: {e}"),
            Self::PasswordRequired => "Требуется пароль".into(),
            Self::NoNoteInUrl => "Заметка не найдена в URL".into(),
            Self::NoNoteParam => "URL не содержит параметр заметки".into(),
            Self::Fd(e) => e.render_rus(),
        }
    }
}

impl From<hkdf::InvalidLength> for AppError {
    fn from(e: hkdf::InvalidLength) -> Self {
        AppError::KeyDerive(e.to_string())
    }
}

impl From<serde_json::Error> for AppError {
    fn from(e: serde_json::Error) -> Self {
        AppError::Json(e.to_string())
    }
}

impl From<getrandom::Error> for AppError {
    fn from(e: getrandom::Error) -> Self {
        AppError::Getrandom(e)
    }
}
