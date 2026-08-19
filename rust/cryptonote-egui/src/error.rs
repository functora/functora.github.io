use functora_core::error::JsonError;
use functora_core::i18n::I18N;
use functora_core::Error as CoreError;
use std::string::FromUtf8Error;
use std::sync::Arc;
use thiserror::Error;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MsgError(Arc<AppError>);

impl From<AppError> for MsgError {
    fn from(e: AppError) -> Self {
        MsgError(Arc::new(e))
    }
}

impl std::ops::Deref for MsgError {
    type Target = AppError;

    fn deref(&self) -> &AppError {
        &self.0
    }
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum AppError {
    #[error("JSON parsing error: {0}")]
    Json(#[from] JsonError),
    #[error("Decrypted data is not valid UTF-8: {0}")]
    Utf8(#[from] FromUtf8Error),
    #[error("Password is required")]
    PasswordRequired,
    #[error("Invalid encrypted payload format: {0}")]
    InvalidFormat(String),
    #[error("Archive error: {0}")]
    Archive(String),
    #[error("No note found in URL")]
    NoNoteInUrl,
    #[error("URL does not contain a note parameter")]
    NoNoteParam,
    #[error(transparent)]
    FunctoraCore(#[from] CoreError),
    #[error("Platform error: {0}")]
    Platform(String),
}

impl From<serde_json::Error> for AppError {
    fn from(e: serde_json::Error) -> Self {
        AppError::Json(JsonError::from(e))
    }
}

impl I18N for AppError {
    fn render_eng(&self) -> String {
        match self {
            Self::Json(e) => format!("JSON parsing error: {e}"),
            Self::Utf8(e) => format!("Decrypted data is not valid UTF-8: {e}"),
            Self::PasswordRequired => "Password is required".into(),
            Self::InvalidFormat(e) => format!("Invalid encrypted payload format: {e}"),
            Self::Archive(e) => format!("Archive error: {e}"),
            Self::NoNoteInUrl => "No note found in URL".into(),
            Self::NoNoteParam => "URL does not contain a note parameter".into(),
            Self::FunctoraCore(e) => e.render_eng(),
            Self::Platform(e) => format!("Platform error: {e}"),
        }
    }

    fn render_spa(&self) -> String {
        match self {
            Self::Json(e) => format!("Error de análisis JSON: {e}"),
            Self::Utf8(e) => format!("Los datos descifrados no son UTF-8 válidos: {e}"),
            Self::PasswordRequired => "Se requiere contraseña".into(),
            Self::InvalidFormat(e) => format!("Formato de carga útil cifrada no válido: {e}"),
            Self::Archive(e) => format!("Error de archivo: {e}"),
            Self::NoNoteInUrl => "No se encontró nota en la URL".into(),
            Self::NoNoteParam => "La URL no contiene un parámetro de nota".into(),
            Self::FunctoraCore(e) => e.render_spa(),
            Self::Platform(e) => format!("Error de plataforma: {e}"),
        }
    }

    fn render_rus(&self) -> String {
        match self {
            Self::Json(e) => format!("Ошибка разбора JSON: {e}"),
            Self::Utf8(e) => format!("Расшифрованные данные не являются допустимым UTF-8: {e}"),
            Self::PasswordRequired => "Требуется пароль".into(),
            Self::InvalidFormat(e) => format!("Неверный формат зашифрованных данных: {e}"),
            Self::Archive(e) => format!("Ошибка архива: {e}"),
            Self::NoNoteInUrl => "Заметка не найдена в URL".into(),
            Self::NoNoteParam => "URL не содержит параметр заметки".into(),
            Self::FunctoraCore(e) => e.render_rus(),
            Self::Platform(e) => format!("Ошибка платформы: {e}"),
        }
    }
}
