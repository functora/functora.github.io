use functora_egui::error::JsonError;
use functora_egui::i18n::I18N;
use std::string::FromUtf8Error;
use std::sync::Arc;

#[derive(Clone, Debug)]
pub struct MsgError(pub Arc<AppError>);

impl From<AppError> for MsgError {
    fn from(e: AppError) -> Self {
        Self(Arc::new(e))
    }
}

impl std::ops::Deref for MsgError {
    type Target = AppError;

    fn deref(&self) -> &AppError {
        &self.0
    }
}

#[derive(Debug, thiserror::Error)]
pub enum AppError {
    #[error(transparent)]
    Json(JsonError),
    #[error(transparent)]
    Utf8(#[from] FromUtf8Error),
    #[error(transparent)]
    Base64(#[from] base64::DecodeError),
    #[error(transparent)]
    Zip(#[from] functora_egui::error::ZipErr),
    #[error("Password is required")]
    PasswordRequired,
    #[error(transparent)]
    QrScan(std::sync::Arc<functora_egui::error::Error>),
    #[error("No file selected")]
    NoFileSelected,
    #[error("No note found in URL")]
    NoNoteInUrl,
    #[error("URL does not contain a note parameter")]
    NoNoteParam,
    #[error("Cancelled")]
    Cancelled,
    #[error(transparent)]
    FunctoraEgui(#[from] functora_egui::error::Error),
    #[error(transparent)]
    Io(#[from] std::io::Error),
}

impl I18N for AppError {
    fn render_eng(&self) -> String {
        match self {
            Self::Json(e) => format!("JSON parsing error: {e}"),
            Self::Utf8(e) => format!("Decrypted data is not valid UTF-8: {e}"),
            Self::Base64(e) => format!("Base64 decoding error: {e}"),
            Self::Zip(e) => format!("Archive error: {e}"),
            Self::PasswordRequired => "Password is required".into(),
            Self::QrScan(e) => e.render_eng(),
            Self::NoFileSelected => "No file selected".into(),
            Self::NoNoteInUrl => "No note found in URL".into(),
            Self::NoNoteParam => "URL does not contain a note parameter".into(),
            Self::Cancelled => "Cancelled".into(),
            Self::FunctoraEgui(e) => e.render_eng(),
            Self::Io(e) => format!("I/O error: {e}"),
        }
    }

    fn render_spa(&self) -> String {
        match self {
            Self::Json(e) => format!("Error de análisis JSON: {e}"),
            Self::Utf8(e) => format!("Los datos descifrados no son UTF-8 válidos: {e}"),
            Self::Base64(e) => format!("Error de decodificación Base64: {e}"),
            Self::Zip(e) => format!("Error de archivo: {e}"),
            Self::PasswordRequired => "Se requiere contraseña".into(),
            Self::QrScan(e) => e.render_spa(),
            Self::NoFileSelected => "Ningún archivo seleccionado".into(),
            Self::NoNoteInUrl => "No se encontró nota en la URL".into(),
            Self::NoNoteParam => "La URL no contiene un parámetro de nota".into(),
            Self::Cancelled => "Cancelado".into(),
            Self::FunctoraEgui(e) => e.render_spa(),
            Self::Io(e) => format!("Error de E/S: {e}"),
        }
    }

    fn render_rus(&self) -> String {
        match self {
            Self::Json(e) => format!("Ошибка разбора JSON: {e}"),
            Self::Utf8(e) => format!("Расшифрованные данные не являются допустимым UTF-8: {e}"),
            Self::Base64(e) => format!("Ошибка декодирования Base64: {e}"),
            Self::Zip(e) => format!("Ошибка архива: {e}"),
            Self::PasswordRequired => "Требуется пароль".into(),
            Self::QrScan(e) => e.render_rus(),
            Self::NoFileSelected => "Файл не выбран".into(),
            Self::NoNoteInUrl => "Заметка не найдена в URL".into(),
            Self::NoNoteParam => "URL не содержит параметр заметки".into(),
            Self::Cancelled => "Отменено".into(),
            Self::FunctoraEgui(e) => e.render_rus(),
            Self::Io(e) => format!("Ошибка ввода-вывода: {e}"),
        }
    }
}

impl From<serde_json::Error> for AppError {
    fn from(e: serde_json::Error) -> Self {
        Self::Json(e.into())
    }
}
