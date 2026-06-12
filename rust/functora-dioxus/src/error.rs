use crate::i18n::I18N;
use crate::messages::*;
use std::sync::Arc;

#[derive(Debug, Clone, thiserror::Error)]
pub enum Error {
    #[error("IO error: {0}")]
    IO(#[source] Arc<std::io::Error>),
    #[cfg(target_os = "android")]
    #[error("JNI error: {0}")]
    JNI(#[source] Arc<jni::errors::Error>),
    #[error("JSON error: {0}")]
    Json(#[source] Arc<serde_json::Error>),
    #[error("Environment error: {0}")]
    Env(#[from] std::env::VarError),
    #[error("Channel error: {0}")]
    Channel(#[from] std::sync::mpsc::RecvError),
    #[error("JS error: {0}")]
    JS(#[source] Arc<dioxus::document::EvalError>),
    #[error("Camera not available: {0}")]
    CameraNotAvailable(String),
    #[error("Camera permission denied: {0}")]
    CameraPermissionDenied(String),
    #[error("Not a JSON object: {0}")]
    NotJsonObject(String),
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error::IO(Arc::new(e))
    }
}

impl From<serde_json::Error> for Error {
    fn from(e: serde_json::Error) -> Self {
        Error::Json(Arc::new(e))
    }
}

impl From<dioxus::document::EvalError> for Error {
    fn from(e: dioxus::document::EvalError) -> Self {
        Error::JS(Arc::new(e))
    }
}

#[cfg(target_os = "android")]
impl From<jni::errors::Error> for Error {
    fn from(e: jni::errors::Error) -> Self {
        Error::JNI(Arc::new(e))
    }
}

impl I18N for Error {
    fn render_eng(&self) -> String {
        format!("{}: {self}", Msg::ClipboardReadError.render_eng())
    }

    fn render_spa(&self) -> String {
        format!("{}: {self}", Msg::ClipboardReadError.render_spa())
    }

    fn render_rus(&self) -> String {
        format!("{}: {self}", Msg::ClipboardReadError.render_rus())
    }
}
