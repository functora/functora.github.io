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
        let msg = match self {
            Self::IO(_) => MsgClipboardReadError.render_eng(),
            Self::Json(_) => MsgClipboardReadError.render_eng(),
            Self::Env(_) => MsgClipboardReadError.render_eng(),
            Self::Channel(_) => MsgClipboardReadError.render_eng(),
            Self::JS(_) => MsgClipboardReadError.render_eng(),
            Self::CameraNotAvailable(_) => MsgClipboardReadError.render_eng(),
            Self::CameraPermissionDenied(_) => MsgClipboardReadError.render_eng(),
            Self::NotJsonObject(_) => MsgClipboardReadError.render_eng(),
            #[cfg(target_os = "android")]
            Self::JNI(_) => MsgClipboardReadError.render_eng(),
        };
        format!("{msg}: {self}")
    }

    fn render_spa(&self) -> String {
        let msg = match self {
            Self::IO(_) => MsgClipboardReadError.render_spa(),
            Self::Json(_) => MsgClipboardReadError.render_spa(),
            Self::Env(_) => MsgClipboardReadError.render_spa(),
            Self::Channel(_) => MsgClipboardReadError.render_spa(),
            Self::JS(_) => MsgClipboardReadError.render_spa(),
            Self::CameraNotAvailable(_) => MsgClipboardReadError.render_spa(),
            Self::CameraPermissionDenied(_) => MsgClipboardReadError.render_spa(),
            Self::NotJsonObject(_) => MsgClipboardReadError.render_spa(),
            #[cfg(target_os = "android")]
            Self::JNI(_) => MsgClipboardReadError.render_spa(),
        };
        format!("{msg}: {self}")
    }

    fn render_rus(&self) -> String {
        let msg = match self {
            Self::IO(_) => MsgClipboardReadError.render_rus(),
            Self::Json(_) => MsgClipboardReadError.render_rus(),
            Self::Env(_) => MsgClipboardReadError.render_rus(),
            Self::Channel(_) => MsgClipboardReadError.render_rus(),
            Self::JS(_) => MsgClipboardReadError.render_rus(),
            Self::CameraNotAvailable(_) => MsgClipboardReadError.render_rus(),
            Self::CameraPermissionDenied(_) => MsgClipboardReadError.render_rus(),
            Self::NotJsonObject(_) => MsgClipboardReadError.render_rus(),
            #[cfg(target_os = "android")]
            Self::JNI(_) => MsgClipboardReadError.render_rus(),
        };
        format!("{msg}: {self}")
    }
}
