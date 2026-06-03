use std::fmt;

#[derive(Debug, Clone)]
pub enum Error {
    IO(String),
    JNI(String),
    Json(String),
    Env(std::env::VarError),
    Channel(String),
    JS(String),
    CameraNotAvailable(String),
    CameraPermissionDenied(String),
    NotJsonObject(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::IO(msg) => write!(f, "IO error: {msg}"),
            Error::JNI(msg) => write!(f, "JNI error: {msg}"),
            Error::Json(msg) => write!(f, "JSON error: {msg}"),
            Error::Env(err) => write!(f, "Environment error: {err}"),
            Error::Channel(msg) => write!(f, "Channel error: {msg}"),
            Error::JS(msg) => write!(f, "JS error: {msg}"),
            Error::CameraNotAvailable(msg) => write!(f, "Camera not available: {msg}"),
            Error::CameraPermissionDenied(msg) => write!(f, "Camera permission denied: {msg}"),
            Error::NotJsonObject(msg) => write!(f, "Not a JSON object: {msg}"),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Error::Env(err) => Some(err),
            _ => None,
        }
    }
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error::IO(e.to_string())
    }
}

impl From<serde_json::Error> for Error {
    fn from(e: serde_json::Error) -> Self {
        Error::Json(e.to_string())
    }
}

impl From<std::env::VarError> for Error {
    fn from(e: std::env::VarError) -> Self {
        Error::Env(e)
    }
}

impl From<std::sync::mpsc::RecvError> for Error {
    fn from(e: std::sync::mpsc::RecvError) -> Self {
        Error::Channel(e.to_string())
    }
}

impl From<dioxus::document::EvalError> for Error {
    fn from(e: dioxus::document::EvalError) -> Self {
        Error::JS(e.to_string())
    }
}

#[cfg(target_os = "android")]
impl From<jni::errors::Error> for Error {
    fn from(e: jni::errors::Error) -> Self {
        Error::JNI(e.to_string())
    }
}
