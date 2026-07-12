use crate::i18n::I18N;

#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum Error {
    #[error("IO error: {0}")]
    IO(String),
    #[cfg(target_os = "android")]
    #[error("JNI error: {0}")]
    JNI(String),
    #[error("JSON error: {0}")]
    Json(String),
    #[error("Environment error: {0}")]
    Env(#[from] std::env::VarError),
    #[error("Channel error: {0}")]
    Channel(#[from] std::sync::mpsc::RecvError),
    #[error("JS error: {0}")]
    JS(String),
    #[error("Camera not available: {0}")]
    CameraNotAvailable(String),
    #[error("Camera permission denied: {0}")]
    CameraPermissionDenied(String),
    #[error("Not a JSON object: {0}")]
    NotJsonObject(String),
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

impl I18N for Error {
    fn render_eng(&self) -> String {
        match self {
            Self::IO(e) => format!("IO error: {e}"),
            Self::Json(e) => format!("JSON parsing error: {e}"),
            Self::Env(e) => format!("Environment variable error: {e}"),
            Self::Channel(e) => format!("Channel receive error: {e}"),
            Self::JS(e) => format!("JavaScript evaluation error: {e}"),
            Self::CameraNotAvailable(e) => format!("Camera is not available: {e}"),
            Self::CameraPermissionDenied(e) => format!("Camera permission was denied: {e}"),
            Self::NotJsonObject(e) => format!("Expected JSON object, got: {e}"),
            #[cfg(target_os = "android")]
            Self::JNI(e) => format!("JNI error: {e}"),
        }
    }

    fn render_spa(&self) -> String {
        match self {
            Self::IO(e) => format!("Error de E/S: {e}"),
            Self::Json(e) => format!("Error de análisis JSON: {e}"),
            Self::Env(e) => format!("Error de variable de entorno: {e}"),
            Self::Channel(e) => format!("Error de recepción en canal: {e}"),
            Self::JS(e) => format!("Error de evaluación JavaScript: {e}"),
            Self::CameraNotAvailable(e) => format!("La cámara no está disponible: {e}"),
            Self::CameraPermissionDenied(e) => format!("Permiso de cámara denegado: {e}"),
            Self::NotJsonObject(e) => format!("Se esperaba un objeto JSON, se obtuvo: {e}"),
            #[cfg(target_os = "android")]
            Self::JNI(e) => format!("Error JNI: {e}"),
        }
    }

    fn render_rus(&self) -> String {
        match self {
            Self::IO(e) => format!("Ошибка ввода-вывода: {e}"),
            Self::Json(e) => format!("Ошибка разбора JSON: {e}"),
            Self::Env(e) => format!("Ошибка переменной окружения: {e}"),
            Self::Channel(e) => format!("Ошибка получения из канала: {e}"),
            Self::JS(e) => format!("Ошибка выполнения JavaScript: {e}"),
            Self::CameraNotAvailable(e) => format!("Камера недоступна: {e}"),
            Self::CameraPermissionDenied(e) => format!("Разрешение на камеру отклонено: {e}"),
            Self::NotJsonObject(e) => format!("Ожидался JSON-объект, получено: {e}"),
            #[cfg(target_os = "android")]
            Self::JNI(e) => format!("Ошибка JNI: {e}"),
        }
    }
}
