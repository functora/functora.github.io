use crate::i18n::I18N;
use std::sync::Arc;

#[derive(Debug, thiserror::Error)]
#[error(transparent)]
pub struct IoError(pub Arc<std::io::Error>);

impl PartialEq for IoError {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}
impl Eq for IoError {}

impl From<std::io::Error> for IoError {
    fn from(e: std::io::Error) -> Self {
        IoError(Arc::new(e))
    }
}

#[derive(Debug, thiserror::Error)]
#[error(transparent)]
pub struct JsonError(pub Arc<serde_json::Error>);

impl PartialEq for JsonError {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}
impl Eq for JsonError {}

impl From<serde_json::Error> for JsonError {
    fn from(e: serde_json::Error) -> Self {
        JsonError(Arc::new(e))
    }
}

#[derive(Debug, thiserror::Error)]
#[error(transparent)]
pub struct ZipErr(pub Arc<zip::result::ZipError>);

impl PartialEq for ZipErr {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}
impl Eq for ZipErr {}

impl From<zip::result::ZipError> for ZipErr {
    fn from(e: zip::result::ZipError) -> Self {
        ZipErr(Arc::new(e))
    }
}

#[cfg(target_os = "android")]
#[derive(Debug, thiserror::Error)]
#[error(transparent)]
pub struct JniError(pub Arc<jni::errors::Error>);

#[cfg(target_os = "android")]
impl PartialEq for JniError {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}
#[cfg(target_os = "android")]
impl Eq for JniError {}

#[cfg(target_os = "android")]
impl From<jni::errors::Error> for JniError {
    fn from(e: jni::errors::Error) -> Self {
        JniError(Arc::new(e))
    }
}

#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum Error {
    #[error("IO error: {0}")]
    IO(IoError),
    #[cfg(target_os = "android")]
    #[error("JNI error: {0}")]
    JNI(JniError),
    #[error("JSON error: {0}")]
    Json(JsonError),
    #[error("Base64 decoding error: {0}")]
    Base64(base64::DecodeError),
    #[cfg(feature = "qr")]
    #[error("QR code error: {0}")]
    Qr(#[from] rxing::Exceptions),
    #[error("Environment error: {0}")]
    Env(#[from] std::env::VarError),
    #[error("Channel error: {0}")]
    Channel(#[from] std::sync::mpsc::RecvError),
    #[error("JS error: {0}")]
    JS(String),
    #[error("Eval channel died before completion (dioxus internal GC/drop race)")]
    EvalFinished,
    #[error("Cipher initialization error: {0}")]
    Cipher(cipher::InvalidLength),
    #[error("Key derivation error: {0}")]
    KeyDerive(#[from] argon2::Error),
    #[error("Random number generation error: {0}")]
    Getrandom(#[from] getrandom::Error),
    #[error("Encryption failed: {0}")]
    Encrypt(aead::Error),
    #[error("Decryption failed: {0}")]
    Decrypt(aead::Error),
    #[error("Invalid encrypted payload format: {0}")]
    InvalidFormat(String),
    #[error("Numeric conversion failed ({context}): {source}")]
    Convert {
        context: &'static str,
        source: std::num::TryFromIntError,
    },
    #[error("Camera not available: {0}")]
    CameraNotAvailable(String),
    #[error("Camera permission denied: {0}")]
    CameraPermissionDenied(String),
    #[error("Not a JSON object: {0}")]
    NotJsonObject(serde_json::Value),
    #[error("Archive error: {0}")]
    Archive(ZipErr),
    #[error("Background task error: {0}")]
    Worker(WorkerStopped),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, thiserror::Error)]
#[error("Background task stopped unexpectedly")]
pub struct WorkerStopped;

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error::IO(IoError::from(e))
    }
}

impl From<serde_json::Error> for Error {
    fn from(e: serde_json::Error) -> Self {
        Error::Json(JsonError::from(e))
    }
}

impl From<base64::DecodeError> for Error {
    fn from(e: base64::DecodeError) -> Self {
        Error::Base64(e)
    }
}

impl From<cipher::InvalidLength> for Error {
    fn from(e: cipher::InvalidLength) -> Self {
        Error::Cipher(e)
    }
}

impl From<dioxus::document::EvalError> for Error {
    fn from(e: dioxus::document::EvalError) -> Self {
        match e {
            dioxus::document::EvalError::Finished => Error::EvalFinished,
            dioxus::document::EvalError::InvalidJs(js) => Error::JS(js),
            dioxus::document::EvalError::Communication(msg) => Error::JS(msg),
            dioxus::document::EvalError::Serialization(err) => Error::Json(JsonError::from(err)),
            other => Error::JS(other.to_string()),
        }
    }
}

#[cfg(target_os = "android")]
impl From<jni::errors::Error> for Error {
    fn from(e: jni::errors::Error) -> Self {
        Error::JNI(JniError::from(e))
    }
}

impl From<WorkerStopped> for Error {
    fn from(e: WorkerStopped) -> Self {
        Error::Worker(e)
    }
}

impl From<zip::result::ZipError> for Error {
    fn from(e: zip::result::ZipError) -> Self {
        Error::Archive(ZipErr::from(e))
    }
}

impl I18N for Error {
    fn render_eng(&self) -> String {
        match self {
            Self::IO(e) => format!("IO error: {e}"),
            Self::Json(e) => format!("JSON parsing error: {e}"),
            Self::Base64(e) => format!("Base64 decoding error: {e}"),
            #[cfg(feature = "qr")]
            Self::Qr(e) => format!("QR code error: {e}"),
            Self::Env(e) => format!("Environment variable error: {e}"),
            Self::Channel(e) => format!("Channel receive error: {e}"),
            Self::JS(e) => format!("JavaScript evaluation error: {e}"),
            Self::EvalFinished => "The connection to the browser was lost; please try again".into(),
            Self::Cipher(e) => format!("Cipher initialization error: {e}"),
            Self::KeyDerive(e) => format!("Key derivation error: {e}"),
            Self::Getrandom(e) => format!("Random number generation error: {e}"),
            Self::Encrypt(e) => format!("Encryption failed: {e}"),
            Self::Decrypt(e) => format!("Decryption failed: {e}"),
            Self::InvalidFormat(e) => format!("Invalid encrypted payload format: {e}"),
            Self::Convert { context, source } => format!("Numeric conversion failed ({context}): {source}"),
            Self::CameraNotAvailable(e) => format!("Camera is not available: {e}"),
            Self::CameraPermissionDenied(e) => format!("Camera permission was denied: {e}"),
            Self::NotJsonObject(e) => format!("Expected JSON object, got: {e}"),
            Self::Archive(e) => format!("Archive error: {e}"),
            Self::Worker(e) => format!("Background task error: {e}"),
            #[cfg(target_os = "android")]
            Self::JNI(e) => format!("JNI error: {e}"),
        }
    }

    fn render_spa(&self) -> String {
        match self {
            Self::IO(e) => format!("Error de E/S: {e}"),
            Self::Json(e) => format!("Error de análisis JSON: {e}"),
            Self::Base64(e) => format!("Error de decodificación Base64: {e}"),
            #[cfg(feature = "qr")]
            Self::Qr(e) => format!("Error de código QR: {e}"),
            Self::Env(e) => format!("Error de variable de entorno: {e}"),
            Self::Channel(e) => format!("Error de recepción en canal: {e}"),
            Self::JS(e) => format!("Error de evaluación JavaScript: {e}"),
            Self::EvalFinished => "Se perdió la conexión con el navegador; inténtalo de nuevo".into(),
            Self::Cipher(e) => format!("Error de inicialización de cifrado: {e}"),
            Self::KeyDerive(e) => format!("Error de derivación de clave: {e}"),
            Self::Getrandom(e) => format!("Error de generación de números aleatorios: {e}"),
            Self::Encrypt(e) => format!("Falló el cifrado: {e}"),
            Self::Decrypt(e) => format!("Falló el descifrado: {e}"),
            Self::InvalidFormat(e) => format!("Formato de carga útil cifrada no válido: {e}"),
            Self::Convert { context, source } => format!("Error de conversión numérica ({context}): {source}"),
            Self::CameraNotAvailable(e) => format!("La cámara no está disponible: {e}"),
            Self::CameraPermissionDenied(e) => format!("Permiso de cámara denegado: {e}"),
            Self::NotJsonObject(e) => format!("Se esperaba un objeto JSON, se obtuvo: {e}"),
            Self::Archive(e) => format!("Error de archivo: {e}"),
            Self::Worker(e) => format!("La tarea en segundo plano se detuvo inesperadamente (error: {e})"),
            #[cfg(target_os = "android")]
            Self::JNI(e) => format!("Error JNI: {e}"),
        }
    }

    fn render_rus(&self) -> String {
        match self {
            Self::IO(e) => format!("Ошибка ввода-вывода: {e}"),
            Self::Json(e) => format!("Ошибка разбора JSON: {e}"),
            Self::Base64(e) => format!("Ошибка декодирования Base64: {e}"),
            #[cfg(feature = "qr")]
            Self::Qr(e) => format!("Ошибка QR-кода: {e}"),
            Self::Env(e) => format!("Ошибка переменной окружения: {e}"),
            Self::Channel(e) => format!("Ошибка получения из канала: {e}"),
            Self::JS(e) => format!("Ошибка выполнения JavaScript: {e}"),
            Self::EvalFinished => "Соединение с браузером потеряно; попробуйте ещё раз".into(),
            Self::Cipher(e) => format!("Ошибка инициализации шифра: {e}"),
            Self::KeyDerive(e) => format!("Ошибка вывода ключа: {e}"),
            Self::Getrandom(e) => format!("Ошибка генерации случайных чисел: {e}"),
            Self::Encrypt(e) => format!("Ошибка шифрования: {e}"),
            Self::Decrypt(e) => format!("Ошибка расшифровки: {e}"),
            Self::InvalidFormat(e) => format!("Неверный формат зашифрованных данных: {e}"),
            Self::Convert { context, source } => format!("Ошибка численного преобразования ({context}): {source}"),
            Self::CameraNotAvailable(e) => format!("Камера недоступна: {e}"),
            Self::CameraPermissionDenied(e) => format!("Разрешение на камеру отклонено: {e}"),
            Self::NotJsonObject(e) => format!("Ожидался JSON-объект, получено: {e}"),
            Self::Archive(e) => format!("Ошибка архива: {e}"),
            Self::Worker(e) => format!("Ошибка фоновой задачи: {e}"),
            #[cfg(target_os = "android")]
            Self::JNI(e) => format!("Ошибка JNI: {e}"),
        }
    }
}
