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

#[cfg(feature = "zip")]
#[derive(Debug, thiserror::Error)]
#[error(transparent)]
pub struct ZipErr(pub Arc<zip::result::ZipError>);

#[cfg(feature = "zip")]
impl PartialEq for ZipErr {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}
#[cfg(feature = "zip")]
impl Eq for ZipErr {}

#[cfg(feature = "zip")]
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
    #[error("No QR found in image")]
    QrNotFound,
    #[error("Eval channel died before completion (dioxus internal GC/drop race)")]
    EvalFinished,
    #[cfg(feature = "crypto")]
    #[error("Cipher initialization error: {0}")]
    Cipher(cipher::InvalidLength),
    #[cfg(feature = "crypto")]
    #[error("Key derivation error: {0}")]
    KeyDerive(#[from] argon2::Error),
    #[cfg(feature = "crypto")]
    #[error("Random number generation error: {0}")]
    Getrandom(#[from] getrandom::Error),
    #[cfg(feature = "crypto")]
    #[error("Encryption failed: {0}")]
    Encrypt(aead::Error),
    #[cfg(feature = "crypto")]
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
    #[error("Camera feed stalled: no frames received")]
    CameraStalled,
    #[error("Not a JSON object: {0}")]
    NotJsonObject(serde_json::Value),
    #[cfg(feature = "zip")]
    #[error("Archive error: {0}")]
    Archive(ZipErr),
    #[error("Background task error: {0}")]
    Worker(WorkerStopped),
    #[error("File too large ({name}: {size} bytes, limit {limit} bytes)")]
    FileTooLarge { name: String, size: u64, limit: u64 },
    #[error("Operation cancelled")]
    Cancelled,
    #[error("Required object unavailable: {0}")]
    Missing(Missing),
    #[error("{0} is unavailable: web feature disabled")]
    CapabilityDisabled(Capability),
    #[error("Clipboard error: {0}")]
    Clipboard(ClipboardError),
    #[cfg(target_arch = "wasm32")]
    #[error("JavaScript operation failed: {0}")]
    Js(JsError),
    #[error("File picker timed out")]
    FilePickerTimedOut,
    #[error("File picker failed")]
    FilePickerFailed,
    #[error("Save was cancelled")]
    SaveCancelled,
    #[error("xdg-open failed for {url}")]
    OpenUrlFailed { url: String },
    #[error("xdg-open timed out for {url}")]
    OpenUrlTimedOut { url: String },
}

/// A browser object, platform handle, or storage location the operation
/// requires, but which the current platform or build does not provide.
#[derive(Debug, Clone, Copy, PartialEq, Eq, thiserror::Error)]
pub enum Missing {
    #[error("window")]
    Window,
    #[error("document")]
    Document,
    #[error("document body")]
    Body,
    #[error("video element")]
    VideoElement,
    #[error("canvas")]
    Canvas,
    #[error("2D context")]
    Context2d,
    #[error("localStorage")]
    LocalStorage,
    #[error("anchor element")]
    Anchor,
    #[error("input element")]
    Input,
    #[error("file entry")]
    FileEntry,
    #[error("clipboard text")]
    ClipboardText,
    #[error("desktop session")]
    DesktopSession,
    #[error("Android application")]
    AndroidApp,
    #[error("data directory")]
    DataDir,
}

/// A capability that the current build does not enable.
#[derive(Debug, Clone, Copy, PartialEq, Eq, thiserror::Error)]
pub enum Capability {
    #[error("clipboard")]
    Clipboard,
    #[error("share")]
    Share,
    #[error("save")]
    Save,
}

/// A clipboard that exists but refused the operation.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum ClipboardError {
    #[error("unavailable")]
    Unavailable,
    #[error("busy")]
    Busy,
    #[error("content unavailable")]
    ContentUnavailable,
    #[error("conversion failed")]
    ConversionFailed,
    #[error("{0}")]
    Unknown(String),
}

/// A JavaScript value rejected by a Web API call, kept alive so that its
/// original payload stays inspectable.
#[cfg(target_arch = "wasm32")]
#[derive(Debug, Clone, thiserror::Error)]
#[error("{0:?}")]
pub struct JsError(pub Arc<web_sys::wasm_bindgen::JsValue>);

#[cfg(target_arch = "wasm32")]
impl PartialEq for JsError {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

#[cfg(target_arch = "wasm32")]
impl Eq for JsError {}

#[cfg(target_arch = "wasm32")]
impl From<web_sys::wasm_bindgen::JsValue> for JsError {
    fn from(e: web_sys::wasm_bindgen::JsValue) -> Self {
        Self(Arc::new(e))
    }
}

#[cfg(target_arch = "wasm32")]
impl From<web_sys::wasm_bindgen::JsValue> for Error {
    fn from(e: web_sys::wasm_bindgen::JsValue) -> Self {
        Self::Js(JsError::from(e))
    }
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

#[cfg(feature = "crypto")]
impl From<cipher::InvalidLength> for Error {
    fn from(e: cipher::InvalidLength) -> Self {
        Error::Cipher(e)
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

#[cfg(feature = "zip")]
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
            Self::QrNotFound => "No QR found in image".into(),
            Self::EvalFinished => "The connection to the browser was lost; please try again".into(),
            #[cfg(feature = "crypto")]
            Self::Cipher(e) => format!("Cipher initialization error: {e}"),
            #[cfg(feature = "crypto")]
            Self::KeyDerive(e) => format!("Key derivation error: {e}"),
            #[cfg(feature = "crypto")]
            Self::Getrandom(e) => format!("Random number generation error: {e}"),
            #[cfg(feature = "crypto")]
            Self::Encrypt(e) => format!("Encryption failed: {e}"),
            #[cfg(feature = "crypto")]
            Self::Decrypt(e) => format!("Decryption failed: {e}"),
            Self::InvalidFormat(e) => format!("Invalid encrypted payload format: {e}"),
            Self::Convert { context, source } => {
                format!("Numeric conversion failed ({context}): {source}")
            }
            Self::CameraNotAvailable(e) => format!("Camera is not available: {e}"),
            Self::CameraPermissionDenied(e) => format!("Camera permission was denied: {e}"),
            Self::CameraStalled => "The camera feed stopped responding; try again".into(),
            Self::NotJsonObject(e) => format!("Expected JSON object, got: {e}"),
            #[cfg(feature = "zip")]
            Self::Archive(e) => format!("Archive error: {e}"),
            Self::Worker(e) => format!("Background task error: {e}"),
            Self::FileTooLarge { name, size, limit } => {
                format!("File too large ({name}: {size} bytes, limit {limit} bytes)")
            }
            Self::Cancelled => "Operation cancelled".into(),
            Self::Missing(m) => format!("Required object unavailable: {m}"),
            Self::CapabilityDisabled(c) => format!("{c} is unavailable: web feature disabled"),
            Self::Clipboard(e) => format!("Clipboard error: {e}"),
            #[cfg(target_arch = "wasm32")]
            Self::Js(e) => format!("JavaScript operation failed: {e}"),
            Self::FilePickerTimedOut => "File picker timed out".into(),
            Self::FilePickerFailed => "File picker failed".into(),
            Self::SaveCancelled => "Save was cancelled".into(),
            Self::OpenUrlFailed { url } => format!("xdg-open failed for {url}"),
            Self::OpenUrlTimedOut { url } => format!("xdg-open timed out for {url}"),
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
            Self::QrNotFound => "No se encontró ningún QR en la imagen".into(),
            Self::EvalFinished => {
                "Se perdió la conexión con el navegador; inténtalo de nuevo".into()
            }
            #[cfg(feature = "crypto")]
            Self::Cipher(e) => format!("Error de inicialización de cifrado: {e}"),
            #[cfg(feature = "crypto")]
            Self::KeyDerive(e) => format!("Error de derivación de clave: {e}"),
            #[cfg(feature = "crypto")]
            Self::Getrandom(e) => format!("Error de generación de números aleatorios: {e}"),
            #[cfg(feature = "crypto")]
            Self::Encrypt(e) => format!("Falló el cifrado: {e}"),
            #[cfg(feature = "crypto")]
            Self::Decrypt(e) => format!("Falló el descifrado: {e}"),
            Self::InvalidFormat(e) => format!("Formato de carga útil cifrada no válido: {e}"),
            Self::Convert { context, source } => {
                format!("Error de conversión numérica ({context}): {source}")
            }
            Self::CameraNotAvailable(e) => format!("La cámara no está disponible: {e}"),
            Self::CameraPermissionDenied(e) => format!("Permiso de cámara denegado: {e}"),
            Self::CameraStalled => "La cámara dejó de responder; inténtalo de nuevo".into(),
            Self::NotJsonObject(e) => format!("Se esperaba un objeto JSON, se obtuvo: {e}"),
            #[cfg(feature = "zip")]
            Self::Archive(e) => format!("Error de archivo: {e}"),
            Self::Worker(e) => {
                format!("La tarea en segundo plano se detuvo inesperadamente (error: {e})")
            }
            Self::FileTooLarge { name, size, limit } => {
                format!("Archivo demasiado grande ({name}: {size} bytes, límite {limit} bytes)")
            }
            Self::Cancelled => "Operación cancelada".into(),
            Self::Missing(m) => format!("Objeto requerido no disponible: {m}"),
            Self::CapabilityDisabled(c) => {
                format!("{c} no está disponible: función web deshabilitada")
            }
            Self::Clipboard(e) => format!("Error de portapapeles: {e}"),
            #[cfg(target_arch = "wasm32")]
            Self::Js(e) => format!("La operación JavaScript falló: {e}"),
            Self::FilePickerTimedOut => "El selector de archivos agotó el tiempo de espera".into(),
            Self::FilePickerFailed => "El selector de archivos falló".into(),
            Self::SaveCancelled => "Se canceló el guardado".into(),
            Self::OpenUrlFailed { url } => format!("xdg-open falló al abrir {url}"),
            Self::OpenUrlTimedOut { url } => format!("xdg-open agotó el tiempo para {url}"),
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
            Self::QrNotFound => "QR-код не найден на изображении".into(),
            Self::EvalFinished => "Соединение с браузером потеряно; попробуйте ещё раз".into(),
            #[cfg(feature = "crypto")]
            Self::Cipher(e) => format!("Ошибка инициализации шифра: {e}"),
            #[cfg(feature = "crypto")]
            Self::KeyDerive(e) => format!("Ошибка вывода ключа: {e}"),
            #[cfg(feature = "crypto")]
            Self::Getrandom(e) => format!("Ошибка генерации случайных чисел: {e}"),
            #[cfg(feature = "crypto")]
            Self::Encrypt(e) => format!("Ошибка шифрования: {e}"),
            #[cfg(feature = "crypto")]
            Self::Decrypt(e) => format!("Ошибка расшифровки: {e}"),
            Self::InvalidFormat(e) => format!("Неверный формат зашифрованных данных: {e}"),
            Self::Convert { context, source } => {
                format!("Ошибка численного преобразования ({context}): {source}")
            }
            Self::CameraNotAvailable(e) => format!("Камера недоступна: {e}"),
            Self::CameraPermissionDenied(e) => format!("Разрешение на камеру отклонено: {e}"),
            Self::CameraStalled => "Поток с камеры остановился; попробуйте ещё раз".into(),
            Self::NotJsonObject(e) => format!("Ожидался JSON-объект, получено: {e}"),
            #[cfg(feature = "zip")]
            Self::Archive(e) => format!("Ошибка архива: {e}"),
            Self::Worker(e) => format!("Ошибка фоновой задачи: {e}"),
            Self::FileTooLarge { name, size, limit } => {
                format!("Файл слишком большой ({name}: {size} байт, лимит {limit} байт)")
            }
            Self::Cancelled => "Операция отменена".into(),
            Self::Missing(m) => format!("Требуемый объект недоступен: {m}"),
            Self::CapabilityDisabled(c) => format!("{c} недоступна: веб-функция отключена"),
            Self::Clipboard(e) => format!("Ошибка буфера обмена: {e}"),
            #[cfg(target_arch = "wasm32")]
            Self::Js(e) => format!("Операция JavaScript не выполнена: {e}"),
            Self::FilePickerTimedOut => "Выбор файла: время ожидания истекло".into(),
            Self::FilePickerFailed => "Не удалось выбрать файл".into(),
            Self::SaveCancelled => "Сохранение отменено".into(),
            Self::OpenUrlFailed { url } => format!("xdg-open не смог открыть {url}"),
            Self::OpenUrlTimedOut { url } => format!("xdg-open превысил время ожидания для {url}"),
            #[cfg(target_os = "android")]
            Self::JNI(e) => format!("Ошибка JNI: {e}"),
        }
    }
}
