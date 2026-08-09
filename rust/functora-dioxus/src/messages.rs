use crate::i18n::I18N;
use std::env::VarError;
use std::sync::mpsc::RecvError;

#[derive(Clone, Debug, PartialEq)]
pub enum Msg {
    Copied,
    Password,
    PasswordPlaceholder,
    Paste,
    Copy,
    Loading,
    ErrorTitle(String),
    EnvError(VarError),
    ChannelError(RecvError),
    NotJsonObject(String),
    #[cfg(target_os = "android")]
    JniError(String),
    ErrorTitleLabel,
    PasswordRequired,
    ClipboardWriteError(String),
    ClipboardReadError(String),
    CameraNotAvailable(String),
    CameraPermissionDenied(String),
    Back,
    Home,
    Stage(crate::progress::Stage),
}

impl I18N for Msg {
    fn render_eng(&self) -> String {
        match self {
            Self::Copied => "Copied!".into(),
            Self::Password => "Password".into(),
            Self::PasswordPlaceholder => "Enter password".into(),
            Self::Paste => "Paste".into(),
            Self::Copy => "Copy".into(),
            Self::Loading => "Loading...".into(),
            Self::ErrorTitle(e) => format!("Error: {e}"),
            Self::EnvError(e) => format!("Environment variable error: {e}"),
            Self::ChannelError(e) => format!("Channel receive error: {e}"),
            Self::NotJsonObject(e) => format!("Expected JSON object, got: {e}"),
            #[cfg(target_os = "android")]
            Self::JniError(e) => format!("JNI error: {e}"),
            Self::ErrorTitleLabel => "Error".into(),
            Self::PasswordRequired => "Password is required for encryption".into(),
            Self::ClipboardWriteError(e) => format!("Failed to copy to clipboard: {e}"),
            Self::ClipboardReadError(e) => format!("Failed to read from clipboard: {e}"),
            Self::CameraNotAvailable(e) => crate::Error::CameraNotAvailable(e.clone()).render_eng(),
            Self::CameraPermissionDenied(e) => crate::Error::CameraPermissionDenied(e.clone()).render_eng(),
            Self::Back => "Back".into(),
            Self::Home => "Home".into(),
            Self::Stage(s) => match s {
                crate::progress::Stage::Attach => "Attaching files...".into(),
                crate::progress::Stage::Zip => "Zipping files...".into(),
                crate::progress::Stage::Encrypt => "Encrypting...".into(),
                crate::progress::Stage::Decrypt => "Decrypting...".into(),
                crate::progress::Stage::Unzip => "Unzipping...".into(),
                crate::progress::Stage::Download => "Downloading...".into(),
            },
        }
    }

    fn render_spa(&self) -> String {
        match self {
            Self::Copied => "¡Copiado!".into(),
            Self::Password => "Contraseña".into(),
            Self::PasswordPlaceholder => "Ingresa contraseña".into(),
            Self::Paste => "Pegar".into(),
            Self::Copy => "Copiar".into(),
            Self::Loading => "Cargando...".into(),
            Self::ErrorTitle(e) => format!("Error: {e}"),
            Self::EnvError(e) => format!("Error de variable de entorno: {e}"),
            Self::ChannelError(e) => format!("Error de recepción en canal: {e}"),
            Self::NotJsonObject(e) => format!("Se esperaba un objeto JSON, se obtuvo: {e}"),
            #[cfg(target_os = "android")]
            Self::JniError(e) => format!("Error JNI: {e}"),
            Self::ErrorTitleLabel => "Error".into(),
            Self::PasswordRequired => "Se requiere contraseña para el cifrado".into(),
            Self::ClipboardWriteError(e) => format!("No se pudo copiar al portapapeles: {e}"),
            Self::ClipboardReadError(e) => format!("No se pudo leer del portapapeles: {e}"),
            Self::CameraNotAvailable(e) => crate::Error::CameraNotAvailable(e.clone()).render_spa(),
            Self::CameraPermissionDenied(e) => crate::Error::CameraPermissionDenied(e.clone()).render_spa(),
            Self::Back => "Atrás".into(),
            Self::Home => "Inicio".into(),
            Self::Stage(s) => match s {
                crate::progress::Stage::Attach => "Adjuntando archivos...".into(),
                crate::progress::Stage::Zip => "Comprimiendo archivos...".into(),
                crate::progress::Stage::Encrypt => "Cifrando...".into(),
                crate::progress::Stage::Decrypt => "Descifrando...".into(),
                crate::progress::Stage::Unzip => "Descomprimiendo...".into(),
                crate::progress::Stage::Download => "Descargando...".into(),
            },
        }
    }

    fn render_rus(&self) -> String {
        match self {
            Self::Copied => "Скопировано!".into(),
            Self::Password => "Пароль".into(),
            Self::PasswordPlaceholder => "Введите пароль".into(),
            Self::Paste => "Вставить".into(),
            Self::Copy => "Копировать".into(),
            Self::Loading => "Загрузка...".into(),
            Self::ErrorTitle(e) => format!("Ошибка: {e}"),
            Self::EnvError(e) => format!("Ошибка переменной окружения: {e}"),
            Self::ChannelError(e) => format!("Ошибка получения из канала: {e}"),
            Self::NotJsonObject(e) => format!("Ожидался JSON-объект, получено: {e}"),
            #[cfg(target_os = "android")]
            Self::JniError(e) => format!("Ошибка JNI: {e}"),
            Self::ErrorTitleLabel => "Ошибка".into(),
            Self::PasswordRequired => "Для шифрования требуется пароль".into(),
            Self::ClipboardWriteError(e) => format!("Не удалось скопировать в буфер обмена: {e}"),
            Self::ClipboardReadError(e) => format!("Не удалось прочитать из буфера обмена: {e}"),
            Self::CameraNotAvailable(e) => crate::Error::CameraNotAvailable(e.clone()).render_rus(),
            Self::CameraPermissionDenied(e) => crate::Error::CameraPermissionDenied(e.clone()).render_rus(),
            Self::Back => "Назад".into(),
            Self::Home => "Главная".into(),
            Self::Stage(s) => match s {
                crate::progress::Stage::Attach => "Прикрепление файлов...".into(),
                crate::progress::Stage::Zip => "Архивация файлов...".into(),
                crate::progress::Stage::Encrypt => "Шифрование...".into(),
                crate::progress::Stage::Decrypt => "Расшифровка...".into(),
                crate::progress::Stage::Unzip => "Распаковка...".into(),
                crate::progress::Stage::Download => "Скачивание...".into(),
            },
        }
    }
}
