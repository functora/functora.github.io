use crate::i18n::I18N;
use std::env::VarError;
use std::sync::mpsc::RecvError;

#[derive(Clone, PartialEq)]
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
}

impl I18N for Msg {
    fn render_eng(&self) -> String {
        match self {
            Self::Copied => "Copied!",
            Self::Password => "Password",
            Self::PasswordPlaceholder => "Enter password",
            Self::Paste => "Paste",
            Self::Copy => "Copy",
            Self::Loading => "Loading...",
            Self::ErrorTitle(e) => return format!("Error: {e}"),
            Self::EnvError(e) => return format!("Error: {e}"),
            Self::ChannelError(e) => return format!("Error: {e}"),
            Self::NotJsonObject(e) => return format!("Error: {e}"),
            #[cfg(target_os = "android")]
            Self::JniError(e) => return format!("Error: {e}"),
            Self::ErrorTitleLabel => "Error",
            Self::PasswordRequired => "Password is required for encryption",
            Self::ClipboardWriteError(e) => return format!("Failed to copy to clipboard: {e}"),
            Self::ClipboardReadError(e) => return format!("Failed to read from clipboard: {e}"),
            Self::CameraNotAvailable(e) => return format!("Camera is not available: {e}"),
            Self::CameraPermissionDenied(e) => return format!("Camera permission was denied: {e}"),
            Self::Back => "Back",
            Self::Home => "Home",
        }
        .to_string()
    }

    fn render_spa(&self) -> String {
        match self {
            Self::Copied => "¡Copiado!",
            Self::Password => "Contraseña",
            Self::PasswordPlaceholder => "Ingresa contraseña",
            Self::Paste => "Pegar",
            Self::Copy => "Copiar",
            Self::Loading => "Cargando...",
            Self::ErrorTitle(e) => return format!("Error: {e}"),
            Self::EnvError(e) => return format!("Error: {e}"),
            Self::ChannelError(e) => return format!("Error: {e}"),
            Self::NotJsonObject(e) => return format!("Error: {e}"),
            #[cfg(target_os = "android")]
            Self::JniError(e) => return format!("Error: {e}"),
            Self::ErrorTitleLabel => "Error",
            Self::PasswordRequired => "Se requiere contraseña para el cifrado",
            Self::ClipboardWriteError(e) => return format!("No se pudo copiar al portapapeles: {e}"),
            Self::ClipboardReadError(e) => return format!("No se pudo leer del portapapeles: {e}"),
            Self::CameraNotAvailable(e) => return format!("La cámara no está disponible: {e}"),
            Self::CameraPermissionDenied(e) => return format!("Permiso de cámara denegado: {e}"),
            Self::Back => "Atrás",
            Self::Home => "Inicio",
        }
        .to_string()
    }

    fn render_rus(&self) -> String {
        match self {
            Self::Copied => "Скопировано!",
            Self::Password => "Пароль",
            Self::PasswordPlaceholder => "Введите пароль",
            Self::Paste => "Вставить",
            Self::Copy => "Копировать",
            Self::Loading => "Загрузка...",
            Self::ErrorTitle(e) => return format!("Ошибка: {e}"),
            Self::EnvError(e) => return format!("Ошибка: {e}"),
            Self::ChannelError(e) => return format!("Ошибка: {e}"),
            Self::NotJsonObject(e) => return format!("Ошибка: {e}"),
            #[cfg(target_os = "android")]
            Self::JniError(e) => return format!("Ошибка: {e}"),
            Self::ErrorTitleLabel => "Ошибка",
            Self::PasswordRequired => "Для шифрования требуется пароль",
            Self::ClipboardWriteError(e) => return format!("Не удалось скопировать в буфер обмена: {e}"),
            Self::ClipboardReadError(e) => return format!("Не удалось прочитать из буфера обмена: {e}"),
            Self::CameraNotAvailable(e) => return format!("Камера недоступна: {e}"),
            Self::CameraPermissionDenied(e) => return format!("Разрешение на камеру отклонено: {e}"),
            Self::Back => "Назад",
            Self::Home => "Главная",
        }
        .to_string()
    }
}
