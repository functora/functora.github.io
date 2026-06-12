use crate::i18n::I18N;

#[derive(Clone, PartialEq)]
pub enum Msg {
    Copied,
    Password,
    PasswordPlaceholder,
    PasteButton,
    CopyButton,
    Loading,
    ErrorTitle,
    PasswordRequired,
    ClipboardWriteError,
    ClipboardReadError,
    Back,
    Home,
}

impl I18N for Msg {
    fn render_eng(&self) -> String {
        match self {
            Self::Copied => "Copied!",
            Self::Password => "Password",
            Self::PasswordPlaceholder => "Enter password",
            Self::PasteButton => "Paste",
            Self::CopyButton => "Copy",
            Self::Loading => "Loading...",
            Self::ErrorTitle => "Error",
            Self::PasswordRequired => "Password is required for encryption",
            Self::ClipboardWriteError => "Failed to copy to clipboard",
            Self::ClipboardReadError => "Failed to read from clipboard",
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
            Self::PasteButton => "Pegar",
            Self::CopyButton => "Copiar",
            Self::Loading => "Cargando...",
            Self::ErrorTitle => "Error",
            Self::PasswordRequired => "Se requiere contraseña para el cifrado",
            Self::ClipboardWriteError => "No se pudo copiar al portapapeles",
            Self::ClipboardReadError => "No se pudo leer del portapapeles",
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
            Self::PasteButton => "Вставить",
            Self::CopyButton => "Копировать",
            Self::Loading => "Загрузка...",
            Self::ErrorTitle => "Ошибка",
            Self::PasswordRequired => "Для шифрования требуется пароль",
            Self::ClipboardWriteError => "Не удалось скопировать в буфер обмена",
            Self::ClipboardReadError => "Не удалось прочитать из буфера обмена",
            Self::Back => "Назад",
            Self::Home => "Главная",
        }
        .to_string()
    }
}
