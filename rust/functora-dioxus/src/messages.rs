use crate::i18n::I18N;

macro_rules! msg {
    ($name:ident, $eng:expr, $spa:expr, $rus:expr) => {
        #[derive(Clone, PartialEq)]
        pub struct $name;

        impl I18N for $name {
            fn render_eng(&self) -> String {
                $eng.to_string()
            }
            fn render_spa(&self) -> String {
                $spa.to_string()
            }
            fn render_rus(&self) -> String {
                $rus.to_string()
            }
        }
    };
}

msg!(MsgCopied, "Copied!", "¡Copiado!", "Скопировано!");
msg!(MsgPassword, "Password", "Contraseña", "Пароль");
msg!(
    MsgPasswordPlaceholder,
    "Enter password",
    "Ingresa contraseña",
    "Введите пароль"
);
msg!(MsgPasteButton, "Paste", "Pegar", "Вставить");
msg!(MsgCopyButton, "Copy", "Copiar", "Копировать");
msg!(MsgLoading, "Loading...", "Cargando...", "Загрузка...");
msg!(MsgErrorTitle, "Error", "Error", "Ошибка");
msg!(
    MsgPasswordRequired,
    "Password is required for encryption",
    "Se requiere contraseña para el cifrado",
    "Для шифрования требуется пароль"
);
msg!(
    MsgClipboardWriteError,
    "Failed to copy to clipboard",
    "No se pudo copiar al portapapeles",
    "Не удалось скопировать в буфер обмена"
);
msg!(
    MsgClipboardReadError,
    "Failed to read from clipboard",
    "No se pudo leer del portapapeles",
    "Не удалось прочитать из буфера обмена"
);
msg!(MsgBack, "Back", "Atrás", "Назад");
msg!(MsgHome, "Home", "Inicio", "Главная");
