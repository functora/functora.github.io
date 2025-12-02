use serde::{Deserialize, Serialize};

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Serialize,
    Deserialize,
)]
pub enum Language {
    English,
    Spanish,
    Russian,
}

impl Language {
    pub fn from_code(code: &str) -> Self {
        match code.split('-').next().unwrap_or("en") {
            "es" => Language::Spanish,
            "ru" => Language::Russian,
            _ => Language::English,
        }
    }

    pub fn code(&self) -> &'static str {
        match self {
            Language::English => "en",
            Language::Spanish => "es",
            Language::Russian => "ru",
        }
    }

    pub fn name(&self) -> &'static str {
        match self {
            Language::English => "English",
            Language::Spanish => "Español",
            Language::Russian => "Русский",
        }
    }
}

#[derive(Clone)]
pub struct Translations {
    pub app_title: &'static str,
    pub app_subtitle: &'static str,
    pub your_note: &'static str,
    pub note_placeholder: &'static str,
    pub encryption_options: &'static str,
    pub no_encryption: &'static str,
    pub password_encryption: &'static str,
    pub cipher: &'static str,
    pub password_placeholder: &'static str,
    pub generate_button: &'static str,
    pub share_title: &'static str,
    pub click_to_copy: &'static str,
    pub qr_code: &'static str,
    pub encrypted_note: &'static str,
    pub encrypted_note_desc: &'static str,
    pub decrypt_button: &'static str,
    pub your_note_title: &'static str,
    pub create_new_note: &'static str,
    pub error_title: &'static str,
    pub loading: &'static str,
    pub password_required: &'static str,
    pub encryption_failed: &'static str,
    pub qr_generation_failed: &'static str,
    pub url_generation_failed: &'static str,
    pub failed_to_parse_url: &'static str,
    pub no_note_in_url: &'static str,
    pub decryption_failed: &'static str,
    pub invalid_utf8: &'static str,
}

pub fn get_translations(lang: Language) -> Translations {
    match lang {
        Language::English => Translations {
            app_title: "Cryptonote",
            app_subtitle: "Create encrypted notes that live in URLs",
            your_note: "Your Note",
            note_placeholder: "Enter your note here...",
            encryption_options: "Encryption Options",
            no_encryption: "No encryption (plaintext)",
            password_encryption: "Password encryption",
            cipher: "Cipher",
            password_placeholder: "Enter password",
            generate_button: "Generate Shareable Link",
            share_title: "Share Your Note",
            click_to_copy: "Click to copy",
            qr_code: "QR Code",
            encrypted_note: "🔒 Encrypted Note",
            encrypted_note_desc: "This note is encrypted. Enter the password to decrypt it.",
            decrypt_button: "Decrypt",
            your_note_title: "📝 Your Note",
            create_new_note: "Create a new note",
            error_title: "Error",
            loading: "Loading note...",
            password_required: "Password is required for encryption",
            encryption_failed: "Encryption failed",
            qr_generation_failed: "QR generation failed",
            url_generation_failed: "URL generation failed",
            failed_to_parse_url: "Failed to parse URL",
            no_note_in_url: "No note found in URL",
            decryption_failed: "Decryption failed",
            invalid_utf8: "Decrypted data is not valid UTF-8",
        },
        Language::Spanish => Translations {
            app_title: "Cryptonote",
            app_subtitle: "Crea notas cifradas que viven en URLs",
            your_note: "Tu Nota",
            note_placeholder: "Escribe tu nota aquí...",
            encryption_options: "Opciones de Cifrado",
            no_encryption: "Sin cifrado (texto plano)",
            password_encryption: "Cifrado con contraseña",
            cipher: "Cifrado",
            password_placeholder: "Ingresa contraseña",
            generate_button: "Generar Enlace Compartible",
            share_title: "Comparte Tu Nota",
            click_to_copy: "Haz clic para copiar",
            qr_code: "Código QR",
            encrypted_note: "🔒 Nota Cifrada",
            encrypted_note_desc: "Esta nota está cifrada. Ingresa la contraseña para descifrarla.",
            decrypt_button: "Descifrar",
            your_note_title: "📝 Tu Nota",
            create_new_note: "Crear una nota nueva",
            error_title: "Error",
            loading: "Cargando nota...",
            password_required: "Se requiere contraseña para el cifrado",
            encryption_failed: "Falló el cifrado",
            qr_generation_failed: "Falló la generación del código QR",
            url_generation_failed: "Falló la generación de URL",
            failed_to_parse_url: "No se pudo analizar la URL",
            no_note_in_url: "No se encontró nota en la URL",
            decryption_failed: "Falló el descifrado",
            invalid_utf8: "Los datos descifrados no son UTF-8 válidos",
        },
        Language::Russian => Translations {
            app_title: "Cryptonote",
            app_subtitle: "Создавайте зашифрованные заметки в URL",
            your_note: "Ваша Заметка",
            note_placeholder: "Введите вашу заметку здесь...",
            encryption_options: "Параметры Шифрования",
            no_encryption: "Без шифрования (открытый текст)",
            password_encryption: "Шифрование паролем",
            cipher: "Шифр",
            password_placeholder: "Введите пароль",
            generate_button: "Создать Ссылку для Обмена",
            share_title: "Поделиться Заметкой",
            click_to_copy: "Нажмите, чтобы скопировать",
            qr_code: "QR-код",
            encrypted_note: "🔒 Зашифрованная Заметка",
            encrypted_note_desc: "Эта заметка зашифрована. Введите пароль для расшифровки.",
            decrypt_button: "Расшифровать",
            your_note_title: "📝 Ваша Заметка",
            create_new_note: "Создать новую заметку",
            error_title: "Ошибка",
            loading: "Загрузка заметки...",
            password_required: "Для шифрования требуется пароль",
            encryption_failed: "Ошибка шифрования",
            qr_generation_failed: "Ошибка генерации QR-кода",
            url_generation_failed: "Ошибка генерации URL",
            failed_to_parse_url: "Не удалось разобрать URL",
            no_note_in_url: "Заметка не найдена в URL",
            decryption_failed: "Ошибка расшифровки",
            invalid_utf8: "Расшифрованные данные не являются допустимым UTF-8",
        },
    }
}

pub fn detect_browser_language() -> Language {
    web_sys::window()
        .and_then(|w| w.navigator().language())
        .map(|lang| Language::from_code(&lang))
        .unwrap_or(Language::English)
}
