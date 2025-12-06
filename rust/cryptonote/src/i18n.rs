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
}

#[derive(Clone)]
pub struct Translations {
    pub note: &'static str,
    pub note_placeholder: &'static str,
    pub mode: &'static str,
    pub no_encryption: &'static str,
    pub password_encryption: &'static str,
    pub algorithm: &'static str,
    pub password: &'static str,
    pub password_placeholder: &'static str,
    pub generate_button: &'static str,
    pub share_title: &'static str,
    pub encrypted_note: &'static str,
    pub encrypted_note_desc: &'static str,
    pub decrypt_button: &'static str,
    pub your_note_title: &'static str,
    pub error_title: &'static str,
    pub loading: &'static str,
    pub create_new_note: &'static str,
    pub edit_note: &'static str,
    pub password_required: &'static str,
    pub no_note_in_url: &'static str,
    pub invalid_utf8: &'static str,
    pub cipher_error: &'static str,
    pub getrandom_error: &'static str,
    pub base64_error: &'static str,
    pub json_error: &'static str,
    pub qr_error: &'static str,
    pub encrypt_error: &'static str,
    pub decrypt_error: &'static str,
    pub url_error: &'static str,
    pub home: &'static str,
    pub copied: &'static str,
    pub copy_button: &'static str,
    pub license: &'static str,
    pub license_text: &'static str,
}

pub fn get_translations(lang: Language) -> Translations {
    match lang {
        Language::English => Translations {
            note: "Note",
            note_placeholder: "Enter your note here...",
            mode: "Mode",
            no_encryption: "No encryption (plaintext)",
            password_encryption: "Password encryption",
            algorithm: "Algo",
            password: "Password",
            password_placeholder: "Enter password",
            generate_button: "Share",
            share_title: "Share",
            encrypted_note: "Encrypted",
            encrypted_note_desc: "This note is encrypted. Enter the password to decrypt it.",
            decrypt_button: "Decrypt",
            your_note_title: "Note",
            error_title: "Error",
            loading: "Loading...",
            create_new_note: "Reset",
            edit_note: "Edit",
            password_required: "Password is required for encryption",
            no_note_in_url: "No note found in URL",
            invalid_utf8: "Decrypted data is not valid UTF-8",
            cipher_error: "Cipher initialization error",
            getrandom_error: "Random number generation error",
            base64_error: "Base64 decoding error",
            json_error: "JSON parsing error",
            qr_error: "QR code generation error",
            encrypt_error: "Encryption failed",
            decrypt_error: "Decryption failed",
            url_error: "Invalid URL format",
            home: "Home",
            copied: "Copied!",
            copy_button: "Copy",
            license: "License",
            license_text: r#"Copyright (c) 2025 Functora

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE."#,
        },
        Language::Spanish => Translations {
            note: "Nota",
            note_placeholder: "Escribe tu nota aquí...",
            mode: "Modo",
            no_encryption: "Sin cifrado (texto plano)",
            password_encryption: "Cifrado con contraseña",
            algorithm: "Algo",
            password: "Clave",
            password_placeholder: "Ingresa contraseña",
            generate_button: "Compartir",
            share_title: "Compartir",
            encrypted_note: "Cifrado",
            encrypted_note_desc: "Esta nota está cifrada. Ingresa la contraseña para descifrarla.",
            decrypt_button: "Descifrar",
            your_note_title: "Nota",
            error_title: "Error",
            loading: "Cargando...",
            create_new_note: "Reiniciar",
            edit_note: "Editar",
            password_required: "Se requiere contraseña para el cifrado",
            no_note_in_url: "No se encontró nota en la URL",
            invalid_utf8: "Los datos descifrados no son UTF-8 válidos",
            cipher_error: "Error de inicialización de cifrado",
            getrandom_error: "Error de generación de números aleatorios",
            base64_error: "Error de decodificación Base64",
            json_error: "Error de análisis JSON",
            qr_error: "Error de generación de código QR",
            encrypt_error: "Falló el cifrado",
            decrypt_error: "Falló el descifrado",
            url_error: "Formato de URL inválido",
            home: "Inicio",
            copied: "¡Copiado!",
            copy_button: "Copiar",
            license: "Licencia",
            license_text: r#"Copyright (c) 2025 Functora

Por la presente se concede permiso, libre de cargos, a cualquier persona que haya obtenido una copia de este software y archivos de documentación asociados (el "Software"), para utilizar el Software sin restricción, incluyendo sin limitación los derechos a usar, copiar, modificar, fusionar, publicar, distribuir, sublicenciar y/o vender copias del Software, y a permitir a las personas a las que se les proporcione el Software que hagan lo mismo, sujeto a las siguientes condiciones:

El aviso de copyright anterior y este aviso de permiso deberán incluirse en todas las copias o partes sustanciales del Software.

EL SOFTWARE SE PROPORCIONA "TAL CUAL", SIN GARANTÍA DE NINGÚN TIPO, EXPRESA O IMPLÍCITA, INCLUYENDO PERO NO LIMITADO A LAS GARANTÍAS DE COMERCIABILIDAD, IDONEIDAD PARA UN FIN PARTICULAR Y NO INFRACCIÓN. EN NINGÚN CASO LOS AUTORES O TITULARES DEL COPYRIGHT SERÁN RESPONSABLES DE NINGUNA RECLAMACIÓN, DAÑOS O OTRAS RESPONSABILIDADES, YA SEA EN UNA ACCIÓN DE CONTRATO, AGRAVIO O DE OTRO TIPO, QUE SURJA DE, O EN RELACIÓN CON EL SOFTWARE O EL USO U OTROS TRATOS EN EL SOFTWARE."#,
        },
        Language::Russian => Translations {
            note: "Заметка",
            note_placeholder: "Введите вашу заметку здесь...",
            mode: "Режим",
            no_encryption: "Без шифрования (открытый текст)",
            password_encryption: "Шифрование паролем",
            algorithm: "Алго",
            password: "Пароль",
            password_placeholder: "Введите пароль",
            generate_button: "Поделиться",
            share_title: "Поделиться",
            encrypted_note: "Шифр",
            encrypted_note_desc: "Эта заметка зашифрована. Введите пароль для расшифровки.",
            decrypt_button: "Расшифровать",
            your_note_title: "Заметка",
            error_title: "Ошибка",
            loading: "Загрузка...",
            create_new_note: "Сброс",
            edit_note: "Правка",
            password_required: "Для шифрования требуется пароль",
            no_note_in_url: "Заметка не найдена в URL",
            invalid_utf8: "Расшифрованные данные не являются допустимым UTF-8",
            cipher_error: "Ошибка инициализации шифра",
            getrandom_error: "Ошибка генерации случайных чисел",
            base64_error: "Ошибка декодирования Base64",
            json_error: "Ошибка разбора JSON",
            qr_error: "Ошибка генерации QR-кода",
            encrypt_error: "Ошибка шифрования",
            decrypt_error: "Ошибка расшифровки",
            url_error: "Неверный формат URL",
            home: "Главная",
            copied: "Скопировано!",
            copy_button: "Копировать",
            license: "Лицензия",
            license_text: r#"Copyright (c) 2025 Functora

Настоящим предоставляется бесплатное разрешение любому лицу, получившему копию данного программного обеспечения и сопутствующих файлов документации (далее — «Программное обеспечение»), использовать Программное обеспечение без ограничений, включая неограниченное право использовать, копировать, изменять, объединять, публиковать, распространять, сублицензировать и/или продавать копии Программного обеспечения, а также разрешать лицам, которым предоставлено Программное обеспечение, делать то же самое, при соблюдении следующих условий:

Указанное выше уведомление об авторских правах и данное уведомление о разрешении должны быть включены во все копии или существенные части Программного обеспечения.

ПРОГРАММНОЕ ОБЕСПЕЧЕНИЕ ПРЕДОСТАВЛЯЕТСЯ «КАК ЕСТЬ», БЕЗ КАКИХ-ЛИБО ГАРАНТИЙ, ЯВНО ВЫРАЖЕННЫХ ИЛИ ПОДРАЗУМЕВАЕМЫХ, ВКЛЮЧАЯ, НО НЕ ОГРАНИЧИВАЯСЬ ГАРАНТИЯМИ ТОВАРНОГО СОСТОЯНИЯ, ПРИГОДНОСТИ ДЛЯ КОНКРЕТНЫХ ЦЕЛЕЙ И ОТСУТСТВИЯ НАРУШЕНИЙ АВТОРСКИХ ПРАВ. НИ ПРИ КАКИХ ОБСТОЯТЕЛЬСТВАХ АВТОРЫ ИЛИ ПРАВООБЛАДАТЕЛИ НЕ НЕСУТ ОТВЕТСТВЕННОСТИ ПО ЛЮБЫМ ПРЕТЕНЗИЯМ, ЗА УБЫТКИ ИЛИ ДРУГИЕ ТРЕБОВАНИЯ, ВЫТЕКАЮЩИЕ ИЗ ДОГОВОРА, ДЕЛИКТА ИЛИ ИНЫХ ОБСТОЯТЕЛЬСТВ, СВЯЗАННЫЕ С ПРОГРАММНЫМ ОБЕСПЕЧЕНИЕМ, ЕГО ИСПОЛЬЗОВАНИЕМ ИЛИ ДРУГИМИ ДЕЙСТВИЯМИ С ПРОГРАММНЫМ ОБЕСПЕЧЕНИЕМ."#,
        },
    }
}

pub fn detect_browser_language() -> Language {
    web_sys::window()
        .and_then(|w| w.navigator().language())
        .map(|lang| Language::from_code(&lang))
        .unwrap_or(Language::English)
}
