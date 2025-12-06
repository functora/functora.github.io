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
    pub license_text: &'static str,
    pub privacy_text: &'static str,
    pub copyright: &'static str,
    pub all_rights_reserved: &'static str,
    pub by_continuing: &'static str,
    pub you_agree: &'static str,
    pub terms_of_service: &'static str,
    pub privacy_policy_and: &'static str,
    pub version_label: &'static str,
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
            license_text: r#"Copyright (c) 2025 Functora

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE."#,
            privacy_text: r#"Privacy Policy

This privacy policy applies to the app (hereby referred to as “Application”) for mobile devices that was created by Functora (hereby referred to as “Service Provider”) as a Free service. This service is intended for use “AS IS”.

What information does the Application obtain and how is it used?

The Application does not obtain any information when you download and use it. Registration is not required to use the Application.

Does the Application collect precise real time location information of the device?

This Application does not collect precise information about the location of your mobile device.

Do third parties see and/or have access to information obtained by the Application?

Since the Application does not collect any information, no data is shared with third parties.

What are my opt-out rights?

You can stop all collection of information by the Application easily by uninstalling it. You may use the standard uninstall processes as may be available as part of your mobile device or via the mobile application marketplace or network.

Children

The Application is not used to knowingly solicit data from or market to children under the age of 13.

The Service Provider does not knowingly collect personally identifiable information from children. The Service Provider encourages all children to never submit any personally identifiable information through the Application and/or Services. The Service Provider encourage parents and legal guardians to monitor their children’s Internet usage and to help enforce this Policy by instructing their children never to provide personally identifiable information through the Application and/or Services without their permission. If you have reason to believe that a child has provided personally identifiable information to the Service Provider through the Application and/or Services, please contact the Service Provider (functora@gmail.com) so that they will be able to take the necessary actions. You must also be at least 16 years of age to consent to the processing of your personally identifiable information in your country (in some countries we may allow your parent or guardian to do so on your behalf).

Security

The Service Provider is concerned about safeguarding the confidentiality of your information. However, since the Application does not collect any information, there is no risk of your data being accessed by unauthorized individuals.

Changes

This Privacy Policy may be updated from time to time for any reason. The Service Provider will notify you of any changes to their Privacy Policy by updating this page with the new Privacy Policy. You are advised to consult this Privacy Policy regularly for any changes, as continued use is deemed approval of all changes.

This privacy policy is effective as of 2025-12-06

Your Consent

By using the Application, you are consenting to the processing of your information as set forth in this Privacy Policy now and as amended by the Service Provider.

Contact Us

If you have any questions regarding privacy while using the Application, or have questions about the practices, please contact the Service Provider via email at functora@gmail.com."#,
            copyright: "©",
            all_rights_reserved: "All rights reserved.",
            by_continuing: "By continuing to use this software, you agree to the",
            you_agree: "and",
            terms_of_service: "Terms of Service",
            privacy_policy_and: "Privacy Policy",
            version_label: "Version",
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
            license_text: r#"Copyright (c) 2025 Functora

Por la presente se concede permiso, libre de cargos, a cualquier persona que haya obtenido una copia de este software y archivos de documentación asociados (el "Software"), para utilizar el Software sin restricción, incluyendo sin limitación los derechos a usar, copiar, modificar, fusionar, publicar, distribuir, sublicenciar y/o vender copias del Software, y a permitir a las personas a las que se les proporcione el Software que hagan lo mismo, sujeto a las siguientes condiciones:

El aviso de copyright anterior y este aviso de permiso deberán incluirse en todas las copias o partes sustanciales del Software.

EL SOFTWARE SE PROPORCIONA "TAL CUAL", SIN GARANTÍA DE NINGÚN TIPO, EXPRESA O IMPLÍCITA, INCLUYENDO PERO NO LIMITADO A LAS GARANTÍAS DE COMERCIABILIDAD, IDONEIDAD PARA UN FIN PARTICULAR Y NO INFRACCIÓN. EN NINGÚN CASO LOS AUTORES O TITULARES DEL COPYRIGHT SERÁN RESPONSABLES DE NINGUNA RECLAMACIÓN, DAÑOS O OTRAS RESPONSABILIDADES, YA SEA EN UNA ACCIÓN DE CONTRATO, AGRAVIO O DE OTRO TIPO, QUE SURJA DE, O EN RELACIÓN CON EL SOFTWARE O EL USO U OTROS TRATOS EN EL SOFTWARE."#,
            privacy_text: r#"Política de Privacidad

Esta política de privacidad se aplica a la aplicación (en adelante, "Aplicación") para dispositivos móviles creada por Functora (en adelante, "Proveedor de Servicios") como un servicio gratuito. Este servicio está destinado a su uso "TAL CUAL".

¿Qué información obtiene la Aplicación y cómo se utiliza?

La Aplicación no obtiene ninguna información cuando la descargas y la usas. No se requiere registro para usar la Aplicación.

¿La Aplicación recopila información precisa de ubicación en tiempo real del dispositivo?

Esta Aplicación no recopila información precisa sobre la ubicación de tu dispositivo móvil.

¿Tienen terceros acceso a la información obtenida por la Aplicación?

Dado que la Aplicación no recopila ninguna información, no se comparten datos con terceros.

¿Cuáles son mis derechos de exclusión voluntaria?

Puedes detener toda la recopilación de información por parte de la Aplicación fácilmente desinstalándola. Puedes utilizar los procesos de desinstalación estándar disponibles como parte de tu dispositivo móvil o a través del mercado o red de aplicaciones móviles.

Niños

La Aplicación no se utiliza para solicitar datos a sabiendas de niños menores de 13 años ni para comercializar con ellos.

El Proveedor de Servicios no recopila a sabiendas información de identificación personal de niños. El Proveedor de Servicios alienta a todos los niños a que nunca envíen ninguna información de identificación personal a través de la Aplicación y/o los Servicios. El Proveedor de Servicios alienta a los padres y tutores legales a monitorear el uso de Internet de sus hijos y a ayudar a hacer cumplir esta Política instruyendo a sus hijos para que nunca proporcionen información de identificación personal a través de la Aplicación y/o los Servicios sin su permiso. Si tienes motivos para creer que un niño ha proporcionado información de identificación personal al Proveedor de Servicios a través de la Aplicación y/o los Servicios, comunícate con el Proveedor de Servicios (functora@gmail.com) para que puedan tomar las medidas necesarias. También debes tener al menos 16 años de edad para dar tu consentimiento al procesamiento de tu información de identificación personal en tu país (en algunos países podemos permitir que tu padre o tutor lo haga en tu nombre).

Seguridad

El Proveedor de Servicios se preocupa por salvaguardar la confidencialidad de tu información. Sin embargo, dado que la Aplicación no recopila ninguna información, no existe riesgo de que personas no autorizadas accedan a tus datos.

Cambios

Esta Política de Privacidad puede actualizarse de vez en cuando por cualquier motivo. El Proveedor de Servicios te notificará de cualquier cambio en su Política de Privacidad actualizando esta página con la nueva Política de Privacidad. Se te aconseja consultar esta Política de Privacidad regularmente para ver si hay cambios, ya que el uso continuado se considera aprobación de todos los cambios.

Esta política de privacidad es efectiva a partir del 2025-12-06

Tu Consentimiento

Al usar la Aplicación, das tu consentimiento al procesamiento de tu información según lo establecido en esta Política de Privacidad ahora y según sea modificada por el Proveedor de Servicios.

Contáctanos

Si tienes alguna pregunta sobre la privacidad al usar la Aplicación, o tienes preguntas sobre las prácticas, comunícate con el Proveedor de Servicios por correo electrónico a functora@gmail.com."#,
            copyright: "©",
            all_rights_reserved: "Todos los derechos reservados.",
            by_continuing: "Al continuar usando este software, aceptas los",
            you_agree: "y la",
            terms_of_service: "Términos de Servicio",
            privacy_policy_and: "Política de Privacidad",
            version_label: "Versión",
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
            license_text: r#"Copyright (c) 2025 Functora

Настоящим предоставляется бесплатное разрешение любому лицу, получившему копию данного программного обеспечения и сопутствующих файлов документации (далее — «Программное обеспечение»), использовать Программное обеспечение без ограничений, включая неограниченное право использовать, копировать, изменять, объединять, публиковать, распространять, сублицензировать и/или продавать копии Программного обеспечения, а также разрешать лицам, которым предоставлено Программное обеспечение, делать то же самое, при соблюдении следующих условий:

Указанное выше уведомление об авторских правах и данное уведомление о разрешении должны быть включены во все копии или существенные части Программного обеспечения.

ПРОГРАММНОЕ ОБЕСПЕЧЕНИЕ ПРЕДОСТАВЛЯЕТСЯ «КАК ЕСТЬ», БЕЗ КАКИХ-ЛИБО ГАРАНТИЙ, ЯВНО ВЫРАЖЕННЫХ ИЛИ ПОДРАЗУМЕВАЕМЫХ, ВКЛЮЧАЯ, НО НЕ ОГРАНИЧИВАЯСЬ ГАРАНТИЯМИ ТОВАРНОГО СОСТОЯНИЯ, ПРИГОДНОСТИ ДЛЯ КОНКРЕТНЫХ ЦЕЛЕЙ И ОТСУТСТВИЯ НАРУШЕНИЙ АВТОРСКИХ ПРАВ. НИ ПРИ КАКИХ ОБСТОЯТЕЛЬСТВАХ АВТОРЫ ИЛИ ПРАВООБЛАДАТЕЛИ НЕ НЕСУТ ОТВЕТСТВЕННОСТИ ПО ЛЮБЫМ ПРЕТЕНЗИЯМ, ЗА УБЫТКИ ИЛИ ДРУГИЕ ТРЕБОВАНИЯ, ВЫТЕКАЮЩИЕ ИЗ ДОГОВОРА, ДЕЛИКТА ИЛИ ИНЫХ ОБСТОЯТЕЛЬСТВ, СВЯЗАННЫЕ С ПРОГРАММНЫМ ОБЕСПЕЧЕНИЕМ, ЕГО ИСПОЛЬЗОВАНИЕМ ИЛИ ДРУГИМИ ДЕЙСТВИЯМИ С ПРОГРАММНЫМ ОБЕСПЕЧЕНИЕМ."#,
            privacy_text: r#"Политика конфиденциальности

Эта политика конфиденциальности применяется к приложению (далее именуемому «Приложение») для мобильных устройств, созданному Functora (далее именуемому «Поставщик услуг») в качестве бесплатной услуги. Эта услуга предназначена для использования «КАК ЕСТЬ».

Какую информацию получает Приложение и как она используется?

Приложение не получает никакой информации, когда вы загружаете и используете его. Регистрация не требуется для использования Приложения.

Собирает ли Приложение точную информацию о местоположении устройства в реальном времени?

Это Приложение не собирает точную информацию о местоположении вашего мобильного устройства.

Видят ли третьи стороны и/или имеют ли доступ к информации, полученной Приложением?

Поскольку Приложение не собирает никакой информации, никакие данные не передаются третьим лицам.

Каковы мои права на отказ?

Вы можете легко прекратить сбор информации Приложением, удалив его. Вы можете использовать стандартные процессы удаления, доступные как часть вашего мобильного устройства или через магазин мобильных приложений или сеть.

Дети

Приложение не используется для намеренного сбора данных или маркетинга среди детей младше 13 лет.

Поставщик услуг не собирает намеренно личную информацию от детей. Поставщик услуг призывает всех детей никогда не отправлять какую-либо личную информацию через Приложение и/или Услуги. Поставщик услуг призывает родителей и законных опекунов контролировать использование Интернета их детьми и помогать обеспечивать соблюдение этой Политики, инструктируя своих детей никогда не предоставлять личную информацию через Приложение и/или Услуги без их разрешения. Если у вас есть основания полагать, что ребенок предоставил личную информацию Поставщику услуг через Приложение и/или Услуги, свяжитесь с Поставщиком услуг (functora@gmail.com), чтобы они могли принять необходимые меры. Вам также должно быть не менее 16 лет, чтобы дать согласие на обработку вашей личной информации в вашей стране (в некоторых странах мы можем разрешить вашему родителю или опекуну сделать это от вашего имени).

Безопасность

Поставщик услуг заботится о защите конфиденциальности вашей информации. Однако, поскольку Приложение не собирает никакой информации, нет риска доступа к вашим данным посторонних лиц.

Изменения

Эта Политика конфиденциальности может время от времени обновляться по любой причине. Поставщик услуг уведомит вас о любых изменениях в своей Политике конфиденциальности, обновив эту страницу новой Политикой конфиденциальности. Вам рекомендуется регулярно просматривать эту Политику конфиденциальности на предмет изменений, так как продолжение использования считается одобрением всех изменений.

Эта политика конфиденциальности вступает в силу с 2025-12-06

Ваше согласие

Используя Приложение, вы даете согласие на обработку вашей информации, как изложено в этой Политике конфиденциальности сейчас и с изменениями, внесенными Поставщиком услуг.

Свяжитесь с нами

Если у вас есть какие-либо вопросы относительно конфиденциальности при использовании Приложения или вопросы о практике, свяжитесь с Поставщиком услуг по электронной почте functora@gmail.com."#,
            copyright: "©",
            all_rights_reserved: "Все права защищены.",
            by_continuing: "Продолжая использовать это программное обеспечение, вы соглашаетесь с",
            you_agree: "и",
            terms_of_service: "Условиями обслуживания",
            privacy_policy_and: "Политикой конфиденциальности",
            version_label: "Версия",
        },
    }
}

pub fn detect_browser_language() -> Language {
    web_sys::window()
        .and_then(|w| w.navigator().language())
        .map(|lang| Language::from_code(&lang))
        .unwrap_or(Language::English)
}
