use functora_dioxus::i18n::I18N;

pub use functora_dioxus::{
    MsgBack, MsgClipboardReadError, MsgClipboardWriteError,
    MsgCopied, MsgCopyButton, MsgErrorTitle, MsgHome,
    MsgLoading, MsgPassword, MsgPasswordPlaceholder,
    MsgPasswordRequired, MsgPasteButton,
};

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

msg!(MsgNote, "Note", "Nota", "Заметка");
msg!(MsgNotePlaceholder, "Enter your note here (Markdown/HTML supported)...", "Escribe tu nota aquí (Markdown/HTML soportado)...", "Введите вашу заметку здесь (Markdown/HTML поддерживается)...");
msg!(MsgMode, "Mode", "Modo", "Режим");
msg!(
    MsgNoEncryption,
    "No encryption (plaintext)",
    "Sin cifrado (texto plano)",
    "Без шифрования (открытый текст)"
);
msg!(
    MsgPasswordEncryption,
    "Password encryption",
    "Cifrado con contraseña",
    "Шифрование паролем"
);
msg!(MsgCipher, "Cipher", "Cifrado", "Шифр");
msg!(MsgGenerateButton, "Share", "Compartir", "Поделиться");
msg!(MsgShareTitle, "Share", "Compartir", "Поделиться");
msg!(MsgEncryptedNote, "Encrypted", "Cifrado", "Шифр");
msg!(MsgEncryptedNoteDesc, "This note is encrypted. Enter the password to decrypt it.", "Esta nota está cifrada. Ingresa la contraseña para descifrarla.", "Эта заметка зашифрована. Введите пароль для расшифровки.");
msg!(
    MsgDecryptButton,
    "Decrypt",
    "Descifrar",
    "Расшифровать"
);
msg!(MsgYourNoteTitle, "Note", "Nota", "Заметка");
msg!(MsgCreateNewNote, "Reset", "Reiniciar", "Сброс");
msg!(MsgEditNote, "Edit", "Editar", "Правка");
msg!(MsgViewButton, "View", "Ver", "Смотреть");
msg!(MsgCopyright, "©", "©", "©");
msg!(
    MsgAllRightsReserved,
    "All rights reserved.",
    "Todos los derechos reservados.",
    "Все права защищены."
);
msg!(MsgByContinuing, "By continuing to use this software, you agree to the", "Al continuar usando este software, aceptas los", "Продолжая использовать это программное обеспечение, вы соглашаетесь с");
msg!(MsgYouAgree, "and", "y la", "и");
msg!(
    MsgTermsOfService,
    "Terms of Service",
    "Términos de Servicio",
    "Условиями обслуживания"
);
msg!(
    MsgTermsOfServiceTitle,
    "Terms of Service",
    "Términos de Servicio",
    "Условия обслуживания"
);
msg!(
    MsgPrivacyPolicyAnd,
    "Privacy Policy",
    "Política de Privacidad",
    "Политикой конфиденциальности"
);
msg!(
    MsgPrivacyPolicyTitle,
    "Privacy Policy",
    "Política de Privacidad",
    "Политика конфиденциальности"
);
msg!(MsgVersionLabel, "Version", "Versión", "Версия");
msg!(MsgAboutTitle, "About", "Referencia", "Справка");
msg!(MsgAboutAndroidBeta1, "The Android app is in closed beta. To install it, join the", "La aplicación de Android está en beta cerrada. Para instalarla, únase al grupo de", "Приложение Android в закрытом бета-тестировании. Чтобы установить его, вступите в группу");
msg!(
    MsgAboutAndroidBetaLink1,
    "closed beta",
    "beta cerrada",
    "бета-тестирования"
);
msg!(
    MsgAboutAndroidBeta2,
    "group and then install the app from",
    "y luego instale la aplicación desde",
    ", затем установите приложение из"
);
msg!(
    MsgAboutAndroidBetaLink2,
    "Google Play",
    "Google Play",
    "Google Play"
);
msg!(
    MsgAboutAndroidBeta3,
    ", or download the",
    ", o descargue el",
    " или скачайте"
);
msg!(
    MsgAboutAndroidBetaLink3,
    "APK file",
    "archivo APK",
    "APK-файл"
);
msg!(
    MsgAboutAndroidBeta4,
    "directly.",
    "directamente.",
    "напрямую."
);
msg!(
    MsgJoinTestingButton,
    "Join testing",
    "Unirse a prueba",
    "Вступить в бета-тест"
);
msg!(
    MsgGooglePlayButton,
    "Google Play",
    "Google Play",
    "Google Play"
);
msg!(
    MsgDownloadApkButton,
    "Download APK",
    "Descargar APK",
    "Скачать APK"
);
msg!(
    MsgSourceCodeButton,
    "Source code",
    "Código fuente",
    "Исходный код"
);
msg!(MsgAuthorButton, "Author", "Autor", "Автор");
msg!(MsgDonateButton, "Donate", "Donar", "Пожертвовать");
msg!(MsgOpenUrlLabel, "URL", "URL", "URL");
msg!(
    MsgOpenUrlPlaceholder,
    "Paste shared note URL here...",
    "Pega la URL de la nota compartida aquí...",
    "Вставьте URL заметки здесь..."
);
msg!(MsgOpenButton, "Open", "Abrir", "Открыть");
msg!(
    MsgNoNoteParam,
    "URL does not contain a note parameter",
    "La URL no contiene un parámetro de nota",
    "URL не содержит параметр заметки"
);
msg!(MsgDonateTitle, "Donate", "Donar", "Пожертвовать");
msg!(
    MsgDonateGreeting,
    "Hello, User!",
    "¡Hola, Usuario!",
    "Здравствуйте, пользователь!"
);
msg!(
    MsgDonateLink,
    "Donate",
    "Donar",
    "сделайте пожертвование"
);
msg!(MsgPlease, "Please", "Por favor", "Пожалуйста");
msg!(MsgActionLabel, "Action", "Acción", "Действие");
msg!(
    MsgActionCreate,
    "Create new note",
    "Crear nueva nota",
    "Создать новую заметку"
);
msg!(
    MsgActionOpen,
    "Open note URL",
    "Abrir URL de nota",
    "Открыть URL заметки"
);
msg!(
    MsgActionScan,
    "Scan note QR",
    "Escanear QR de nota",
    "Сканировать QR заметки"
);
msg!(MsgTheme, "Theme", "Tema", "Тема");
msg!(MsgScanQrButton, "Scan", "Escanear", "Сканировать");
msg!(
    MsgQrScannerTitle,
    "Scan QR Code",
    "Escanear código QR",
    "Сканирование QR-кода"
);
msg!(
    MsgQrCameraNotAvailable,
    "Camera is not available on this device",
    "La cámara no está disponible en este dispositivo",
    "Камера недоступна на этом устройстве"
);
msg!(
    MsgQrPermissionDenied,
    "Camera permission was denied",
    "Se denegó el permiso de cámara",
    "Разрешение на камеру отклонено"
);

msg!(
    MsgCipherError,
    "Cipher initialization error",
    "Error de inicialización de cifrado",
    "Ошибка инициализации шифра"
);
msg!(
    MsgGetrandomError,
    "Random number generation error",
    "Error de generación de números aleatorios",
    "Ошибка генерации случайных чисел"
);
msg!(
    MsgBase64Error,
    "Base64 decoding error",
    "Error de decodificación Base64",
    "Ошибка декодирования Base64"
);
msg!(
    MsgJsonError,
    "JSON parsing error",
    "Error de análisis JSON",
    "Ошибка разбора JSON"
);
msg!(
    MsgQrError,
    "QR code generation error",
    "Error de generación de código QR",
    "Ошибка генерации QR-кода"
);
msg!(
    MsgEncryptError,
    "Encryption failed",
    "Falló el cifrado",
    "Ошибка шифрования"
);
msg!(
    MsgDecryptError,
    "Decryption failed",
    "Falló el descifrado",
    "Ошибка расшифровки"
);
msg!(
    MsgInvalidUtf8,
    "Decrypted data is not valid UTF-8",
    "Los datos descifrados no son UTF-8 válidos",
    "Расшифрованные данные не являются допустимым UTF-8"
);
msg!(
    MsgNoNoteInUrl,
    "No note found in URL",
    "No se encontró nota en la URL",
    "Заметка не найдена в URL"
);

msg!(
    MsgLicenseText,
    r#"Copyright (c) 2025 Functora

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE."#,
    r#"Copyright (c) 2025 Functora

Por la presente se concede permiso, libre de cargos, a cualquier persona que haya obtenido una copia de este software y archivos de documentación asociados (el "Software"), para utilizar el Software sin restricción, incluyendo sin limitación los derechos a usar, copiar, modificar, fusionar, publicar, distribuir, sublicenciar y/o vender copias del Software, y a permitir a las personas a las que se les proporcione el Software que hagan lo mismo, sujeto a las siguientes condiciones:

El aviso de copyright anterior y este aviso de permiso deberán incluirse en todas las copias o partes sustanciales del Software.

EL SOFTWARE SE PROPORCIONA "TAL CUAL", SIN GARANTÍA DE NINGÚN TIPO, EXPRESA O IMPLÍCITA, INCLUYENDO PERO NO LIMITADO A LAS GARANTÍAS DE COMERCIABILIDAD, IDONEIDAD PARA UN FIN PARTICULAR Y NO INFRACCIÓN. EN NINGÚN CASO LOS AUTORES O TITULARES DEL COPYRIGHT SERÁN RESPONSABLES DE NINGUNA RECLAMACIÓN, DAÑOS U OTRAS RESPONSABILIDADES, YA SEA EN UNA ACCIÓN DE CONTRATO, AGRAVIO O DE OTRO TIPO, QUE SURJA DE, O EN RELACIÓN CON EL SOFTWARE O EL USO U OTROS TRATOS EN EL SOFTWARE."#,
    r#"Copyright (c) 2025 Functora

Настоящим предоставляется бесплатное разрешение любому лицу, получившему копию данного программного обеспечения и сопутствующих файлов документации (далее — «Программное обеспечение»), использовать Программное обеспечение без ограничений, включая неограниченное право использовать, копировать, изменять, объединять, публиковать, распространять, сублицензировать и/или продавать копии Программного обеспечения, а также разрешать лицам, которым предоставлено Программное обеспечение, делать то же самое, при соблюдении следующих условий:

Указанное выше уведомление об авторских правах и данное уведомление о разрешении должны быть включены во все копии или существенные части Программного обеспечения.

ПРОГРАММНОЕ ОБЕСПЕЧЕНИЕ ПРЕДОСТАВЛЯЕТСЯ «КАК ЕСТЬ», БЕЗ КАКИХ-ЛИБО ГАРАНТИЙ, ЯВНО ВЫРАЖЕННЫХ ИЛИ ПОДРАЗУМЕВАЕМЫХ, ВКЛЮЧАЯ, НО НЕ ОГРАНИЧИВАЯСЬ ГАРАНТИЯМИ ТОВАРНОГО СОСТОЯНИЯ, ПРИГОДНОСТИ ДЛЯ КОНКРЕТНЫХ ЦЕЛЕЙ И ОТСУТСТВИЯ НАРУШЕНИЙ АВТОРСКИХ ПРАВ. НИ ПРИ КАКИХ ОБСТООЯТЕЛЬСТВАХ АВТОРЫ ИЛИ ПРАВООБЛАДАТЕЛИ НЕ НЕСУТ ОТВЕТСТВЕННОСТИ ПО ЛЮБЫМ ПРЕТЕНЗИЯМ, ЗА УБЫТКИ ИЛИ ДРУГИЕ ТРЕБОВАНИЯ, ВЫТЕКАЮЩИЕ ИЗ ДОГОВОРА, ДЕЛИКТА ИЛИ ИНЫХ ОБСТОЯТЕЛЬСТВ, СВЯЗАННЫЕ С ПРОГРАММНЫМ ОБЕСПЕЧЕНИЕМ, ЕГО ИСПОЛЬЗОВАНИЕМ ИЛИ ДРУГИМИ ДЕЙСТВИЯМИ С ПРОГРАММНЫМ ОБЕСПЕЧЕНИЕМ."#
);

msg!(
    MsgPrivacyText,
    r#"Privacy Policy

This privacy policy applies to the app (hereby referred to as "Application") for mobile devices that was created by Functora (hereby referred to as "Service Provider") as a Free service. This service is intended for use "AS IS".

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

The Service Provider does not knowingly collect personally identifiable information from children. The Service Provider encourages all children to never submit any personally identifiable information through the Application and/or Services. The Service Provider encourage parents and legal guardians to monitor their children's Internet usage and to help enforce this Policy by instructing their children never to provide personally identifiable information through the Application and/or Services without their permission. If you have reason to believe that a child has provided personally identifiable information to the Service Provider through the Application and/or Services, please contact the Service Provider (functora@proton.me) so that they will be able to take the necessary actions. You must also be at least 16 years of age to consent to the processing of your personally identifiable information in your country (in some countries we may allow your parent or guardian to do so on your behalf).

Security

The Service Provider is concerned about safeguarding the confidentiality of your information. However, since the Application does not collect any information, there is no risk of your data being accessed by unauthorized individuals.

Changes

This Privacy Policy may be updated from time to time for any reason. The Service Provider will notify you of any changes to their Privacy Policy by updating this page with the new Privacy Policy. You are advised to consult this Privacy Policy regularly for any changes, as continued use is deemed approval of all changes.

This privacy policy is effective as of 2025-12-06

Your Consent

By using the Application, you are consenting to the processing of your information as set forth in this Privacy Policy now and as amended by the Service Provider.

Contact Us

If you have any questions regarding privacy while using the Application, or have questions about the practices, please contact the Service Provider via email at functora@proton.me."#,
    r#"Política de Privacidad

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

El Proveedor de Servicios no recopila a sabiendas información de identificación personal de niños. El Proveedor de Servicios alienta a todos los niños a que nunca envíen ninguna información de identificación personal a través de la Aplicación y/o los Servicios. El Proveedor de Servicios alienta a los padres y tutores legales a monitorear el uso de Internet de sus hijos y a ayudar a hacer cumplir esta Política instruyendo a sus hijos para que nunca proporcionen información de identificación personal a través de la Aplicación y/o los Servicios sin su permiso. Si tienes motivos para creer que un niño ha proporcionado información de identificación personal al Proveedor de Servicios a través de la Aplicación y/o los Servicios, comunícate con el Proveedor de Servicios (functora@proton.me) para que puedan tomar las medidas necesarias. También debes tener al menos 16 años de edad para dar tu consentimiento al procesamiento de tu información de identificación personal en tu país (en algunos países podemos permitir que tu padre o tutor lo haga en tu nombre).

Seguridad

El Proveedor de Servicios se preocupa por salvaguardar la confidencialidad de tu información. Sin embargo, dado que la Aplicación no recopila ninguna información, no existe riesgo de que personas no autorizadas accedan a tus datos.

Cambios

Esta Política de Privacidad puede actualizarse de vez en cuando por cualquier motivo. El Proveedor de Servicios te notificará de cualquier cambio en su Política de Privacidad actualizando esta página con la nueva Política de Privacidad. Se te aconseja consultar esta Política de Privacidad regularmente para ver si hay cambios, ya que el uso continuado se considera aprobación de todos los cambios.

Esta política de privacidad es efectiva a partir del 2025-12-06

Tu Consentimiento

Al usar la Aplicación, das tu consentimiento al procesamiento de tu información según lo establecido en esta Política de Privacidad ahora y según sea modificada por el Proveedor de Servicios.

Contáctanos

Si tienes alguna pregunta sobre la privacidad al usar la Aplicación, o tienes preguntas sobre las prácticas, comunícate con el Proveedor de Servicios por correo electrónico a functora@proton.me."#,
    r#"Политика конфиденциальности

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

Поставщик услуг не собирает намеренно личную информацию от детей. Поставщик услуг призывает всех детей никогда не отправлять какую-либо личную информацию через Приложение и/или Услуги. Поставщик услуг призывает родителей и законных опекунов контролировать использование Интернета их детьми и помогать обеспечивать соблюдение этой Политики, инструктируя своих детей никогда не предоставлять личную информацию через Приложение и/или Услуги без их разрешения. Если у вас есть основания полагать, что ребенок предоставил личную информацию Поставщику услуг через Приложение и/или Услуги, свяжитесь с Поставщиком услуг (functora@proton.me), чтобы они могли принять необходимые меры. Вам также должно быть не менее 16 лет, чтобы дать согласие на обработку вашей личной информации в вашей стране (в некоторых странах мы можем разрешить вашему родителю или опекуну сделать это от вашего имени).

Безопасность

Поставщик услуг заботится о защите конфиденциальности вашей информации. Однако, поскольку Приложение не собирает никакой информации, нет риска доступа к вашим данным посторонних лиц.

Изменения

Эта Политика конфиденциальности может время от времени обновляться по любой причине. Поставщик услуг уведомит вас о любых изменениях в своей Политике конфиденциальности, обновив эту страницу новой Политикой конфиденциальности. Вам рекомендуется регулярно просматривать эту Политику конфиденциальности на предмет изменений, так как продолжение использования считается одобрением всех изменений.

Эта политика конфиденциальности вступает в силу с 2025-12-06

Ваше согласие

Используя Приложение, вы даете согласие на обработку вашей информации, как изложено в этой Политике конфиденциальности сейчас и с изменениями, внесенными Поставщиком услуг.

Свяжитесь с нами

Если у вас есть какие-либо вопросы относительно конфиденциальности при использовании Приложения или вопросы о практике, свяжитесь с Поставщиком услуг по электронной почте functora@proton.me."#
);

msg!(
    MsgAboutText,
    r#"Cryptonote is a cross-platform, fully offline application for creating, storing, and sharing encrypted notes. It is completely serverless and runs entirely on your device or in your web browser - no internet connection or external services are required.

With Cryptonote, you can:

- Write a note in Markdown or HTML
- Optionally encrypt it using strong, well-established algorithms (e.g., AES-GCM or ChaCha20-Poly1305)
- Or leave it unencrypted
- Share the note instantly via a URL or a scannable QR code

All content - whether ciphertext or plaintext - is embedded directly in the URL itself, making sharing as simple as sending a link or displaying a QR code.

Cryptonote follows modern cryptographic best practices:

- Strong key derivation with HKDF, allowing users to supply just a password (which is used directly as the initial keying material)
- Authenticated encryption for confidentiality, integrity, and authenticity
- No data ever leaves your device unless you explicitly choose to share it

Secure, private, and truly offline - your notes remain yours alone."#,
    r#"Cryptonote es una aplicación multiplataforma y completamente offline para crear, almacenar y compartir notas cifradas. Es completamente sin servidores y se ejecuta completamente en su dispositivo o navegador web - no se requiere conexión a internet ni servicios externos.

Con Cryptonote, puedes:

- Escribir una nota en Markdown o HTML
- Opcionalmente cifrarla usando algoritmos fuertes y bien establecidos (p. ej., AES-GCM o ChaCha20-Poly1305)
- O dejarla sin cifrar
- Compartir la nota instantáneamente a través de una URL o un código QR escaneable

Todo el contenido - ya sea texto cifrado o plano - se incrusta directamente en la URL, lo que hace que compartir sea tan simple como enviar un enlace o mostrar un código QR.

Cryptonote sigue las mejores prácticas criptográficas modernas:

- Derivación de claves fuerte con HKDF, permitiendo a los usuarios proporcionar solo una contraseña (que se usa directamente como material inicial de claves)
- Cifrado autenticado para confidencialidad, integridad y autenticidad
- Ningún dato sale de su dispositivo a menos que usted elija explícitamente compartirlo

Seguro, privado y verdaderamente offline - sus notas siguen siendo solo suyas."#,
    r#"Cryptonote — кроссплатформенное, полностью автономное приложение для создания, хранения и обмена зашифрованными заметками. Оно полностью бессерверное и работает целиком на вашем устройстве или в веб-браузере — подключение к интернету или внешние сервисы не требуются.

С Cryptonote, вы можете:

- Написать заметку в Markdown или HTML
- Опционально зашифровать её с помощью надёжных, широко применяемых алгоритмов (например, AES-GCM или ChaCha20-Poly1305)
- Или оставить без шифрования
- Мгновенно поделиться заметкой через URL или сканируемый QR-код

Всё содержимое — будь то зашифрованный текст или открытый — встраивается непосредственно в URL, что делает совместное использование таким же простым, как отправка ссылки или демонстрация QR-кода.

Cryptonote следует современным криптографическим практикам:

- Надёжная деривация ключей с помощью HKDF позволяет пользователям использовать только пароль (который применяется напрямую как исходный ключевой материал)
- Аутентифицированное шифрование обеспечивает конфиденциальность, целостность и подлинность
- Никакие данные не покидают ваше устройство, пока вы явно не решите ими поделиться

Безопасно, приватно и по-настоящему автономно — ваши заметки остаются только вашими."#
);

msg!(MsgDonateIntro, "I'm Functora, the creator of this software. If you're enjoying it, a donation would be greatly appreciated. Sincerely yours, Functora.", "Soy Functora, el creador de este software. Si lo estás disfrutando, una donación sería muy apreciada. Atentamente, Functora.", "Я Functora, создатель этого программного обеспечения. Если оно вам нравится, я буду очень признателен за пожертвование. С уважением, Functora.");
