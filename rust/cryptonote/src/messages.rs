use crate::error::MsgError;
use functora_egui::i18n::I18N;
use functora_egui::messages::Msg as BaseMsg;

#[derive(Clone, Debug, strum::EnumCount)]
pub enum Msg {
    Base(BaseMsg),
    Error(MsgError),
    Note,
    NotePlaceholder,
    Encryption,
    NoEncryption,
    Share,
    Shared,
    SharedNoteText,
    ShareAppDesc,
    EncryptedNote,
    EncryptedNoteDesc,
    DecryptButton,
    EditNote,
    Reset,
    ResetDone,
    ViewButton,
    OpenUrlLabel,
    OpenUrlPlaceholder,
    OpenButton,
    ActionLabel,
    ActionCreate,
    ActionOpen,
    ActionScan,
    AboutText,
    AboutSecurityPrefix,
    AboutSecuritySuffix,
    Security,
    AttachFiles,
    RemoveFile,
    ArchiveReady,
    OpenArchive,
    Save,
    SaveAll,
    File,
    PreviewUnavailable,
    FileNotFound,
    Saved(String),
    WeakPasswordWarning,
    PlainShareWarning,
    ClipboardPlaintextWarning,
    UnencryptedExportWarning,
}

impl I18N for Msg {
    fn render_eng(&self) -> String {
        match self {
            Self::Base(m) => m.render_eng(),
            Self::Note => "Note".into(),
            Self::NotePlaceholder => "Enter your note here (Markdown/HTML supported)...".into(),
            Self::Encryption => "Encryption".into(),
            Self::NoEncryption => "No encryption (plaintext)".into(),
            Self::Share => "Share".into(),
            Self::Shared => "Shared!".into(),
            Self::SharedNoteText => "A note sent via Cryptonote".into(),
            Self::ShareAppDesc => {
                "Cryptonote is a cross-platform app for encrypted notes. Try it:".into()
            }
            Self::EncryptedNote => "Encrypted".into(),
            Self::EncryptedNoteDesc => {
                "This note is encrypted. Enter the password to decrypt it.".into()
            }
            Self::DecryptButton => "Decrypt".into(),
            Self::EditNote => "Edit".into(),
            Self::Reset => "Reset".into(),
            Self::ResetDone => "App reset".into(),
            Self::ViewButton => "View".into(),
            Self::OpenUrlLabel => "URL".into(),
            Self::OpenUrlPlaceholder => "Paste shared note URL here...".into(),
            Self::OpenButton => "Open URL".into(),
            Self::ActionLabel => "Choose action".into(),
            Self::ActionCreate => "Create note".into(),
            Self::ActionOpen => "Open note".into(),
            Self::ActionScan => "Scan note".into(),
            Self::Error(e) => e.render_eng(),
            Self::AboutText => r"Cryptonote is a cross-platform application for creating, storing, and sharing encrypted notes. It has no backend and no account: all cryptography runs on your device or in your browser.

With Cryptonote, you can:

- Write a note in Markdown or HTML
- Optionally encrypt it using strong, well-established algorithms (e.g., AES-GCM or ChaCha20-Poly1305)
- Or leave it unencrypted
- Share the note instantly via a URL or a scannable QR code
- Attach files to your note and pack everything into an offline archive (.cryptonote)

Note content - whether ciphertext or plaintext - is embedded directly in the URL itself, making sharing as simple as sending a link or displaying a QR code. Notes with attachments are packaged into an archive file that you save and share separately.

Cryptonote uses modern cryptographic primitives:

- Password-based key derivation with Argon2id
- Authenticated encryption for confidentiality and integrity against tampering
- Nothing is stored anywhere, and no data leaves your device unless you explicitly choose to share it

What Cryptonote does not do:

- It does not prove who wrote a note. Authenticated encryption detects tampering, not a forged author.
- It is not anonymous. A shared URL or QR code still travels through mail, chat, or wherever you send it.
- The note travels inside the link itself. Plain mode puts the note text in the clear, so everyone who gets the link can read it.
- In the web build the note rides in the URL of the address bar, so the browser history and anything else that handles URLs can see it too.
- A shared link reveals the ciphertext, so a weak password can be cracked offline. Use a long passphrase.
- Copying text to the clipboard and exporting an unencrypted archive expose the note to other software on the device.".into(),
            Self::AboutSecurityPrefix => "See the ".into(),
            Self::AboutSecuritySuffix => " screen for the full threat model.".into(),
            Self::Security => "Security".into(),
            Self::AttachFiles => "Attach files".into(),
            Self::RemoveFile => "Remove".into(),
            Self::ArchiveReady => {
                "Archive ready. Press Save to share it with your recipient.".into()
            }
            Self::OpenArchive => "Open archive".into(),
            Self::Save => "Save".into(),
            Self::SaveAll => "Save all".into(),
            Self::File => "File".into(),
            Self::PreviewUnavailable => "Preview is not available for this file type".into(),
            Self::FileNotFound => "File not found".into(),
            Self::Saved(loc) => format!("Saved: {loc}"),
            Self::WeakPasswordWarning => {
                "This password is short. Anyone who gets the encrypted note can try every password offline, so use a long one.".into()
            }
            Self::PlainShareWarning => {
                "This note is not encrypted. Everyone who gets the link can read the text.".into()
            }
            Self::ClipboardPlaintextWarning => {
                "Copied the decrypted text to the clipboard. Other applications can read it.".into()
            }
            Self::UnencryptedExportWarning => {
                "This archive is not encrypted. Anyone who gets the file can read its contents.".into()
            }
        }
    }

    fn render_spa(&self) -> String {
        match self {
            Self::Base(m) => m.render_spa(),
            Self::Note => "Nota".into(),
            Self::NotePlaceholder => {
                "Escribe tu nota aquí (Markdown/HTML soportado)...".into()
            }
            Self::Encryption => "Cifrado".into(),
            Self::NoEncryption => "Sin cifrado (texto plano)".into(),
            Self::Share => "Compartir".into(),
            Self::Shared => "¡Compartido!".into(),
            Self::SharedNoteText => "Nota enviada vía Cryptonote".into(),
            Self::ShareAppDesc => {
                "Cryptonote es una aplicación multiplataforma para notas cifradas. Pruébala:".into()
            }
            Self::EncryptedNote => "Nota cifrada".into(),
            Self::EncryptedNoteDesc => {
                "Esta nota está cifrada. Ingresa la contraseña para descifrarla.".into()
            }
            Self::DecryptButton => "Descifrar".into(),
            Self::EditNote => "Editar".into(),
            Self::Reset => "Restablecer".into(),
            Self::ResetDone => "Aplicación restablecida".into(),
            Self::ViewButton => "Ver".into(),
            Self::OpenUrlLabel => "URL".into(),
            Self::OpenUrlPlaceholder => "Pega la URL de la nota compartida aquí...".into(),
            Self::OpenButton => "Abrir URL".into(),
            Self::ActionLabel => "Elige una acción".into(),
            Self::ActionCreate => "Crear nota".into(),
            Self::ActionOpen => "Abrir nota".into(),
            Self::ActionScan => "Escanear nota".into(),
            Self::Error(e) => e.render_spa(),
            Self::AboutText => r"Cryptonote es una aplicación multiplataforma para crear, almacenar y compartir notas cifradas. No tiene cuenta ni servicios externos: toda la criptografía se ejecuta en tu dispositivo o en tu navegador.

Con Cryptonote, puedes:

- Escribir una nota en Markdown o HTML
- Opcionalmente cifrarla usando algoritmos fuertes y bien establecidos (p. ej., AES-GCM o ChaCha20-Poly1305)
- O dejarla sin cifrar
- Compartir la nota instantáneamente a través de una URL o un código QR escaneable
- Adjuntar archivos a tu nota y empaquetarlo todo en un archivo offline (.cryptonote)

El contenido de la nota - ya sea texto cifrado o plano - se incrusta directamente en la URL, lo que hace que compartir sea tan simple como enviar un enlace o mostrar un código QR. Las notas con archivos adjuntos se empaquetan en un archivo que guardas y compartes por separado.

Cryptonote usa primitivas criptográficas modernas:

- Derivación de claves basada en contraseña con Argon2id
- Cifrado autenticado para confidencialidad e integridad frente a manipulaciones
- No se guarda nada en ninguna parte, y ningún dato sale de tu dispositivo a menos que elijas explícitamente compartirlo

Lo que Cryptonote no hace:

- No demuestra quién escribió una nota. El cifrado autenticado detecta manipulaciones, no un autor falsificado.
- No es anónimo. Una URL o un código QR compartido sigue viajando por correo, chat o donde tú lo envíes.
- La nota viaja dentro del enlace. El modo sin cifrar pone el texto de la nota en claro, así que cualquiera que reciba el enlace puede leerlo.
- En la versión web la nota viaja en la URL de la barra de direcciones, así que el historial del navegador y cualquier otro sitio que gestione URLs también pueden verla.
- Un enlace compartido revela el texto cifrado, así que una contraseña débil se puede descifrar sin conexión. Usa una frase larga.
- Copiar texto al portapapeles y exportar un archivo sin cifrar exponen la nota a otro software del dispositivo.".into(),
            Self::AboutSecurityPrefix => "Consulta la pantalla de ".into(),
            Self::AboutSecuritySuffix => " para el modelo de amenazas completo.".into(),
            Self::Security => "Seguridad".into(),
            Self::AttachFiles => "Adjuntar archivos".into(),
            Self::RemoveFile => "Eliminar".into(),
            Self::ArchiveReady => {
                "Archivo listo. Presiona Guardar para compartirlo con tu destinatario.".into()
            }
            Self::OpenArchive => "Abrir archivo".into(),
            Self::Save => "Guardar".into(),
            Self::SaveAll => "Guardar todo".into(),
            Self::File => "Archivo".into(),
            Self::PreviewUnavailable => {
                "La vista previa no está disponible para este tipo de archivo".into()
            }
            Self::FileNotFound => "Archivo no encontrado".into(),
            Self::Saved(loc) => format!("Guardado: {loc}"),
            Self::WeakPasswordWarning => {
                "Esta contraseña es corta. Cualquiera que obtenga la nota cifrada puede probar todas las contraseñas sin conexión, así que usa una larga.".into()
            }
            Self::PlainShareWarning => {
                "Esta nota no está cifrada. Cualquiera que reciba el enlace puede leer el texto.".into()
            }
            Self::ClipboardPlaintextWarning => {
                "Se copió el texto descifrado al portapapeles. Otras aplicaciones pueden leerlo.".into()
            }
            Self::UnencryptedExportWarning => {
                "Este archivo no está cifrado. Cualquiera que lo obtenga puede leer su contenido.".into()
            }
        }
    }

    fn render_rus(&self) -> String {
        match self {
            Self::Base(m) => m.render_rus(),
            Self::Note => "Заметка".into(),
            Self::NotePlaceholder => {
                "Введите вашу заметку здесь (Markdown/HTML поддерживается)...".into()
            }
            Self::Encryption => "Шифрование".into(),
            Self::NoEncryption => "Без шифрования (открытый текст)".into(),
            Self::Share => "Поделиться".into(),
            Self::Shared => "Отправлено!".into(),
            Self::SharedNoteText => "Заметка, отправленная через Cryptonote".into(),
            Self::ShareAppDesc => {
                "Cryptonote - кроссплатформенное приложение для зашифрованных заметок. Попробуйте:".into()
            }
            Self::EncryptedNote => "Зашифровано".into(),
            Self::EncryptedNoteDesc => {
                "Эта заметка зашифрована. Введите пароль для расшифровки.".into()
            }
            Self::DecryptButton => "Расшифровать".into(),
            Self::EditNote => "Изменить".into(),
            Self::Reset => "Сбросить".into(),
            Self::ResetDone => "Приложение сброшено".into(),
            Self::ViewButton => "Просмотр".into(),
            Self::OpenUrlLabel => "URL".into(),
            Self::OpenUrlPlaceholder => "Вставьте URL заметки здесь...".into(),
            Self::OpenButton => "Открыть URL".into(),
            Self::ActionLabel => "Выберите действие".into(),
            Self::ActionCreate => "Создать заметку".into(),
            Self::ActionOpen => "Открыть заметку".into(),
            Self::ActionScan => "Сканировать заметку".into(),
            Self::Error(e) => e.render_rus(),
            Self::AboutText => r"Cryptonote - кроссплатформенное приложение для создания, хранения и обмена зашифрованными заметками. У него нет учётной записи и внешних сервисов: вся криптография выполняется на вашем устройстве или в браузере.

С Cryptonote вы можете:

- Написать заметку в Markdown или HTML
- Опционально зашифровать её с помощью надёжных, широко применяемых алгоритмов (например, AES-GCM или ChaCha20-Poly1305)
- Или оставить без шифрования
- Мгновенно поделиться заметкой через URL или сканируемый QR-код
- Прикрепить файлы к заметке и упаковать всё в автономный архив (.cryptonote)

Содержимое заметки - будь то зашифрованный текст или открытый - встраивается непосредственно в URL, что делает совместное использование таким же простым, как отправка ссылки или демонстрация QR-кода. Заметки с вложениями упаковываются в архивный файл, который вы сохраняете и отправляете отдельно.

Cryptonote использует современные криптографические примитивы:

- Деривация ключей из пароля на основе Argon2id
- Аутентифицированное шифрование обеспечивает конфиденциальность и целостность при проверке на подмену
- Нигде ничего не хранится, и никакие данные не покидают ваше устройство, пока вы явно не решите ими поделиться

Чего Cryptonote не делает:

- Не доказывает, кто написал заметку. Аутентифицированное шифрование обнаруживает подмену, а не поддельного автора.
- Не обеспечивает анонимность. Ссылка или QR-код всё равно проходят через почту, чат или куда угодно, куда вы их отправите.
- Заметка едет внутри ссылки. В режиме без шифрования текст заметки передаётся открытым, поэтому любой, кто получит ссылку, сможет его прочитать.
- В веб-версии заметка едет в URL из адресной строки, поэтому её видят и история браузера, и всё, что работает с URL.
- Общая ссылка раскрывает зашифрованный текст, поэтому слабый пароль можно подобрать без подключения к сети. Используйте длинную парольную фразу.
- Копирование текста в буфер обмена и экспорт незашифрованного архива открывают заметку другому программному обеспечению на устройстве.".into(),
            Self::AboutSecurityPrefix => "Полная модель угроз описана на экране «".into(),
            Self::AboutSecuritySuffix => "».".into(),
            Self::Security => "Безопасность".into(),
            Self::AttachFiles => "Прикрепить файлы".into(),
            Self::RemoveFile => "Удалить".into(),
            Self::ArchiveReady => {
                "Архив готов. Нажмите «Сохранить», чтобы поделиться им с получателем.".into()
            }
            Self::OpenArchive => "Открыть архив".into(),
            Self::Save => "Сохранить".into(),
            Self::SaveAll => "Сохранить всё".into(),
            Self::File => "Файл".into(),
            Self::PreviewUnavailable => {
                "Предварительный просмотр недоступен для этого типа файлов".into()
            }
            Self::FileNotFound => "Файл не найден".into(),
            Self::Saved(loc) => format!("Сохранено: {loc}"),
            Self::WeakPasswordWarning => {
                "Этот пароль короткий. Любой, кто получит зашифрованную заметку, может перебрать пароли без подключения к сети, поэтому используйте длинный.".into()
            }
            Self::PlainShareWarning => {
                "Эта заметка не зашифрована. Любой, кто получит ссылку, сможет прочитать текст.".into()
            }
            Self::ClipboardPlaintextWarning => {
                "Расшифрованный текст скопирован в буфер обмена. Другие приложения могут его прочитать.".into()
            }
            Self::UnencryptedExportWarning => {
                "Этот архив не зашифрован. Любой, кто получит файл, сможет прочитать его содержимое.".into()
            }
        }
    }
}
