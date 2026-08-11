use crate::error::MsgError;
use functora_dioxus::i18n::I18N;
use functora_dioxus::Msg as BaseMsg;

#[derive(Clone, Debug, PartialEq)]
pub enum Msg {
    Base(BaseMsg),
    Error(MsgError),
    Note,
    NotePlaceholder,
    Mode,
    NoEncryption,
    EncryptionSuffix,
    Share,
    Sent,
    SharedNoteText,
    ShareAppDesc,
    EncryptedNote,
    EncryptedNoteDesc,
    DecryptButton,
    CreateNewNote,
    EditNote,
    ViewButton,
    OpenUrlLabel,
    OpenUrlPlaceholder,
    OpenButton,
    ActionLabel,
    ActionCreate,
    ActionOpen,
    ActionScan,
    AboutText,
    Print,
    Clear,
    AttachFiles,
    RemoveFile,
    ArchiveReady,
    OpenArchive,
    Download,
    DownloadAll,
    FileName,
    FileSize,
    Preview,
    File,
    PreviewUnavailable,
    FileNotFound,
    Downloaded(String),
}

impl I18N for Msg {
    fn render_eng(&self) -> String {
        match self {
            Self::Base(m) => return m.render_eng(),
            Self::Note => "Note",
            Self::NotePlaceholder => "Enter your note here (Markdown/HTML supported)...",
            Self::Mode => "Mode",
            Self::NoEncryption => "No encryption (plaintext)",
            Self::EncryptionSuffix => "encryption",
            Self::Share => "Share",
            Self::Sent => "Sent!",
            Self::SharedNoteText => "A note sent via Cryptonote",
            Self::ShareAppDesc => "Cryptonote is a cross-platform, serverless app for encrypted offline notes. Try it:",
            Self::EncryptedNote => "Encrypted",
            Self::EncryptedNoteDesc => "This note is encrypted. Enter the password to decrypt it.",
            Self::DecryptButton => "Decrypt",
            Self::CreateNewNote => "Reset",
            Self::EditNote => "Edit",
            Self::ViewButton => "View",
            Self::OpenUrlLabel => "URL",
            Self::OpenUrlPlaceholder => "Paste shared note URL here...",
            Self::OpenButton => "Open URL",
            Self::ActionLabel => "Action",
            Self::ActionCreate => "Create note",
            Self::ActionOpen => "Open note",
            Self::ActionScan => "Scan note",
            Self::Error(e) => return e.render_eng(),
            Self::AboutText => r"Cryptonote is a cross-platform, fully offline application for creating, storing, and sharing encrypted notes. It is completely serverless and runs entirely on your device or in your web browser - no internet connection or external services are required.

With Cryptonote, you can:

- Write a note in Markdown or HTML
- Optionally encrypt it using strong, well-established algorithms (e.g., AES-GCM or ChaCha20-Poly1305)
- Or leave it unencrypted
- Share the note instantly via a URL or a scannable QR code
- Attach files to your note and pack everything into a secure offline archive (.cryptonote)

Note content - whether ciphertext or plaintext - is embedded directly in the URL itself, making sharing as simple as sending a link or displaying a QR code. Notes with attachments are packaged into an archive file that you download and share separately.

Cryptonote follows modern cryptographic best practices:

- Strong password-based key derivation with Argon2id
- Authenticated encryption for confidentiality, integrity, and authenticity
- No data ever leaves your device unless you explicitly choose to share it

Secure, private, and truly offline - your notes remain yours alone.",
            Self::Print => "Print",
            Self::Clear => "Clear",
            Self::AttachFiles => "Attach files",
            Self::RemoveFile => "Remove",
            Self::ArchiveReady => "Cryptonote archive ready. Press Download to save and share with your recipient.",
            Self::OpenArchive => "Open archive",
            Self::Download => "Download",
            Self::DownloadAll => "Download all",
            Self::FileName => "Name",
            Self::FileSize => "Size",
            Self::Preview => "Preview",
            Self::File => "File",
            Self::PreviewUnavailable => "Preview is not available for this file type",
            Self::FileNotFound => "File not found",
            Self::Downloaded(loc) => return format!("Downloaded: {loc}"),
        }
        .to_string()
    }

    fn render_spa(&self) -> String {
        match self {
            Self::Base(m) => return m.render_spa(),
            Self::Note => "Nota",
            Self::NotePlaceholder => "Escribe tu nota aquí (Markdown/HTML soportado)...",
            Self::Mode => "Modo",
            Self::NoEncryption => "Sin cifrado (texto plano)",
            Self::EncryptionSuffix => "cifrado",
            Self::Share => "Compartir",
            Self::Sent => "¡Enviado!",
            Self::SharedNoteText => "Nota enviada vía Cryptonote",
            Self::ShareAppDesc => "Cryptonote es una aplicación multiplataforma y sin servidores para notas cifradas sin conexión. Pruébala:",
            Self::EncryptedNote => "Cifrado",
            Self::EncryptedNoteDesc => "Esta nota está cifrada. Ingresa la contraseña para descifrarla.",
            Self::DecryptButton => "Descifrar",
            Self::CreateNewNote => "Reiniciar",
            Self::EditNote => "Editar",
            Self::ViewButton => "Ver",
            Self::OpenUrlLabel => "URL",
            Self::OpenUrlPlaceholder => "Pega la URL de la nota compartida aquí...",
            Self::OpenButton => "Abrir URL",
            Self::ActionLabel => "Acción",
            Self::ActionCreate => "Crear nota",
            Self::ActionOpen => "Abrir nota",
            Self::ActionScan => "Escanear nota",
            Self::Error(e) => return e.render_spa(),
            Self::AboutText => r"Cryptonote es una aplicación multiplataforma y completamente offline para crear, almacenar y compartir notas cifradas. Es completamente sin servidores y se ejecuta completamente en su dispositivo o navegador web - no se requiere conexión a internet ni servicios externos.

Con Cryptonote, puedes:

- Escribir una nota en Markdown o HTML
- Opcionalmente cifrarla usando algoritmos fuertes y bien establecidos (p. ej., AES-GCM o ChaCha20-Poly1305)
- O dejarla sin cifrar
- Compartir la nota instantáneamente a través de una URL o un código QR escaneable
- Adjuntar archivos a tu nota y empaquetarlo todo en un archivo offline seguro (.cryptonote)

El contenido de la nota - ya sea texto cifrado o plano - se incrusta directamente en la URL, lo que hace que compartir sea tan simple como enviar un enlace o mostrar un código QR. Las notas con archivos adjuntos se empaquetan en un archivo que descargas y compartes por separado.

Cryptonote sigue las mejores prácticas criptográficas modernas:

- Derivación de claves robusta basada en contraseña con Argon2id
- Cifrado autenticado para confidencialidad, integridad y autenticidad
- Ningún dato sale de su dispositivo a menos que usted elija explícitamente compartirlo

Seguro, privado y verdaderamente offline - sus notas siguen siendo solo suyas.",
            Self::Print => "Imprimir",
            Self::Clear => "Borrar",
            Self::AttachFiles => "Adjuntar archivos",
            Self::RemoveFile => "Eliminar",
            Self::ArchiveReady => "Archivo Cryptonote listo. Presiona Descargar para guardar y compartir con tu destinatario.",
            Self::OpenArchive => "Abrir archivo",
            Self::Download => "Descargar",
            Self::DownloadAll => "Descargar todo",
            Self::FileName => "Nombre",
            Self::FileSize => "Tamaño",
            Self::Preview => "Vista previa",
            Self::File => "Archivo",
            Self::PreviewUnavailable => "La vista previa no está disponible para este tipo de archivo",
            Self::FileNotFound => "Archivo no encontrado",
            Self::Downloaded(loc) => return format!("Descargado: {loc}"),
        }
        .to_string()
    }

    fn render_rus(&self) -> String {
        match self {
            Self::Base(m) => return m.render_rus(),
            Self::Note => "Заметка",
            Self::NotePlaceholder => "Введите вашу заметку здесь (Markdown/HTML поддерживается)...",
            Self::Mode => "Режим",
            Self::NoEncryption => "Без шифрования (открытый текст)",
            Self::EncryptionSuffix => "шифрование",
            Self::Share => "Поделиться",
            Self::Sent => "Отправлено!",
            Self::SharedNoteText => "Заметка, отправленная через Cryptonote",
            Self::ShareAppDesc => "Cryptonote - кроссплатформенное бессерверное приложение для зашифрованных офлайн-заметок. Попробуйте:",
            Self::EncryptedNote => "Шифр",
            Self::EncryptedNoteDesc => "Эта заметка зашифрована. Введите пароль для расшифровки.",
            Self::DecryptButton => "Расшифровать",
            Self::CreateNewNote => "Сброс",
            Self::EditNote => "Правка",
            Self::ViewButton => "Смотреть",
            Self::OpenUrlLabel => "URL",
            Self::OpenUrlPlaceholder => "Вставьте URL заметки здесь...",
            Self::OpenButton => "Открыть URL",
            Self::ActionLabel => "Действие",
            Self::ActionCreate => "Создать заметку",
            Self::ActionOpen => "Открыть заметку",
            Self::ActionScan => "Сканировать заметку",
            Self::Error(e) => return e.render_rus(),
            Self::AboutText => r"Cryptonote - кроссплатформенное, полностью автономное приложение для создания, хранения и обмена зашифрованными заметками. Оно полностью бессерверное и работает целиком на вашем устройстве или в веб-браузере - подключение к интернету или внешние сервисы не требуются.

С Cryptonote, вы можете:

- Написать заметку в Markdown или HTML
- Опционально зашифровать её с помощью надёжных, широко применяемых алгоритмов (например, AES-GCM или ChaCha20-Poly1305)
- Или оставить без шифрования
- Мгновенно поделиться заметкой через URL или сканируемый QR-код
- Прикрепить файлы к заметке и упаковать всё в защищённый автономный архив (.cryptonote)

Содержимое заметки - будь то зашифрованный текст или открытый - встраивается непосредственно в URL, что делает совместное использование таким же простым, как отправка ссылки или демонстрация QR-кода. Заметки с вложениями упаковываются в архивный файл, который вы скачиваете и отправляете отдельно.

Cryptonote следует современным криптографическим практикам:

- Надёжная деривация ключей из пароля на основе Argon2id
- Аутентифицированное шифрование обеспечивает конфиденциальность, целостность и подлинность
- Никакие данные не покидают ваше устройство, пока вы явно не решите ими поделиться

Безопасно, приватно и по-настоящему автономно - ваши заметки остаются только вашими.",
            Self::Print => "Печать",
            Self::Clear => "Очистить",
            Self::AttachFiles => "Прикрепить файлы",
            Self::RemoveFile => "Удалить",
            Self::ArchiveReady => "Архив Cryptonote готов. Нажмите Скачать, чтобы сохранить и поделиться с получателем.",
            Self::OpenArchive => "Открыть архив",
            Self::Download => "Скачать",
            Self::DownloadAll => "Скачать всё",
            Self::FileName => "Имя",
            Self::FileSize => "Размер",
            Self::Preview => "Предпросмотр",
            Self::File => "Файл",
            Self::PreviewUnavailable => "Предварительный просмотр недоступен для этого типа файлов",
            Self::FileNotFound => "Файл не найден",
            Self::Downloaded(loc) => return format!("Скачано: {loc}"),
        }
        .to_string()
    }
}
