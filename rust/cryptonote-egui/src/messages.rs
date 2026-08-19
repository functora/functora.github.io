use crate::error::{AppError, MsgError};
use crate::i18n::I18N;
use functora_core::messages::Msg as BaseMsg;

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
            Self::Base(m) => m.render_eng(),
            Self::Error(e) => e.render_eng(),
            Self::Note => "Note".into(),            Self::NotePlaceholder => "Enter your note here (Markdown/HTML supported)...".into(),
            Self::Mode => "Mode".into(),
            Self::NoEncryption => "No encryption (plaintext)".into(),
            Self::EncryptionSuffix => "encryption".into(),
            Self::Share => "Share".into(),
            Self::Sent => "Sent!".into(),
            Self::SharedNoteText => "A note sent via Cryptonote".into(),
            Self::ShareAppDesc => "Cryptonote is a cross-platform, serverless app for encrypted offline notes. Try it:".into(),
            Self::EncryptedNote => "Encrypted".into(),
            Self::EncryptedNoteDesc => "This note is encrypted. Enter the password to decrypt it.".into(),
            Self::DecryptButton => "Decrypt".into(),
            Self::CreateNewNote => "Reset".into(),
            Self::EditNote => "Edit".into(),
            Self::ViewButton => "View".into(),
            Self::OpenUrlLabel => "URL".into(),
            Self::OpenUrlPlaceholder => "Paste shared note URL here...".into(),
            Self::OpenButton => "Open URL".into(),
            Self::ActionLabel => "Action".into(),
            Self::ActionCreate => "Create note".into(),
            Self::ActionOpen => "Open note".into(),
            Self::ActionScan => "Scan note".into(),
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

Secure, private, and truly offline - your notes remain yours alone.".into(),
            Self::Print => "Print".into(),
            Self::Clear => "Clear".into(),
            Self::AttachFiles => "Attach files".into(),
            Self::RemoveFile => "Remove".into(),
            Self::ArchiveReady => "Cryptonote archive ready. Press Download to save and share with your recipient.".into(),
            Self::OpenArchive => "Open archive".into(),
            Self::Download => "Download".into(),
            Self::DownloadAll => "Download all".into(),
            Self::FileName => "Name".into(),
            Self::FileSize => "Size".into(),
            Self::Preview => "Preview".into(),
            Self::File => "File".into(),
            Self::PreviewUnavailable => "Preview is not available for this file type".into(),
            Self::FileNotFound => "File not found".into(),
            Self::Downloaded(loc) => format!("Downloaded: {loc}"),
        }
    }

    fn render_spa(&self) -> String {
        match self {
            Self::Base(m) => m.render_spa(),
            Self::Error(e) => e.render_spa(),
            Self::Note => "Nota".into(),
            Self::NotePlaceholder => "Escribe tu nota aquí (Markdown/HTML soportado)...".into(),
            Self::Mode => "Modo".into(),
            Self::NoEncryption => "Sin cifrado (texto plano)".into(),
            Self::EncryptionSuffix => "cifrado".into(),
            Self::Share => "Compartir".into(),
            Self::Sent => "¡Enviado!".into(),
            Self::SharedNoteText => "Nota enviada vía Cryptonote".into(),
            Self::ShareAppDesc => "Cryptonote es una aplicación multiplataforma y sin servidores para notas cifradas sin conexión. Pruébala:".into(),
            Self::EncryptedNote => "Cifrado".into(),
            Self::EncryptedNoteDesc => "Esta nota está cifrada. Ingresa la contraseña para descifrarla.".into(),
            Self::DecryptButton => "Descifrar".into(),
            Self::CreateNewNote => "Reiniciar".into(),
            Self::EditNote => "Editar".into(),
            Self::ViewButton => "Ver".into(),
            Self::OpenUrlLabel => "URL".into(),
            Self::OpenUrlPlaceholder => "Pega la URL de la nota compartida aquí...".into(),
            Self::OpenButton => "Abrir URL".into(),
            Self::ActionLabel => "Acción".into(),
            Self::ActionCreate => "Crear nota".into(),
            Self::ActionOpen => "Abrir nota".into(),
            Self::ActionScan => "Escanear nota".into(),
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

Seguro, privado y verdaderamente offline - sus notas siguen siendo solo suyas.".into(),
            Self::Print => "Imprimir".into(),
            Self::Clear => "Borrar".into(),
            Self::AttachFiles => "Adjuntar archivos".into(),
            Self::RemoveFile => "Eliminar".into(),
            Self::ArchiveReady => "Archivo Cryptonote listo. Presiona Descargar para guardar y compartir con tu destinatario.".into(),
            Self::OpenArchive => "Abrir archivo".into(),
            Self::Download => "Descargar".into(),
            Self::DownloadAll => "Descargar todo".into(),
            Self::FileName => "Nombre".into(),
            Self::FileSize => "Tamaño".into(),
            Self::Preview => "Vista previa".into(),
            Self::File => "Archivo".into(),
            Self::PreviewUnavailable => "La vista previa no está disponible para este tipo de archivo".into(),
            Self::FileNotFound => "Archivo no encontrado".into(),
            Self::Downloaded(loc) => format!("Descargado: {loc}"),
        }
    }

    fn render_rus(&self) -> String {
        match self {
            Self::Base(m) => m.render_rus(),
            Self::Error(e) => e.render_rus(),
            Self::Note => "Заметка".into(),
            Self::NotePlaceholder => "Введите вашу заметку здесь (Markdown/HTML поддерживается)...".into(),
            Self::Mode => "Режим".into(),
            Self::NoEncryption => "Без шифрования (открытый текст)".into(),
            Self::EncryptionSuffix => "шифрование".into(),
            Self::Share => "Поделиться".into(),
            Self::Sent => "Отправлено!".into(),
            Self::SharedNoteText => "Заметка, отправленная через Cryptonote".into(),
            Self::ShareAppDesc => "Cryptonote - кроссплатформенное бессерверное приложение для зашифрованных офлайн-заметок. Попробуйте:".into(),
            Self::EncryptedNote => "Шифр".into(),
            Self::EncryptedNoteDesc => "Эта заметка зашифрована. Введите пароль для расшифровки.".into(),
            Self::DecryptButton => "Расшифровать".into(),
            Self::CreateNewNote => "Сброс".into(),
            Self::EditNote => "Правка".into(),
            Self::ViewButton => "Смотреть".into(),
            Self::OpenUrlLabel => "URL".into(),
            Self::OpenUrlPlaceholder => "Вставьте URL заметки здесь...".into(),
            Self::OpenButton => "Открыть URL".into(),
            Self::ActionLabel => "Действие".into(),
            Self::ActionCreate => "Создать заметку".into(),
            Self::ActionOpen => "Открыть заметку".into(),
            Self::ActionScan => "Сканировать заметку".into(),
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

Безопасно, приватно и по-настоящему автономно - ваши заметки остаются только вашими.".into(),
            Self::Print => "Печать".into(),
            Self::Clear => "Очистить".into(),
            Self::AttachFiles => "Прикрепить файлы".into(),
            Self::RemoveFile => "Удалить".into(),
            Self::ArchiveReady => "Архив Cryptonote готов. Нажмите Скачать, чтобы сохранить и поделиться с получателем.".into(),
            Self::OpenArchive => "Открыть архив".into(),
            Self::Download => "Скачать".into(),
            Self::DownloadAll => "Скачать всё".into(),
            Self::FileName => "Имя".into(),
            Self::FileSize => "Размер".into(),
            Self::Preview => "Предпросмотр".into(),
            Self::File => "Файл".into(),
            Self::PreviewUnavailable => "Предварительный просмотр недоступен для этого типа файлов".into(),
            Self::FileNotFound => "Файл не найден".into(),
            Self::Downloaded(loc) => format!("Скачано: {loc}"),
        }
    }
}
impl From<AppError> for Msg {
    fn from(e: AppError) -> Self {
        Self::Error(MsgError::from(e))
    }
}
