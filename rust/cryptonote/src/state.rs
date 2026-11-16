use crate::crypto::{CipherType, ExternalArchive};
use crate::encoding::NoteData;
use crate::progress::{Job, Stage};
use zeroize::Zeroizing;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct AttachmentIdx(pub usize);

impl AttachmentIdx {
    #[must_use]
    pub fn new(index: usize, len: usize) -> Option<Self> {
        (index < len).then_some(Self(index))
    }

    #[must_use]
    pub fn get(self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone, PartialEq, Copy, Default)]
pub enum ActionMode {
    #[default]
    Create,
    Open,
    Scan,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum CipherChoice {
    Plain,
    #[default]
    Aes256Gcm,
    ChaCha20Poly1305,
}

impl From<Option<CipherType>> for CipherChoice {
    fn from(cipher: Option<CipherType>) -> Self {
        match cipher {
            None => Self::Plain,
            Some(CipherType::Aes256Gcm) => Self::Aes256Gcm,
            Some(CipherType::ChaCha20Poly1305) => Self::ChaCha20Poly1305,
        }
    }
}

impl From<CipherChoice> for Option<CipherType> {
    fn from(choice: CipherChoice) -> Self {
        match choice {
            CipherChoice::Plain => None,
            CipherChoice::Aes256Gcm => Some(CipherType::Aes256Gcm),
            CipherChoice::ChaCha20Poly1305 => Some(CipherType::ChaCha20Poly1305),
        }
    }
}

#[derive(Clone)]
pub struct ExternalNote {
    pub data: NoteData,
    pub url: String,
}

/// The URL carries the note payload, and in plain mode that payload is the
/// note body, so neither the body nor the URL may reach a log.
impl std::fmt::Debug for ExternalNote {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ExternalNote")
            .field("data", &self.data)
            .field("url", &"[redacted]")
            .finish()
    }
}

#[derive(Clone, Default)]
pub enum External {
    #[default]
    Nothing,
    Note(ExternalNote),
    Archive(ExternalArchive),
}

/// An archive carries the note body in plain mode, so only its size may
/// reach a log.
impl std::fmt::Debug for External {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nothing => f.write_str("Nothing"),
            Self::Note(note) => f.debug_tuple("Note").field(note).finish(),
            Self::Archive(archive) => f.debug_struct("Archive").field("bytes", &archive.rep().len()).finish(),
        }
    }
}

impl External {
    #[must_use]
    pub fn note_url(&self) -> String {
        match self {
            Self::Note(n) => n.url.clone(),
            Self::Nothing | Self::Archive(_) => String::new(),
        }
    }

    #[must_use]
    pub fn archive_bytes(&self) -> Vec<u8> {
        match self {
            Self::Archive(a) => a.clone().untag(),
            Self::Nothing | Self::Note(_) => Vec::new(),
        }
    }
}

#[derive(Clone)]
pub struct TemporaryState {
    pub note: String,
    pub password: Zeroizing<String>,
    pub cipher: CipherChoice,
    pub attachments: Vec<functora_egui::files::Attachment>,
    pub screen: crate::route::Screen,
    pub action: ActionMode,
    pub url_input: String,
    pub external: External,
    pub progress: Option<Job<Stage>>,
    pub attachment: Option<AttachmentIdx>,
}

/// The note body, the password and the attachment contents are secrets, so
/// they never reach a log or a panic message through the derived `Debug`
/// implementation. Only the attachment names and sizes stay visible.
impl std::fmt::Debug for TemporaryState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let attachments = self
            .attachments
            .iter()
            .map(|attachment| (attachment.name.as_str(), attachment.data.len()))
            .collect::<Vec<_>>();

        f.debug_struct("TemporaryState")
            .field("note", &"[redacted]")
            .field("password", &"[redacted]")
            .field("cipher", &self.cipher)
            .field("attachments", &attachments)
            .field("screen", &self.screen)
            .field("action", &self.action)
            .field("url_input", &self.url_input)
            .field("external", &self.external)
            .field("progress", &self.progress)
            .field("attachment", &self.attachment)
            .finish()
    }
}

impl Default for TemporaryState {
    fn default() -> Self {
        Self {
            note: String::new(),
            password: Zeroizing::new(String::new()),
            cipher: CipherChoice::Aes256Gcm,
            attachments: Vec::new(),
            screen: crate::route::Screen::default(),
            action: ActionMode::Create,
            url_input: String::new(),
            external: External::Nothing,
            progress: None,
            attachment: None,
        }
    }
}

impl TemporaryState {
    pub fn reset(&mut self) {
        *self = Self::default();
    }
}

/// A fully loaded `.cryptonote` archive, computed off the UI thread and
/// applied to `TemporaryState` by the poll arm. Keeps file IO and zip/crypto
/// work out of the frame loop.
#[derive(Clone)]
pub struct OpenedArchive {
    pub screen: crate::route::Screen,
    pub external: External,
    pub note: String,
    pub attachments: Vec<functora_egui::files::Attachment>,
}

/// The note body and the attachment contents are secrets, so they never
/// reach a log or a panic message through the derived `Debug`
/// implementation. Only the attachment names and sizes stay visible.
impl std::fmt::Debug for OpenedArchive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let attachments = self
            .attachments
            .iter()
            .map(|attachment| (attachment.name.as_str(), attachment.data.len()))
            .collect::<Vec<_>>();

        f.debug_struct("OpenedArchive")
            .field("screen", &self.screen)
            .field("external", &self.external)
            .field("note", &"[redacted]")
            .field("attachments", &attachments)
            .finish()
    }
}
