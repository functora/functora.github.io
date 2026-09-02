use crate::crypto::{CipherType, ExternalArchive};
use crate::encoding::NoteData;
use crate::progress::{Job, Stage};

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

    #[must_use]
    pub fn as_usize(self) -> usize {
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

#[derive(Debug, Clone)]
pub struct ExternalNote {
    pub data: NoteData,
    pub url: String,
    pub qr: String,
}

#[derive(Debug, Clone, Default)]
pub enum External {
    #[default]
    Nothing,
    Note(ExternalNote),
    Archive(ExternalArchive),
}

impl External {
    #[must_use]
    pub fn note_url(self) -> String {
        match self {
            Self::Note(n) => n.url,
            _ => String::new(),
        }
    }

    #[must_use]
    pub fn archive_bytes(self) -> Vec<u8> {
        match self {
            Self::Archive(a) => a.untag(),
            _ => Vec::new(),
        }
    }

    #[must_use]
    pub fn is_nothing(&self) -> bool {
        matches!(self, Self::Nothing)
    }
}

#[derive(Debug, Clone)]
pub struct TemporaryState {
    pub note: String,
    pub password: String,
    pub cipher: Option<CipherType>,
    pub attachments: Vec<functora_egui::files::Attachment>,
    pub screen: crate::route::Screen,
    pub action: ActionMode,
    pub url_input: String,
    pub external: External,
    pub progress: Option<Job<Stage>>,
    pub attachment: Option<AttachmentIdx>,
    pub message: Option<crate::messages::Msg>,
}

impl Default for TemporaryState {
    fn default() -> Self {
        Self {
            note: String::new(),
            password: String::new(),
            cipher: Some(CipherType::Aes256Gcm),
            attachments: Vec::new(),
            screen: crate::route::Screen::default(),
            action: ActionMode::Create,
            url_input: String::new(),
            external: External::Nothing,
            progress: None,
            attachment: None,
            message: None,
        }
    }
}

impl TemporaryState {
    pub fn reset(&mut self) {
        *self = Self::default();
    }
}
