use crate::encoding::NoteData;
use functora_tagged::{FCrude, Tagged};

#[derive(Debug, Clone)]
pub struct ExternalNote {
    pub data: NoteData,
    pub url: String,
    pub qr: String,
}

#[derive(Debug)]
pub enum DExternalArchive {}

pub type ExternalArchive = Tagged<Vec<u8>, DExternalArchive, FCrude>;

#[derive(Debug, Clone, Default)]
pub enum External {
    #[default]
    Nothing,
    Note(ExternalNote),
    Archive(ExternalArchive),
}

impl External {
    #[must_use]
    pub fn note_url(&self) -> String {
        match self {
            Self::Note(n) => n.url.clone(),
            _ => String::new(),
        }
    }

    #[must_use]
    pub fn archive_bytes(&self) -> Option<Vec<u8>> {
        match self {
            Self::Archive(a) => Some(a.clone().untag()),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ActionMode {
    #[default]
    Create,
    Open,
    Scan,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PasteTarget {
    Note,
    Url,
    Password,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PickKind {
    Attach,
    OpenArchive,
    Scan,
}
