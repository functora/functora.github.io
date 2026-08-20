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

impl ActionMode {
    pub const ALL: [Self; 3] = [Self::Create, Self::Open, Self::Scan];

    #[must_use]
    pub const fn index(self) -> usize {
        match self {
            Self::Create => 0,
            Self::Open => 1,
            Self::Scan => 2,
        }
    }

    #[must_use]
    pub fn from_index(index: usize) -> Self {
        Self::ALL.get(index).copied().unwrap_or_default()
    }
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
