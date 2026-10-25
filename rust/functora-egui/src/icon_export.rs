use std::path::{Path, PathBuf};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IconSet {
    Launcher,
    Desktop,
}

impl std::fmt::Display for IconSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Launcher => write!(f, "launcher"),
            Self::Desktop => write!(f, "desktop"),
        }
    }
}

#[derive(Debug)]
pub enum IconsError {
    CreateDir {
        set: IconSet,
        dir: PathBuf,
        source: std::io::Error,
    },
    ReadSource {
        set: IconSet,
        path: PathBuf,
        source: std::io::Error,
    },
    WriteDest {
        set: IconSet,
        path: PathBuf,
        source: std::io::Error,
    },
}

impl std::fmt::Display for IconsError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CreateDir { set, dir, .. } => {
                write!(f, "cannot create {set} icon directory {}", dir.display())
            }
            Self::ReadSource { set, path, .. } => {
                write!(f, "cannot read {set} icon source {}", path.display())
            }
            Self::WriteDest { set, path, .. } => {
                write!(f, "cannot write {set} icon {}", path.display())
            }
        }
    }
}

impl std::error::Error for IconsError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::CreateDir { source, .. }
            | Self::ReadSource { source, .. }
            | Self::WriteDest { source, .. } => Some(source),
        }
    }
}

pub(crate) fn ensure_dest(set: IconSet, to: PathBuf, png: &[u8]) -> Result<PathBuf, IconsError> {
    let dir = to
        .parent()
        .map_or_else(|| PathBuf::from("."), Path::to_path_buf);
    std::fs::create_dir_all(&dir).map_err(|source| IconsError::CreateDir { set, dir, source })?;
    let fresh = std::fs::read(&to).ok().as_deref() != Some(png);
    if fresh {
        std::fs::write(&to, png).map_err(|source| IconsError::WriteDest {
            set,
            path: to.clone(),
            source,
        })?;
    }
    Ok(to)
}
