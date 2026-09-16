use std::path::{Path, PathBuf};

const DENSITIES: [&str; 5] = ["mdpi", "hdpi", "xhdpi", "xxhdpi", "xxxhdpi"];

#[derive(Debug)]
pub enum IconsError {
    CreateDir {
        dir: PathBuf,
        source: std::io::Error,
    },
    ReadSource {
        path: PathBuf,
        source: std::io::Error,
    },
    WriteDest {
        path: PathBuf,
        source: std::io::Error,
    },
}

impl std::fmt::Display for IconsError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CreateDir { dir, .. } => {
                write!(f, "cannot create launcher icon directory {}", dir.display())
            }
            Self::ReadSource { path, .. } => {
                write!(f, "cannot read launcher icon source {}", path.display())
            }
            Self::WriteDest { path, .. } => {
                write!(f, "cannot write launcher icon {}", path.display())
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

fn ensure_dest(to: PathBuf, png: &[u8]) -> Result<PathBuf, IconsError> {
    let dir = to
        .parent()
        .map_or_else(|| PathBuf::from("."), Path::to_path_buf);
    std::fs::create_dir_all(&dir).map_err(|source| IconsError::CreateDir { dir, source })?;
    let current = std::fs::read(&to).ok();
    let fresh = current.as_deref() != Some(png);
    if fresh {
        std::fs::write(&to, png).map_err(|source| IconsError::WriteDest {
            path: to.clone(),
            source,
        })?;
    }
    Ok(to)
}

fn copy_density(
    android_dir: &Path,
    favicon_dir: &Path,
    density: &str,
) -> Result<Option<PathBuf>, IconsError> {
    let from = favicon_dir.join(format!("mipmap-{density}.png"));
    let to = android_dir.join(format!("app/src/main/res/mipmap-{density}/ic_launcher.png"));
    match std::fs::read(&from) {
        Err(source) if source.kind() == std::io::ErrorKind::NotFound => Ok(None),
        Err(source) => Err(IconsError::ReadSource { path: from, source }),
        Ok(png) => ensure_dest(to, &png).map(Some),
    }
}

pub fn copy_launcher_icons(
    android_dir: &Path,
    favicon_dir: &Path,
) -> Result<Vec<PathBuf>, IconsError> {
    DENSITIES
        .into_iter()
        .map(|density| copy_density(android_dir, favicon_dir, density))
        .collect::<Result<Vec<Option<PathBuf>>, IconsError>>()
        .map(|hits| hits.into_iter().flatten().collect())
}
