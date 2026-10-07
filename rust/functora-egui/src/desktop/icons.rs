use std::path::{Path, PathBuf};

const HICOLOR_SIZES: [u32; 7] = [16, 32, 48, 64, 128, 256, 512];

const MIPMAP_FALLBACK: [(&str, u32); 5] = [
    ("mipmap-mdpi.png", 48),
    ("mipmap-hdpi.png", 72),
    ("mipmap-xhdpi.png", 96),
    ("mipmap-xxhdpi.png", 144),
    ("mipmap-xxxhdpi.png", 192),
];

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
                write!(f, "cannot create desktop icon directory {}", dir.display())
            }
            Self::ReadSource { path, .. } => {
                write!(f, "cannot read desktop icon source {}", path.display())
            }
            Self::WriteDest { path, .. } => {
                write!(f, "cannot write desktop icon {}", path.display())
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

fn dest_for(desktop_dir: &Path, app_id: &str, size: u32) -> PathBuf {
    desktop_dir.join(format!("icons/hicolor/{size}x{size}/apps/{app_id}.png"))
}

fn ensure_dest(to: PathBuf, png: &[u8]) -> Result<PathBuf, IconsError> {
    let dir = to
        .parent()
        .map_or_else(|| PathBuf::from("."), Path::to_path_buf);
    std::fs::create_dir_all(&dir).map_err(|source| IconsError::CreateDir { dir, source })?;
    let fresh = std::fs::read(&to).ok().as_deref() != Some(png);
    if fresh {
        std::fs::write(&to, png).map_err(|source| IconsError::WriteDest {
            path: to.clone(),
            source,
        })?;
    }
    Ok(to)
}

fn copy_size(
    desktop_dir: &Path,
    assets_dir: &Path,
    app_id: &str,
    size: u32,
) -> Result<Option<PathBuf>, IconsError> {
    let sized = assets_dir.join(format!("icon-{size}.png"));
    match std::fs::read(&sized) {
        Ok(png) => ensure_dest(dest_for(desktop_dir, app_id, size), &png).map(Some),
        Err(source) if source.kind() == std::io::ErrorKind::NotFound => Ok(None),
        Err(source) => Err(IconsError::ReadSource {
            path: sized,
            source,
        }),
    }
}

fn copy_mipmap(
    desktop_dir: &Path,
    favicon_dir: &Path,
    app_id: &str,
    file: &str,
    size: u32,
) -> Result<Option<PathBuf>, IconsError> {
    let from = favicon_dir.join(file);
    match std::fs::read(&from) {
        Ok(png) => ensure_dest(dest_for(desktop_dir, app_id, size), &png).map(Some),
        Err(source) if source.kind() == std::io::ErrorKind::NotFound => Ok(None),
        Err(source) => Err(IconsError::ReadSource { path: from, source }),
    }
}

pub fn copy_desktop_icons(
    desktop_dir: &Path,
    assets_dir: &Path,
    app_id: &str,
) -> Result<Vec<PathBuf>, IconsError> {
    let sized = HICOLOR_SIZES
        .into_iter()
        .map(|size| copy_size(desktop_dir, assets_dir, app_id, size))
        .collect::<Result<Vec<Option<PathBuf>>, IconsError>>()?
        .into_iter()
        .flatten()
        .collect::<Vec<PathBuf>>();
    if sized.is_empty() {
        match std::fs::read(assets_dir.join("icon.png")) {
            Ok(png) => [512_u32, 256, 128]
                .into_iter()
                .map(|size| ensure_dest(dest_for(desktop_dir, app_id, size), &png))
                .collect::<Result<Vec<PathBuf>, IconsError>>(),
            Err(source) if source.kind() == std::io::ErrorKind::NotFound => MIPMAP_FALLBACK
                .into_iter()
                .map(|(file, size)| {
                    copy_mipmap(desktop_dir, &assets_dir.join("favicon"), app_id, file, size)
                })
                .collect::<Result<Vec<Option<PathBuf>>, IconsError>>()
                .map(|hits| hits.into_iter().flatten().collect()),
            Err(source) => Err(IconsError::ReadSource {
                path: assets_dir.join("icon.png"),
                source,
            }),
        }
    } else {
        Ok(sized)
    }
}
