use std::path::{Path, PathBuf};

pub use crate::icon_export::IconsError;
use crate::icon_export::{IconSet, ensure_dest};

const HICOLOR_SIZES: [u32; 7] = [16, 32, 48, 64, 128, 256, 512];

const MIPMAP_FALLBACK: [(&str, u32); 5] = [
    ("mipmap-mdpi.png", 48),
    ("mipmap-hdpi.png", 72),
    ("mipmap-xhdpi.png", 96),
    ("mipmap-xxhdpi.png", 144),
    ("mipmap-xxxhdpi.png", 192),
];

fn dest_for(desktop_dir: &Path, app_id: &str, size: u32) -> PathBuf {
    desktop_dir.join(format!("icons/hicolor/{size}x{size}/apps/{app_id}.png"))
}

fn copy_size(
    desktop_dir: &Path,
    assets_dir: &Path,
    app_id: &str,
    size: u32,
) -> Result<Option<PathBuf>, IconsError> {
    let sized = assets_dir.join(format!("icon-{size}.png"));
    match std::fs::read(&sized) {
        Ok(png) => {
            ensure_dest(IconSet::Desktop, dest_for(desktop_dir, app_id, size), &png).map(Some)
        }
        Err(source) if source.kind() == std::io::ErrorKind::NotFound => Ok(None),
        Err(source) => Err(IconsError::ReadSource {
            set: IconSet::Desktop,
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
        Ok(png) => {
            ensure_dest(IconSet::Desktop, dest_for(desktop_dir, app_id, size), &png).map(Some)
        }
        Err(source) if source.kind() == std::io::ErrorKind::NotFound => Ok(None),
        Err(source) => Err(IconsError::ReadSource {
            set: IconSet::Desktop,
            path: from,
            source,
        }),
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
                .map(|size| {
                    ensure_dest(IconSet::Desktop, dest_for(desktop_dir, app_id, size), &png)
                })
                .collect::<Result<Vec<PathBuf>, IconsError>>(),
            Err(source) if source.kind() == std::io::ErrorKind::NotFound => MIPMAP_FALLBACK
                .into_iter()
                .map(|(file, size)| {
                    copy_mipmap(desktop_dir, &assets_dir.join("favicon"), app_id, file, size)
                })
                .collect::<Result<Vec<Option<PathBuf>>, IconsError>>()
                .map(|hits| hits.into_iter().flatten().collect()),
            Err(source) => Err(IconsError::ReadSource {
                set: IconSet::Desktop,
                path: assets_dir.join("icon.png"),
                source,
            }),
        }
    } else {
        Ok(sized)
    }
}
