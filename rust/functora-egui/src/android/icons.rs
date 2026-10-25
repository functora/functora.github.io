use std::path::{Path, PathBuf};

pub use crate::icon_export::IconsError;
use crate::icon_export::{IconSet, ensure_dest};

const DENSITIES: [&str; 5] = ["mdpi", "hdpi", "xhdpi", "xxhdpi", "xxxhdpi"];

fn copy_density(
    android_dir: &Path,
    favicon_dir: &Path,
    density: &str,
) -> Result<Option<PathBuf>, IconsError> {
    let from = favicon_dir.join(format!("mipmap-{density}.png"));
    let to = android_dir.join(format!("app/src/main/res/mipmap-{density}/ic_launcher.png"));
    match std::fs::read(&from) {
        Err(source) if source.kind() == std::io::ErrorKind::NotFound => Ok(None),
        Err(source) => Err(IconsError::ReadSource {
            set: IconSet::Launcher,
            path: from,
            source,
        }),
        Ok(png) => ensure_dest(IconSet::Launcher, to, &png).map(Some),
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
