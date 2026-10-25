#![cfg(feature = "build")]
use functora_egui::android::icons::copy_launcher_icons;
use std::path::{Path, PathBuf};

const DENSITIES: [&str; 5] = ["mdpi", "hdpi", "xhdpi", "xxhdpi", "xxxhdpi"];

fn seed_favicon(favicon: &Path, density: &str, png: &[u8]) {
    std::fs::create_dir_all(favicon).unwrap_or_else(|err| panic!("seed favicon dir: {err}"));
    let path = favicon.join(format!("mipmap-{density}.png"));
    std::fs::write(&path, png).unwrap_or_else(|err| panic!("seed favicon png: {err}"));
}

fn dest_for(android: &Path, density: &str) -> PathBuf {
    android.join(format!("app/src/main/res/mipmap-{density}/ic_launcher.png"))
}

fn layout(root: &Path) -> (PathBuf, PathBuf) {
    (root.join("android"), root.join("assets/favicon"))
}

#[test]
fn copies_all_densities_with_exact_bytes() {
    let root = tempfile::tempdir().unwrap_or_else(|err| panic!("tempdir: {err}"));
    let (android, favicon) = layout(root.path());
    for density in DENSITIES {
        seed_favicon(&favicon, density, format!("png-{density}").as_bytes());
    }
    let hits =
        copy_launcher_icons(&android, &favicon).unwrap_or_else(|err| panic!("copy icons: {err}"));
    assert_eq!(hits.len(), DENSITIES.len());
    for density in DENSITIES {
        let dest = dest_for(&android, density);
        assert!(
            hits.contains(&dest),
            "copied paths must include {dest:?}, got {hits:?}"
        );
        let body = std::fs::read(&dest).unwrap_or_else(|err| panic!("read copied icon: {err}"));
        assert_eq!(body, format!("png-{density}").as_bytes());
    }
}

#[test]
fn skips_missing_sources_without_error() {
    let root = tempfile::tempdir().unwrap_or_else(|err| panic!("tempdir: {err}"));
    let (android, favicon) = layout(root.path());
    for density in ["mdpi", "xxxhdpi"] {
        seed_favicon(&favicon, density, format!("png-{density}").as_bytes());
    }
    let hits =
        copy_launcher_icons(&android, &favicon).unwrap_or_else(|err| panic!("copy icons: {err}"));
    assert_eq!(hits.len(), 2);
    for density in ["hdpi", "xhdpi", "xxhdpi"] {
        assert!(
            !dest_for(&android, density).exists(),
            "missing source must leave no dest for {density}"
        );
    }
}

#[test]
fn missing_favicon_dir_yields_empty_without_creating_tree() {
    let root = tempfile::tempdir().unwrap_or_else(|err| panic!("tempdir: {err}"));
    let (android, favicon) = layout(root.path());
    let hits =
        copy_launcher_icons(&android, &favicon).unwrap_or_else(|err| panic!("copy icons: {err}"));
    assert!(hits.is_empty());
    assert!(
        !android.exists(),
        "empty sources must not create the android tree"
    );
}

#[test]
fn repairs_stale_dest_on_rerun() {
    let root = tempfile::tempdir().unwrap_or_else(|err| panic!("tempdir: {err}"));
    let (android, favicon) = layout(root.path());
    for density in DENSITIES {
        seed_favicon(&favicon, density, format!("png-{density}").as_bytes());
    }
    let _ =
        copy_launcher_icons(&android, &favicon).unwrap_or_else(|err| panic!("initial copy: {err}"));
    let stale = dest_for(&android, "xhdpi");
    std::fs::write(&stale, b"stale-bytes").unwrap_or_else(|err| panic!("stale dest: {err}"));
    let hits =
        copy_launcher_icons(&android, &favicon).unwrap_or_else(|err| panic!("repair copy: {err}"));
    assert_eq!(hits.len(), DENSITIES.len());
    let body = std::fs::read(&stale).unwrap_or_else(|err| panic!("read repaired icon: {err}"));
    assert_eq!(body, b"png-xhdpi");
}

#[test]
fn manifest_template_references_launcher_icon() {
    let manifest = include_str!("../templates/android/app/src/main/AndroidManifest.xml");
    assert!(
        manifest.contains("@mipmap/ic_launcher"),
        "manifest template must reference the launcher icon build.rs copies"
    );
}

#[test]
fn launcher_icons_error_messages_keep_their_launcher_label() {
    use functora_egui::icon_export::{IconSet, IconsError};

    let create = IconsError::CreateDir {
        set: IconSet::Launcher,
        dir: PathBuf::from("/tmp/launcher-icons"),
        source: std::io::Error::other("boom"),
    };
    assert_eq!(
        create.to_string(),
        "cannot create launcher icon directory /tmp/launcher-icons"
    );

    let read = IconsError::ReadSource {
        set: IconSet::Launcher,
        path: PathBuf::from("/tmp/launcher-icons/ic_launcher.png"),
        source: std::io::Error::other("boom"),
    };
    assert_eq!(
        read.to_string(),
        "cannot read launcher icon source /tmp/launcher-icons/ic_launcher.png"
    );

    let write = IconsError::WriteDest {
        set: IconSet::Launcher,
        path: PathBuf::from("/tmp/launcher-icons/ic_launcher.png"),
        source: std::io::Error::other("boom"),
    };
    assert_eq!(
        write.to_string(),
        "cannot write launcher icon /tmp/launcher-icons/ic_launcher.png"
    );
}
