#![cfg(feature = "build")]
#![allow(clippy::unwrap_used, clippy::expect_used)]
use functora_egui::android::icons::copy_launcher_icons;
use std::path::{Path, PathBuf};

const DENSITIES: [&str; 5] = ["mdpi", "hdpi", "xhdpi", "xxhdpi", "xxxhdpi"];

fn seed_favicon(favicon: &Path, density: &str, png: &[u8]) {
    std::fs::create_dir_all(favicon).expect("seed favicon dir");
    let path = favicon.join(format!("mipmap-{density}.png"));
    std::fs::write(&path, png).expect("seed favicon png");
}

fn dest_for(android: &Path, density: &str) -> PathBuf {
    android.join(format!("app/src/main/res/mipmap-{density}/ic_launcher.png"))
}

fn layout(root: &Path) -> (PathBuf, PathBuf) {
    (root.join("android"), root.join("assets/favicon"))
}

#[test]
fn copies_all_densities_with_exact_bytes() {
    let root = tempfile::tempdir().expect("tempdir");
    let (android, favicon) = layout(root.path());
    for density in DENSITIES {
        seed_favicon(&favicon, density, format!("png-{density}").as_bytes());
    }
    let hits = copy_launcher_icons(&android, &favicon).expect("copy icons");
    assert_eq!(hits.len(), DENSITIES.len());
    for density in DENSITIES {
        let dest = dest_for(&android, density);
        assert!(
            hits.contains(&dest),
            "copied paths must include {dest:?}, got {hits:?}"
        );
        let body = std::fs::read(&dest).expect("read copied icon");
        assert_eq!(body, format!("png-{density}").as_bytes());
    }
}

#[test]
fn skips_missing_sources_without_error() {
    let root = tempfile::tempdir().expect("tempdir");
    let (android, favicon) = layout(root.path());
    for density in ["mdpi", "xxxhdpi"] {
        seed_favicon(&favicon, density, format!("png-{density}").as_bytes());
    }
    let hits = copy_launcher_icons(&android, &favicon).expect("copy icons");
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
    let root = tempfile::tempdir().expect("tempdir");
    let (android, favicon) = layout(root.path());
    let hits = copy_launcher_icons(&android, &favicon).expect("copy icons");
    assert!(hits.is_empty());
    assert!(
        !android.exists(),
        "empty sources must not create the android tree"
    );
}

#[test]
fn repairs_stale_dest_on_rerun() {
    let root = tempfile::tempdir().expect("tempdir");
    let (android, favicon) = layout(root.path());
    for density in DENSITIES {
        seed_favicon(&favicon, density, format!("png-{density}").as_bytes());
    }
    let _ = copy_launcher_icons(&android, &favicon).expect("initial copy");
    let stale = dest_for(&android, "xhdpi");
    std::fs::write(&stale, b"stale-bytes").expect("stale dest");
    let hits = copy_launcher_icons(&android, &favicon).expect("repair copy");
    assert_eq!(hits.len(), DENSITIES.len());
    let body = std::fs::read(&stale).expect("read repaired icon");
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
