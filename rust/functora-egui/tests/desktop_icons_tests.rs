#![cfg(feature = "build")]
use functora_egui::desktop::icons::copy_desktop_icons;
use std::path::{Path, PathBuf};

const APP_ID: &str = "io.functora.app";

fn dest_for(desktop: &Path, size: u32) -> PathBuf {
    desktop.join(format!("icons/hicolor/{size}x{size}/apps/{APP_ID}.png"))
}

fn layout(root: &Path) -> (PathBuf, PathBuf) {
    (root.join("desktop"), root.join("assets"))
}

#[test]
fn copies_sized_icons_with_exact_bytes() {
    let root = tempfile::tempdir().unwrap_or_else(|err| panic!("tempdir: {err}"));
    let (desktop, assets) = layout(root.path());
    std::fs::create_dir_all(&assets).unwrap_or_else(|err| panic!("seed assets dir: {err}"));
    for size in [16_u32, 512] {
        std::fs::write(
            assets.join(format!("icon-{size}.png")),
            format!("png-{size}"),
        )
        .unwrap_or_else(|err| panic!("seed icon: {err}"));
    }
    let hits = copy_desktop_icons(&desktop, &assets, APP_ID)
        .unwrap_or_else(|err| panic!("copy icons: {err}"));
    assert_eq!(hits.len(), 2);
    for size in [16_u32, 512] {
        let body = std::fs::read(dest_for(&desktop, size))
            .unwrap_or_else(|err| panic!("read copied icon: {err}"));
        assert_eq!(body, format!("png-{size}").as_bytes());
    }
}

#[test]
fn plain_icon_png_fans_out_to_large_sizes() {
    let root = tempfile::tempdir().unwrap_or_else(|err| panic!("tempdir: {err}"));
    let (desktop, assets) = layout(root.path());
    std::fs::create_dir_all(&assets).unwrap_or_else(|err| panic!("seed assets dir: {err}"));
    std::fs::write(assets.join("icon.png"), b"plain")
        .unwrap_or_else(|err| panic!("seed icon: {err}"));
    let hits = copy_desktop_icons(&desktop, &assets, APP_ID)
        .unwrap_or_else(|err| panic!("copy icons: {err}"));
    assert_eq!(hits.len(), 3);
    for size in [512_u32, 256, 128] {
        let body = std::fs::read(dest_for(&desktop, size))
            .unwrap_or_else(|err| panic!("read copied icon: {err}"));
        assert_eq!(body, b"plain");
    }
}

#[test]
fn favicon_mipmaps_are_fallback_sources() {
    let root = tempfile::tempdir().unwrap_or_else(|err| panic!("tempdir: {err}"));
    let (desktop, assets) = layout(root.path());
    let favicon = assets.join("favicon");
    std::fs::create_dir_all(&favicon).unwrap_or_else(|err| panic!("seed favicon dir: {err}"));
    std::fs::write(favicon.join("mipmap-mdpi.png"), b"mdpi")
        .unwrap_or_else(|err| panic!("seed mipmap: {err}"));
    let hits = copy_desktop_icons(&desktop, &assets, APP_ID)
        .unwrap_or_else(|err| panic!("copy icons: {err}"));
    assert_eq!(hits.len(), 1);
    let body = std::fs::read(dest_for(&desktop, 48))
        .unwrap_or_else(|err| panic!("read copied icon: {err}"));
    assert_eq!(body, b"mdpi");
}

#[test]
fn missing_sources_yield_empty_without_creating_tree() {
    let root = tempfile::tempdir().unwrap_or_else(|err| panic!("tempdir: {err}"));
    let (desktop, assets) = layout(root.path());
    let hits = copy_desktop_icons(&desktop, &assets, APP_ID)
        .unwrap_or_else(|err| panic!("copy icons: {err}"));
    assert!(hits.is_empty());
    assert!(!desktop.join("icons").exists());
}

#[test]
fn skips_missing_sizes_but_copies_present_ones() {
    let root = tempfile::tempdir().unwrap_or_else(|err| panic!("tempdir: {err}"));
    let (desktop, assets) = layout(root.path());
    std::fs::create_dir_all(&assets).unwrap_or_else(|err| panic!("seed assets dir: {err}"));
    std::fs::write(assets.join("icon-32.png"), b"s32")
        .unwrap_or_else(|err| panic!("seed icon: {err}"));
    let hits = copy_desktop_icons(&desktop, &assets, APP_ID)
        .unwrap_or_else(|err| panic!("copy icons: {err}"));
    assert_eq!(hits.len(), 1);
    assert!(!dest_for(&desktop, 16).exists());
    let body = std::fs::read(dest_for(&desktop, 32))
        .unwrap_or_else(|err| panic!("read copied icon: {err}"));
    assert_eq!(body, b"s32");
}

#[test]
fn desktop_icons_error_messages_keep_their_desktop_label() {
    use functora_egui::icon_export::{IconSet, IconsError};

    let create = IconsError::CreateDir {
        set: IconSet::Desktop,
        dir: PathBuf::from("/tmp/desktop-icons"),
        source: std::io::Error::other("boom"),
    };
    assert_eq!(
        create.to_string(),
        "cannot create desktop icon directory /tmp/desktop-icons"
    );

    let read = IconsError::ReadSource {
        set: IconSet::Desktop,
        path: PathBuf::from("/tmp/desktop-icons/icon-32.png"),
        source: std::io::Error::other("boom"),
    };
    assert_eq!(
        read.to_string(),
        "cannot read desktop icon source /tmp/desktop-icons/icon-32.png"
    );

    let write = IconsError::WriteDest {
        set: IconSet::Desktop,
        path: PathBuf::from("/tmp/desktop-icons/icon-32.png"),
        source: std::io::Error::other("boom"),
    };
    assert_eq!(
        write.to_string(),
        "cannot write desktop icon /tmp/desktop-icons/icon-32.png"
    );
}
