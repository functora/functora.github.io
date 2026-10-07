#![cfg(feature = "build")]
#![allow(clippy::unwrap_used, clippy::expect_used)]
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
    let root = tempfile::tempdir().expect("tempdir");
    let (desktop, assets) = layout(root.path());
    std::fs::create_dir_all(&assets).expect("seed assets dir");
    for size in [16_u32, 512] {
        std::fs::write(
            assets.join(format!("icon-{size}.png")),
            format!("png-{size}"),
        )
        .expect("seed icon");
    }
    let hits = copy_desktop_icons(&desktop, &assets, APP_ID).expect("copy icons");
    assert_eq!(hits.len(), 2);
    for size in [16_u32, 512] {
        let body = std::fs::read(dest_for(&desktop, size)).expect("read copied icon");
        assert_eq!(body, format!("png-{size}").as_bytes());
    }
}

#[test]
fn plain_icon_png_fans_out_to_large_sizes() {
    let root = tempfile::tempdir().expect("tempdir");
    let (desktop, assets) = layout(root.path());
    std::fs::create_dir_all(&assets).expect("seed assets dir");
    std::fs::write(assets.join("icon.png"), b"plain").expect("seed icon");
    let hits = copy_desktop_icons(&desktop, &assets, APP_ID).expect("copy icons");
    assert_eq!(hits.len(), 3);
    for size in [512_u32, 256, 128] {
        let body = std::fs::read(dest_for(&desktop, size)).expect("read copied icon");
        assert_eq!(body, b"plain");
    }
}

#[test]
fn favicon_mipmaps_are_fallback_sources() {
    let root = tempfile::tempdir().expect("tempdir");
    let (desktop, assets) = layout(root.path());
    let favicon = assets.join("favicon");
    std::fs::create_dir_all(&favicon).expect("seed favicon dir");
    std::fs::write(favicon.join("mipmap-mdpi.png"), b"mdpi").expect("seed mipmap");
    let hits = copy_desktop_icons(&desktop, &assets, APP_ID).expect("copy icons");
    assert_eq!(hits.len(), 1);
    let body = std::fs::read(dest_for(&desktop, 48)).expect("read copied icon");
    assert_eq!(body, b"mdpi");
}

#[test]
fn missing_sources_yield_empty_without_creating_tree() {
    let root = tempfile::tempdir().expect("tempdir");
    let (desktop, assets) = layout(root.path());
    let hits = copy_desktop_icons(&desktop, &assets, APP_ID).expect("copy icons");
    assert!(hits.is_empty());
    assert!(!desktop.join("icons").exists());
}

#[test]
fn skips_missing_sizes_but_copies_present_ones() {
    let root = tempfile::tempdir().expect("tempdir");
    let (desktop, assets) = layout(root.path());
    std::fs::create_dir_all(&assets).expect("seed assets dir");
    std::fs::write(assets.join("icon-32.png"), b"s32").expect("seed icon");
    let hits = copy_desktop_icons(&desktop, &assets, APP_ID).expect("copy icons");
    assert_eq!(hits.len(), 1);
    assert!(!dest_for(&desktop, 16).exists());
    let body = std::fs::read(dest_for(&desktop, 32)).expect("read copied icon");
    assert_eq!(body, b"s32");
}
