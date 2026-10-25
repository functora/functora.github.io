//! Image preview/thumbnail support for attachments: still images get a
//! clickable thumbnail in attachment lists, other files keep label rows.

use cryptonote_egui::screens::attachment_thumb::{attachment_uri, show_attachment_thumb};
use functora_egui::files::{Attachment, Preview, preview_cached};

pub mod common;
use common::UiDriver;

/// Minimal valid 1x1 PNG, reused as stand-in bytes for still images.
const PIXEL_PNG: &[u8] = &[
    137, 80, 78, 71, 13, 10, 26, 10, 0, 0, 0, 13, 73, 72, 68, 82, 0, 0, 0, 1, 0, 0, 0, 1, 8, 6, 0, 0, 0, 31, 21, 196,
    137, 0, 0, 0, 10, 73, 68, 65, 84, 120, 156, 99, 0, 1, 0, 0, 5, 0, 1, 13, 10, 45, 180, 0, 0, 0, 0, 73, 69, 78, 68,
    174, 66, 96, 130,
];

fn attachment(name: &str, data: &[u8]) -> Attachment {
    Attachment {
        name: name.to_string(),
        data: data.to_vec().into(),
    }
}

#[test]
fn still_images_detect_image_preview() {
    for name in [
        "photo.png",
        "photo.jpg",
        "photo.jpeg",
        "anim.gif",
        "photo.webp",
        "photo.bmp",
        "icon.ico",
    ] {
        assert!(
            matches!(preview_cached(name, PIXEL_PNG), Preview::Image(_)),
            "{name} must detect an image preview"
        );
    }
}

#[test]
fn non_images_have_no_image_preview() {
    assert!(matches!(preview_cached("note.txt", b"hello"), Preview::Text(_)));
    assert!(matches!(preview_cached("archive.xyz", b"hello"), Preview::Download));
}

#[test]
fn attachment_uri_keeps_bytes_scheme_and_extension() {
    assert_eq!(attachment_uri("photo.png"), "bytes://photo.png");
}

#[test]
fn thumb_renders_for_images_only() {
    let png = attachment("photo.png", PIXEL_PNG);
    let txt = attachment("note.txt", b"hello");
    let mut driver = UiDriver::with_fonts();
    let seen_png = std::cell::Cell::new(false);
    let seen_txt = std::cell::Cell::new(false);
    let mut body = |ui: &mut egui::Ui| {
        seen_png.set(show_attachment_thumb(ui, &png).is_some());
        seen_txt.set(show_attachment_thumb(ui, &txt).is_some());
    };
    driver.show(vec![], &mut body);
    driver.show(vec![], &mut body);
    assert!(seen_png.get(), "png attachment must render a thumbnail");
    assert!(!seen_txt.get(), "txt attachment must not render a thumbnail");
}

#[test]
fn thumb_click_reports_clicked() {
    let png = attachment("photo.png", PIXEL_PNG);
    let mut driver = UiDriver::with_fonts();
    let center = std::cell::Cell::new(egui::Pos2::ZERO);
    let clicked = std::cell::Cell::new(false);
    let mut body = |ui: &mut egui::Ui| {
        if let Some(resp) = show_attachment_thumb(ui, &png) {
            center.set(resp.rect.center());
            clicked.set(resp.clicked());
        }
    };
    driver.show(vec![], &mut body);
    let pos = center.get();
    assert_ne!(pos, egui::Pos2::ZERO, "thumbnail must occupy a rect");
    let press = |pressed: bool| egui::Event::PointerButton {
        pos,
        button: egui::PointerButton::Primary,
        pressed,
        modifiers: egui::Modifiers::default(),
    };
    driver.show(vec![egui::Event::PointerMoved(pos)], &mut body);
    driver.show(vec![press(true)], &mut body);
    driver.show(vec![press(false)], &mut body);
    assert!(clicked.get(), "thumbnail click must report clicked");
}
