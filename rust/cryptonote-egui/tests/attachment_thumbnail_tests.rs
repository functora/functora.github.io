#![allow(clippy::unwrap_used, clippy::expect_used)]
//! Image preview/thumbnail support for attachments: still images get a
//! clickable thumbnail in attachment lists, other files keep label rows.

use cryptonote_egui::screens::attachment_thumb::{attachment_uri, preview_cache_key, show_attachment_thumb};
use functora_egui::files::{Preview, preview_cached};

/// Minimal valid 1x1 PNG, reused as stand-in bytes for still images.
const PIXEL_PNG: &[u8] = &[
    137, 80, 78, 71, 13, 10, 26, 10, 0, 0, 0, 13, 73, 72, 68, 82, 0, 0, 0, 1, 0, 0, 0, 1, 8, 6, 0, 0, 0, 31, 21, 196,
    137, 0, 0, 0, 10, 73, 68, 65, 84, 120, 156, 99, 0, 1, 0, 0, 5, 0, 1, 13, 10, 45, 180, 0, 0, 0, 0, 73, 69, 78, 68,
    174, 66, 96, 130,
];

struct UiDriver {
    ctx: egui::Context,
    frame: u32,
}

impl UiDriver {
    fn new() -> Self {
        let ctx = egui::Context::default();
        functora_egui::setup_fonts(&ctx);
        functora_egui::setup_image_loaders(&ctx);
        Self { ctx, frame: 0 }
    }

    fn run(&mut self, events: Vec<egui::Event>, body: &mut dyn FnMut(&mut egui::Ui)) {
        self.frame += 1;
        let raw = egui::RawInput {
            screen_rect: Some(egui::Rect::from_min_size(
                egui::Pos2::ZERO,
                egui::Vec2::new(1280.0, 800.0),
            )),
            time: Some(f64::from(self.frame) / 60.0),
            events,
            ..Default::default()
        };
        let mut out = self.ctx.run_ui(raw, |ui| {
            let _ = egui::CentralPanel::default().show(ui, |inner_ui| body(inner_ui));
        });
        out.textures_delta.clear();
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
fn preview_cache_key_covers_content() {
    assert_eq!(
        preview_cache_key("a.png", PIXEL_PNG),
        preview_cache_key("a.png", PIXEL_PNG)
    );
    assert_ne!(
        preview_cache_key("a.png", PIXEL_PNG),
        preview_cache_key("a.png", b"other bytes")
    );
    assert_ne!(
        preview_cache_key("a.png", PIXEL_PNG),
        preview_cache_key("b.png", PIXEL_PNG)
    );
}

#[test]
fn attachment_uri_keeps_bytes_scheme_and_extension() {
    assert_eq!(attachment_uri("photo.png"), "bytes://photo.png");
}

#[test]
fn thumb_renders_for_images_only() {
    let mut driver = UiDriver::new();
    let seen_png = std::cell::Cell::new(false);
    let seen_txt = std::cell::Cell::new(false);
    let mut body = |ui: &mut egui::Ui| {
        seen_png.set(show_attachment_thumb(ui, "photo.png", PIXEL_PNG).is_some());
        seen_txt.set(show_attachment_thumb(ui, "note.txt", b"hello").is_some());
    };
    driver.run(vec![], &mut body);
    driver.run(vec![], &mut body);
    assert!(seen_png.get(), "png attachment must render a thumbnail");
    assert!(!seen_txt.get(), "txt attachment must not render a thumbnail");
}

#[test]
fn thumb_click_reports_clicked() {
    let mut driver = UiDriver::new();
    let center = std::cell::Cell::new(egui::Pos2::ZERO);
    let clicked = std::cell::Cell::new(false);
    let mut body = |ui: &mut egui::Ui| {
        if let Some(resp) = show_attachment_thumb(ui, "photo.png", PIXEL_PNG) {
            center.set(resp.rect.center());
            clicked.set(resp.clicked());
        }
    };
    driver.run(vec![], &mut body);
    let pos = center.get();
    assert_ne!(pos, egui::Pos2::ZERO, "thumbnail must occupy a rect");
    let press = |pressed: bool| egui::Event::PointerButton {
        pos,
        button: egui::PointerButton::Primary,
        pressed,
        modifiers: egui::Modifiers::default(),
    };
    driver.run(vec![egui::Event::PointerMoved(pos)], &mut body);
    driver.run(vec![press(true)], &mut body);
    driver.run(vec![press(false)], &mut body);
    assert!(clicked.get(), "thumbnail click must report clicked");
}
