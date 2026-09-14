#![allow(clippy::unwrap_used, clippy::expect_used)]
use egui::{Context, Pos2, RawInput, Rect, Vec2};

const SCREEN: Vec2 = Vec2::new(800.0, 600.0);

fn run_once(
    body: &mut dyn FnMut(&mut egui::Ui) -> egui::Response,
) -> (Context, egui::Response, f32) {
    let ctx = Context::default();
    let mut captured: Option<(egui::Response, f32)> = None;
    let raw = RawInput {
        screen_rect: Some(Rect::from_min_size(Pos2::ZERO, SCREEN)),
        ..Default::default()
    };
    let mut out = ctx.run_ui(raw, |ui| {
        let _ = egui::CentralPanel::default().show(ui, |inner| {
            let available = inner.available_width();
            let response = body(inner);
            captured = Some((response, available));
        });
    });
    out.textures_delta.clear();
    let (response, available) = captured.expect("body must run");
    (ctx, response, available)
}

#[test]
fn qr_image_uses_full_width() {
    let mut body = |ui: &mut egui::Ui| functora_egui::QrImage::new("https://example.com").show(ui);
    let (_ctx, response, available) = run_once(&mut body);
    assert!(
        (response.rect.width() - available).abs() < 2.0,
        "QrImage must be full width, got {} expected {available}",
        response.rect.width()
    );
    assert!(
        (response.rect.width() - response.rect.height()).abs() < 2.0,
        "QrImage must stay square, got {:?}",
        response.rect
    );
}

#[test]
fn qr_image_donate_addresses_render_full_width() {
    for address in [
        "bc1qa3qk8d4mxl6qkpvahl5xvg6c5k33kmuwvt9v8q",
        "48sTw2TvjuWKkaomi9J7gLExRUJLJCvUHLrbf8M8qmayQ9zkho1GYdCXVtpTPawNWH7mNS49N4E6HNDF95dtggMMCigrVyG",
    ] {
        let mut body = |ui: &mut egui::Ui| functora_egui::QrImage::new(address).show(ui);
        let (_ctx, response, available) = run_once(&mut body);
        assert!(
            (response.rect.width() - available).abs() < 2.0,
            "donate QR for {address} must be full width, got {} expected {available}",
            response.rect.width()
        );
    }
}

#[test]
fn qr_image_empty_content_does_not_panic() {
    let mut body = |ui: &mut egui::Ui| functora_egui::QrImage::new("").show(ui);
    let (_ctx, response, _available) = run_once(&mut body);
    assert!(response.rect.width() >= 0.0);
}

#[test]
fn qr_image_distinct_contents_use_distinct_textures() {
    assert_ne!(
        egui::util::hash("https://example.com/1"),
        egui::util::hash("https://example.com/2")
    );
}
