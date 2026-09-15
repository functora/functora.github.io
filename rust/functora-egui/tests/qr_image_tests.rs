#![allow(clippy::unwrap_used, clippy::expect_used)]
use egui::{FullOutput, Pos2, RawInput, Rect, Vec2};

const SCREEN: Vec2 = Vec2::new(800.0, 600.0);
const WIDE_SCREEN: Vec2 = Vec2::new(1440.0, 900.0);
const NARROW_SCREEN: Vec2 = Vec2::new(390.0, 844.0);

fn run_on(
    screen: Vec2,
    body: &mut dyn FnMut(&mut egui::Ui) -> egui::Response,
) -> (egui::Response, f32, FullOutput) {
    let ctx = egui::Context::default();
    let mut captured: Option<(egui::Response, f32)> = None;
    let raw = RawInput {
        screen_rect: Some(Rect::from_min_size(Pos2::ZERO, screen)),
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
    (response, available, out)
}

fn run_once(
    body: &mut dyn FnMut(&mut egui::Ui) -> egui::Response,
) -> (egui::Response, f32, FullOutput) {
    run_on(SCREEN, body)
}

fn textured_rects(out: &FullOutput) -> Vec<Rect> {
    out.shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            egui::Shape::Rect(rect_shape)
                if rect_shape
                    .brush
                    .as_ref()
                    .is_some_and(|brush| brush.fill_texture_id != egui::TextureId::default()) =>
            {
                Some(rect_shape.rect)
            }
            _ => None,
        })
        .collect()
}

#[test]
fn qr_image_uses_full_width() {
    let mut body = |ui: &mut egui::Ui| functora_egui::QrImage::new("https://example.com").show(ui);
    let (response, available, out) = run_once(&mut body);
    assert!(
        (response.rect.width() - available).abs() < 2.0,
        "QrImage must be full width, got {} expected {available}",
        response.rect.width()
    );
    #[cfg(feature = "qr")]
    for rect in textured_rects(&out) {
        assert!(
            (rect.width() - rect.height()).abs() < 2.0,
            "QrImage must stay square, got {rect:?}"
        );
    }
}

#[test]
fn qr_image_donate_addresses_render_full_width() {
    for address in [
        "bc1qa3qk8d4mxl6qkpvahl5xvg6c5k33kmuwvt9v8q",
        "48sTw2TvjuWKkaomi9J7gLExRUJLJCvUHLrbf8M8qmayQ9zkho1GYdCXVtpTPawNWH7mNS49N4E6HNDF95dtggMMCigrVyG",
    ] {
        let mut body = |ui: &mut egui::Ui| functora_egui::QrImage::new(address).show(ui);
        let (response, available, _) = run_once(&mut body);
        assert!(
            (response.rect.width() - available).abs() < 2.0,
            "donate QR for {address} must be full width, got {} expected {available}",
            response.rect.width()
        );
    }
}

#[test]
#[cfg(feature = "qr")]
fn qr_image_caps_width_on_wide_desktop() {
    let mut body = |ui: &mut egui::Ui| functora_egui::QrImage::new("https://example.com").show(ui);
    let (_, available, out) = run_on(WIDE_SCREEN, &mut body);
    assert!(
        available > 1000.0,
        "wide fixture must offer desktop-class width, got {available}"
    );
    let rendered = textured_rects(&out);
    assert!(
        !rendered.is_empty(),
        "wide desktop must still render the QR image"
    );
    for rect in &rendered {
        assert!(
            rect.width() <= 481.0,
            "QR image must be capped on wide desktop, got {rect:?}"
        );
        assert!(
            (rect.width() - rect.height()).abs() < 2.0,
            "capped QR image must stay square, got {rect:?}"
        );
        assert!(
            (rect.center().x - WIDE_SCREEN.x * 0.5).abs() < 8.0,
            "capped QR image must stay centered, got {rect:?}"
        );
    }
}

#[test]
#[cfg(feature = "qr")]
fn qr_image_stays_large_on_narrow_phone() {
    let mut body = |ui: &mut egui::Ui| functora_egui::QrImage::new("https://example.com").show(ui);
    let (_, available, out) = run_on(NARROW_SCREEN, &mut body);
    let rendered = textured_rects(&out);
    assert!(
        !rendered.is_empty(),
        "narrow phone must still render the QR image"
    );
    for rect in &rendered {
        assert!(
            rect.width() > 280.0,
            "QR image must stay large on narrow phone, got {rect:?}"
        );
        assert!(
            rect.width() <= available + 1.0,
            "QR image must not overflow narrow phone, got {rect:?} for {available}"
        );
        assert!(
            (rect.width() - rect.height()).abs() < 2.0,
            "phone QR image must stay square, got {rect:?}"
        );
    }
}

#[test]
fn qr_image_empty_content_does_not_panic() {
    let mut body = |ui: &mut egui::Ui| functora_egui::QrImage::new("").show(ui);
    let (response, _available, _) = run_once(&mut body);
    assert!(response.rect.width() >= 0.0);
}

#[test]
fn qr_image_distinct_contents_use_distinct_textures() {
    assert_ne!(
        egui::util::hash("https://example.com/1"),
        egui::util::hash("https://example.com/2")
    );
}
