#![allow(clippy::unwrap_used, clippy::expect_used)]
use functora_egui::utils::responsive_preview_size;

const NARROW: (f32, f32) = (390.0, 844.0);
const WIDE: (f32, f32) = (1440.0, 900.0);

#[test]
fn portrait_frame_takes_full_width_in_portrait() {
    let fitted = responsive_preview_size(366.0, f32::INFINITY, NARROW.0, NARROW.1, 240, 320);
    assert!((fitted.x - 366.0).abs() < 0.5, "got {fitted:?}");
    assert!((fitted.y - 488.0).abs() < 0.5, "got {fitted:?}");
}

#[test]
fn landscape_frame_takes_full_width_in_portrait() {
    let fitted = responsive_preview_size(366.0, f32::INFINITY, NARROW.0, NARROW.1, 640, 480);
    assert!((fitted.x - 366.0).abs() < 0.5, "got {fitted:?}");
    assert!((fitted.y - 274.5).abs() < 0.5, "got {fitted:?}");
}

#[test]
fn portrait_frame_is_height_capped_in_landscape() {
    let fitted = responsive_preview_size(600.0, 300.0, 844.0, 390.0, 240, 320);
    assert!((fitted.y - 300.0).abs() < 0.5, "got {fitted:?}");
    assert!((fitted.x - 225.0).abs() < 0.5, "got {fitted:?}");
}

#[test]
fn desktop_width_is_capped_like_qr_image() {
    let fitted = responsive_preview_size(1000.0, 800.0, WIDE.0, WIDE.1, 640, 480);
    assert!((fitted.x - 480.0).abs() < 0.5, "got {fitted:?}");
    assert!((fitted.y - 360.0).abs() < 0.5, "got {fitted:?}");
}

#[test]
fn hostile_inputs_fall_back_without_panic() {
    let fitted = responsive_preview_size(0.0, 0.0, 0.0, 0.0, 0, 0);
    assert_eq!(fitted, egui::vec2(320.0, 240.0));
    let nan = responsive_preview_size(f32::NAN, f32::NAN, f32::NAN, f32::NAN, 640, 480);
    assert!(nan.x.is_finite() && nan.y.is_finite() && nan.x > 0.0 && nan.y > 0.0);
}

fn run_on(
    screen: egui::Vec2,
    body: &mut dyn FnMut(&mut egui::Ui, &mut Option<egui::Response>),
) -> egui::Response {
    let ctx = egui::Context::default();
    let mut captured: Option<egui::Response> = None;
    let raw = egui::RawInput {
        screen_rect: Some(egui::Rect::from_min_size(egui::Pos2::ZERO, screen)),
        ..Default::default()
    };
    let mut out = ctx.run_ui(raw, |ui| {
        let _ = egui::CentralPanel::default().show(ui, |inner| {
            body(inner, &mut captured);
        });
    });
    out.textures_delta.clear();
    captured.expect("body must run")
}

#[test]
fn qr_scanner_fills_narrow_phone_width() {
    let mut state = functora_egui::QrScannerState::new();
    let mut body = |ui: &mut egui::Ui, slot: &mut Option<egui::Response>| {
        *slot = Some(
            functora_egui::QrScanner::new()
                .auto_start(false)
                .show(ui, &mut state),
        );
    };
    let response = run_on(egui::vec2(NARROW.0, NARROW.1), &mut body);
    assert!(
        response.rect.width() > 340.0,
        "narrow phone preview must fill width, got {:?}",
        response.rect
    );
}

#[test]
fn qr_scanner_is_capped_on_wide_desktop() {
    let mut state = functora_egui::QrScannerState::new();
    let mut body = |ui: &mut egui::Ui, slot: &mut Option<egui::Response>| {
        *slot = Some(
            functora_egui::QrScanner::new()
                .auto_start(false)
                .show(ui, &mut state),
        );
    };
    let response = run_on(egui::vec2(WIDE.0, WIDE.1), &mut body);
    assert!(
        response.rect.width() <= 560.0,
        "wide desktop preview must stay capped, got {:?}",
        response.rect
    );
}

#[test]
fn camera_view_fills_narrow_phone_width() {
    let mut state = functora_egui::CameraViewState::new();
    let mut body = |ui: &mut egui::Ui, slot: &mut Option<egui::Response>| {
        *slot = Some(
            functora_egui::CameraView::new()
                .auto_start(false)
                .show(ui, &mut state),
        );
    };
    let response = run_on(egui::vec2(NARROW.0, NARROW.1), &mut body);
    assert!(
        response.rect.width() > 340.0,
        "narrow phone preview must fill width, got {:?}",
        response.rect
    );
}
