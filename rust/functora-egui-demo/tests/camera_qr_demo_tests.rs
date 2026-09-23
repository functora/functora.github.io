use functora_egui_demo::{CATEGORIES, ComponentId, ShowcaseApp};

#[test]
fn qr_image_component_is_registered() {
    assert_eq!(
        ComponentId::from_slug("qrimage"),
        Some(ComponentId::QrImage)
    );
    assert!(ComponentId::ALL.contains(&ComponentId::QrImage));
    assert_eq!(ComponentId::QrImage.name(), "QrImage");
    assert_eq!(ComponentId::QrImage.slug(), "qrimage");
    let found = CATEGORIES
        .iter()
        .flat_map(|(_, _, items)| items.iter())
        .any(|def| def.id == Some(ComponentId::QrImage));
    assert!(found, "QrImage must appear in CATEGORIES");
}

#[test]
fn platform_state_has_camera_view_and_qr_image_defaults() {
    let state = ShowcaseApp::default();
    assert!(!state.platform.camera_view_state.is_running());
    assert!(state.platform.camera_view_state.error().is_none());
    assert!(!state.platform.qr_image_input.is_empty());
    assert!(state.platform.qr_last_scan.is_empty());
    assert!(state.platform.qr_error_notified.is_none());
}

#[test]
fn qr_image_input_generates_qr_payload() {
    let state = ShowcaseApp::default();
    let payload = functora_egui::qr::qr_rgba(&state.platform.qr_image_input, 128);
    assert!(
        payload.is_some(),
        "default QrImage input must encode to QR payload"
    );
}

#[test]
fn demo_qr_image_renders_without_panic() {
    use egui::{Context, Pos2, RawInput, Rect, Vec2};
    let mut state = ShowcaseApp::default();
    let app_ctx = Context::default();
    let mut out = app_ctx.run_ui(
        RawInput {
            screen_rect: Some(Rect::from_min_size(Pos2::ZERO, Vec2::new(1280.0, 800.0))),
            time: Some(1.0 / 60.0),
            ..Default::default()
        },
        |ui| {
            let _ = egui::CentralPanel::default().show(ui, |inner| state.demo_qr_image(inner));
        },
    );
    out.textures_delta.clear();
    assert!(!out.shapes.is_empty(), "QrImage demo must emit shapes");
}

#[test]
fn demo_camera_renders_live_preview_card() {
    use egui::{Context, Pos2, RawInput, Rect, Shape, Vec2};
    let mut state = ShowcaseApp::default();
    let app_ctx = Context::default();
    let mut out = app_ctx.run_ui(
        RawInput {
            screen_rect: Some(Rect::from_min_size(Pos2::ZERO, Vec2::new(1280.0, 800.0))),
            time: Some(1.0 / 60.0),
            ..Default::default()
        },
        |ui| {
            let _ = egui::CentralPanel::default().show(ui, |inner| state.demo_camera(inner));
        },
    );
    out.textures_delta.clear();
    let labels: Vec<String> = out
        .shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            Shape::Text(text) => Some(text.galley.text().to_owned()),
            _ => None,
        })
        .collect();
    assert!(
        labels.iter().any(|t| t.contains("Live preview")),
        "Camera demo must render live preview card, got: {labels:?}"
    );
}

#[test]
fn demo_qr_scanner_renders_scan_tab_pattern() {
    use egui::{Context, Pos2, RawInput, Rect, Shape, Vec2};
    let mut state = ShowcaseApp::default();
    let app_ctx = Context::default();
    let mut out = app_ctx.run_ui(
        RawInput {
            screen_rect: Some(Rect::from_min_size(Pos2::ZERO, Vec2::new(1280.0, 800.0))),
            time: Some(1.0 / 60.0),
            ..Default::default()
        },
        |ui| {
            let _ = egui::CentralPanel::default().show(ui, |inner| state.demo_qr_scanner(inner));
        },
    );
    let has_qr_texture = !out.textures_delta.set.is_empty();
    out.textures_delta.clear();
    let labels: Vec<String> = out
        .shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            Shape::Text(text) => Some(text.galley.text().to_owned()),
            _ => None,
        })
        .collect();
    assert!(
        labels.iter().any(|t| t.contains("Scan action")),
        "QrScanner demo must render scan-action pattern, got: {labels:?}"
    );
    assert!(
        labels.iter().any(|t| t.contains("Preview content")),
        "QrScanner demo must render generated QR preview, got: {labels:?}"
    );
    assert!(
        has_qr_texture,
        "QrScanner demo must upload generated QR texture"
    );
    assert!(
        labels.iter().all(|t| !t.contains("Generate QR")),
        "QrScanner demo must not render redundant Generate button, got: {labels:?}"
    );
    assert!(
        labels.iter().all(|t| !t.contains("Generating...")),
        "QrScanner demo must not render generating state, got: {labels:?}"
    );
}
