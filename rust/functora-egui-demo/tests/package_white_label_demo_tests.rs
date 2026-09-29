//! Package and white-label demos must render real build metadata and derived
//! branding URLs instead of hardcoded or mislabeled output.

use egui::{Context, Pos2, RawInput, Rect, Vec2};
use functora_egui_demo::ShowcaseApp;

fn rendered_text<F: FnMut(&mut egui::Ui)>(mut body: F) -> String {
    let ctx = Context::default();
    let mut out = ctx.run_ui(
        RawInput {
            screen_rect: Some(Rect::from_min_size(Pos2::ZERO, Vec2::new(1280.0, 900.0))),
            time: Some(1.0 / 60.0),
            ..Default::default()
        },
        |ui| {
            let _ = egui::CentralPanel::default().show(ui, |inner| body(inner));
        },
    );
    out.textures_delta.clear();
    out.shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            egui::Shape::Text(text) => Some(text.galley.text().to_owned()),
            _ => None,
        })
        .collect::<Vec<_>>()
        .join("\n")
}

#[test]
fn package_demo_shows_real_build_metadata() {
    let text = rendered_text(ShowcaseApp::demo_package);
    assert!(
        text.contains(&format!(
            "crate: functora-egui-demo v{}",
            env!("CARGO_PKG_VERSION")
        )),
        "package demo must label the demo crate's own version: {text}"
    );
    assert!(
        text.contains(&format!("title: {}", env!("DEMO_WEB_TITLE"))),
        "package demo must show the web title from Cargo metadata: {text}"
    );
    assert!(
        text.contains(&format!("theme_color: {}", env!("DEMO_WEB_THEME_COLOR"))),
        "package demo must show the theme_color from Cargo metadata: {text}"
    );
    assert!(
        text.contains(functora_egui::FUNCTORA_CORE_DATE),
        "package demo must show FUNCTORA_CORE_DATE: {text}"
    );
    assert!(
        !text.contains("FUNCTORA_CORE version"),
        "package demo must not label the demo crate version as FUNCTORA_CORE: {text}"
    );
    assert!(
        text.contains("CARGO_PKG_VERSION"),
        "package demo must show how version metadata is read: {text}"
    );
}

#[test]
fn white_label_demo_shows_derived_branding() {
    let text = rendered_text(ShowcaseApp::demo_white_label);
    assert!(
        text.contains(ShowcaseApp::DEMO_ATTRS.app_url().as_str()),
        "white-label demo must show the derived app_url: {text}"
    );
    assert!(
        text.contains("BTC - Bitcoin"),
        "white-label demo must list real donate blocks: {text}"
    );
    assert!(
        text.contains("const ATTRS: AppAttrs"),
        "white-label demo snippet must teach AppAttrs construction: {text}"
    );
    assert!(
        !text.contains("WhiteLabel::load"),
        "white-label demo must not advertise a nonexistent load API: {text}"
    );
}
