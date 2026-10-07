use egui::{Context, Pos2, RawInput, Rect, Vec2};
use functora_egui::i18n::{I18N, Language};
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
fn app_footer_shows_real_app_version() {
    let text = rendered_text(|ui| ShowcaseApp::show_app_footer(ui, Language::Eng));
    assert!(
        text.contains(ShowcaseApp::DEMO_ATTRS.vsn),
        "app footer must show the demo crate version: {text}"
    );
    assert!(
        text.contains(&functora_egui::messages::Msg::VersionLabel.render(Language::Eng)),
        "app footer must label the version cryptonote-style: {text}"
    );
    assert!(
        text.contains(env!("CARGO_PKG_VERSION")),
        "app footer must derive from CARGO_PKG_VERSION: {text}"
    );
}
