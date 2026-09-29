//! Hyperlink and Hypertext must be registered catalog components with live
//! demos that render both the widget and its teaching snippet.

use egui::{Context, Pos2, RawInput, Rect, Vec2};
use functora_egui_demo::{CATEGORIES, ComponentId, ShowcaseApp};

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
fn hyperlink_and_hypertext_are_registered_catalog_entries() {
    for id in [ComponentId::Hyperlink, ComponentId::Hypertext] {
        assert!(
            ComponentId::ALL.contains(&id),
            "{id:?} must be in ComponentId::ALL"
        );
        assert_eq!(ComponentId::from_slug(&id.slug()), Some(id));
        let matches = CATEGORIES
            .iter()
            .flat_map(|(_, _, items)| items.iter())
            .filter(|def| def.name == id.name())
            .count();
        assert_eq!(matches, 1, "{id:?} must have exactly one catalog entry");
    }
}

#[test]
fn hyperlink_demo_renders_links_and_snippet() {
    let text = rendered_text(ShowcaseApp::demo_hyperlink);
    assert!(
        text.contains("Same tab"),
        "hyperlink demo must render its link labels: {text}"
    );
    assert!(
        text.contains("https://github.com/functora/functora-egui"),
        "hyperlink demo must show snippet URLs: {text}"
    );
    assert!(
        text.contains("open_in_new_tab(false)"),
        "hyperlink demo must teach the same-tab option: {text}"
    );
}

#[test]
fn hypertext_demo_renders_segments_and_snippet() {
    let mut app = ShowcaseApp::default();
    let text = rendered_text(|ui| app.demo_hypertext(ui));
    assert!(
        text.contains("onboarding guide"),
        "hypertext demo must render its action segment: {text}"
    );
    assert!(
        text.contains(".action(\"onboarding guide\", \"onboarding\")"),
        "hypertext demo must teach action segments: {text}"
    );
    assert!(
        text.contains(".show_action(ui)"),
        "hypertext demo must teach show_action: {text}"
    );
}
