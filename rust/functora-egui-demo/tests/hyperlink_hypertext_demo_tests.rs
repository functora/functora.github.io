//! Hyperlink and Hypertext must be registered catalog components with live
//! demos that render both the widget and its teaching snippet.

use functora_egui_demo::{CATEGORIES, ComponentId, ShowcaseApp};

mod common;
use common::rendered_text;

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
