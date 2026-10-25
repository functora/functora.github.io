//! `Navbar`, `Footer`, `BlockingOverlay` and `CodeSnippet` must be registered
//! catalog components with live demos that render both the widget and
//! its teaching snippet.

use functora_egui_demo::{CATEGORIES, ComponentId, ShowcaseApp};

mod common;
use common::rendered_text;

fn rendered_app<F>(mut body: F) -> String
where
    F: FnMut(&mut ShowcaseApp, &mut egui::Ui),
{
    let mut app = ShowcaseApp::default();
    rendered_text(|ui| body(&mut app, ui))
}

#[test]
fn new_demos_are_registered_catalog_entries() {
    for id in [
        ComponentId::Navbar,
        ComponentId::Footer,
        ComponentId::BlockingOverlay,
        ComponentId::CodeSnippet,
    ] {
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
fn button_group_demo_renders_options_and_snippet() {
    let id = ComponentId::ButtonGroup;
    assert!(ComponentId::ALL.contains(&id));
    assert_eq!(ComponentId::from_slug(&id.slug()), Some(id));
    let text = rendered_app(ShowcaseApp::demo_button_group);
    assert!(
        text.contains("Selectable group"),
        "button group demo must render its selectable section: {text}"
    );
    assert!(
        text.contains("ButtonGroup::show(ui, |g|"),
        "button group demo must teach ButtonGroup::show: {text}"
    );
    assert!(
        text.contains("Disabled group"),
        "button group demo must render its disabled section: {text}"
    );
}

#[test]
fn navbar_demo_renders_brand_and_snippet() {
    let text = rendered_app(ShowcaseApp::demo_navbar);
    assert!(
        text.contains("functora-egui"),
        "navbar demo must render its brand: {text}"
    );
    assert!(
        text.contains("Search..."),
        "navbar demo must render its search field: {text}"
    );
    assert!(
        text.contains("Navbar::new(\"functora-egui\")"),
        "navbar demo must teach Navbar::new: {text}"
    );
    assert!(
        text.contains("Some(&mut on_brand)"),
        "navbar demo must teach brand/search callbacks: {text}"
    );
}

#[test]
fn footer_demo_renders_links_and_snippet() {
    let text = rendered_text(ShowcaseApp::demo_footer);
    assert!(
        text.contains("Privacy"),
        "footer demo must render its links: {text}"
    );
    assert!(
        text.contains("Footer::new()"),
        "footer demo must teach Footer::new: {text}"
    );
}

#[test]
fn blocking_overlay_demo_renders_trigger_and_snippet() {
    let text = rendered_app(ShowcaseApp::demo_blocking_overlay);
    assert!(
        text.contains("Show Blocking Overlay"),
        "blocking overlay demo must render its trigger: {text}"
    );
    assert!(
        text.contains("BlockingOverlay::new(\"Processing files...\")"),
        "blocking overlay demo must teach BlockingOverlay::new: {text}"
    );
    assert!(
        text.contains("cancel.load(Ordering::Relaxed)"),
        "blocking overlay demo must teach cancel polling: {text}"
    );
}

#[test]
fn code_snippet_demo_renders_both_variants_and_snippet() {
    let text = rendered_text(ShowcaseApp::demo_code_snippet);
    assert!(
        text.contains("Hello, world!"),
        "code snippet demo must render its example code: {text}"
    );
    assert!(
        text.contains("snippet_break_long_words"),
        "code snippet demo must teach snippet_break_long_words: {text}"
    );
}
