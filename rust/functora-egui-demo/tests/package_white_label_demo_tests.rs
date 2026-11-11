//! Package and white-label demos must render real build metadata and derived
//! branding URLs instead of hardcoded or mislabeled output.

use functora_egui_demo::ShowcaseApp;

mod common;
use common::{rendered_text, rendered_text_sized};

fn rendered_app<F>(mut body: F) -> String
where
    F: FnMut(&mut ShowcaseApp, &mut egui::Ui),
{
    let mut app = ShowcaseApp::default();
    rendered_text(|ui| body(&mut app, ui))
}

fn rendered_app_sized<F>(mut body: F, height: f32) -> String
where
    F: FnMut(&mut ShowcaseApp, &mut egui::Ui),
{
    let mut app = ShowcaseApp::default();
    rendered_text_sized(&mut |ui| body(&mut app, ui), height)
}

#[test]
fn package_demo_shows_real_build_metadata() {
    let text = rendered_app(ShowcaseApp::demo_package);
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
    let text = rendered_app(ShowcaseApp::demo_white_label);
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

#[test]
fn white_label_preview_does_not_drive_the_page_history() {
    let source = include_str!("../src/screens/platform/white_label.rs");
    let navigations = source
        .lines()
        .filter(|line| line.contains("app.navigate(") && !line.trim_start().starts_with('"'))
        .collect::<Vec<_>>();
    assert!(
        navigations.is_empty(),
        "the embedded WhiteLabelApp preview must stay read-only: on the web its router pushes the real browser history and reverts the URL: {navigations:?}"
    );
    let text = rendered_app_sized(ShowcaseApp::demo_white_label, 8000.0);
    assert!(
        text.contains("route: overview"),
        "white-label demo must show the preview route without navigating: {text}"
    );
}

#[test]
fn white_label_demo_teaches_the_desktop_only_quit_entry() {
    let text = rendered_app(ShowcaseApp::demo_white_label);
    assert!(
        text.contains("QUIT_AVAILABLE"),
        "white-label demo must show where the quit entry is available: {text}"
    );
    for needle in [
        "default_sidebar",
        "SidebarAction",
        "SidebarAction::Quit",
        "functora_egui::platform::request_quit",
    ] {
        assert!(
            text.contains(needle),
            "white-label demo must teach the `{needle}` API: {text}"
        );
    }
    assert!(
        !text.contains("process::exit"),
        "white-label demo must not teach a hard process exit: {text}"
    );
}
