//! Package and white-label demos must render real build metadata and derived
//! branding URLs instead of hardcoded or mislabeled output.

use functora_egui_demo::ShowcaseApp;

mod common;
use common::rendered_text;

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
