use functora_egui::i18n::{I18N, Language};
use functora_egui_demo::ShowcaseApp;

mod common;
use common::rendered_text;

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
