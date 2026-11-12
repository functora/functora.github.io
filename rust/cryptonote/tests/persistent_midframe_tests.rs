//! `eframe::App::ui` must leave persistent state intact across an idle frame:
//! no take/restore churn and no spurious writes to the selected theme and
//! language.

use cryptonote_egui::app::CryptonoteApp;
use functora_egui::i18n::Language;
use functora_egui::theme_extra::Theme;

pub mod common;
use common::run_app;

#[test]
fn idle_frame_leaves_persistent_state_unchanged() {
    let mut app = CryptonoteApp::default();
    app.set_language(Language::Spa);
    app.persistent.theme = Theme::Dark;
    let before = app.persistent.clone();
    let rendered = run_app(&mut app);
    assert_eq!(
        app.persistent, before,
        "an idle frame must not mutate persistent state, rendered:\n{rendered}"
    );
}
