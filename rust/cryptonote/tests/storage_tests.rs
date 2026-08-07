#![allow(clippy::unwrap_used, clippy::expect_used)]
use cryptonote::PersistentState;

#[test]
fn persistent_state_default_has_light_theme() {
    let state = PersistentState::default();
    assert_eq!(state.theme, cryptonote::Theme::Light);
}

#[test]
fn persistent_state_default_language_is_detected() {
    let state = PersistentState::default();
    let detected = cryptonote::i18n::detect_browser_language();
    assert_eq!(state.language, detected);
}
