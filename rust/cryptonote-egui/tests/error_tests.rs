#![allow(clippy::unwrap_used, clippy::expect_used)]
use cryptonote_egui::{AppError, I18N, Language, SUPPORTED_LANGUAGES};

#[test]
fn supported_languages_contains_known() {
    assert!(SUPPORTED_LANGUAGES.contains(&Language::Eng));
    assert!(SUPPORTED_LANGUAGES.contains(&Language::Spa));
    assert!(SUPPORTED_LANGUAGES.contains(&Language::Rus));
}

#[test]
fn app_error_password_required_localized() {
    let err = AppError::PasswordRequired;
    assert!(!err.render(Language::Eng).is_empty());
    assert!(!err.render(Language::Spa).is_empty());
    assert!(!err.render(Language::Rus).is_empty());
}

#[test]
fn app_error_json_localized() {
    let parse: Result<serde_json::Value, _> = serde_json::from_str("not json");
    let err = AppError::from(parse.expect_err("expected parse failure"));
    assert!(!err.render(Language::Eng).is_empty());
    assert!(!err.render(Language::Spa).is_empty());
    assert!(!err.render(Language::Rus).is_empty());
}

#[test]
fn app_error_invalid_format_roundtrip() {
    let err = AppError::InvalidFormat("something went wrong".into());
    let eng = err.render(Language::Eng);
    let spa = err.render(Language::Spa);
    let rus = err.render(Language::Rus);
    assert!(!eng.is_empty());
    assert!(!spa.is_empty());
    assert!(!rus.is_empty());
}

#[test]
fn app_error_display_non_empty() {
    let errors = [
        AppError::PasswordRequired,
        AppError::NoNoteInUrl,
        AppError::NoNoteParam,
        AppError::InvalidFormat("bad format".into()),
        AppError::Archive("bad archive".into()),
        AppError::Platform("bad platform".into()),
    ];
    for err in errors {
        let msg = format!("{err}");
        assert!(!msg.is_empty());
    }
}

#[test]
fn msg_error_clone_and_display() {
    use cryptonote_egui::MsgError;
    let err = MsgError::from(AppError::PasswordRequired);
    let clone = err.clone();
    assert_eq!(err, clone);
    assert!(!err.render(Language::Eng).is_empty());
}
