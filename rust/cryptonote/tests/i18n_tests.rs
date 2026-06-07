use cryptonote::messages::*;
use cryptonote::{
    language_label, Language, I18N, SUPPORTED_LANGUAGES,
};

#[test]
fn supported_languages_contains_known() {
    assert!(SUPPORTED_LANGUAGES.contains(&Language::Eng));
    assert!(SUPPORTED_LANGUAGES.contains(&Language::Spa));
    assert!(SUPPORTED_LANGUAGES.contains(&Language::Rus));
}

#[test]
fn supported_languages_matches_labels() {
    SUPPORTED_LANGUAGES.iter().for_each(|lang| {
        assert_ne!(language_label(*lang), "🌐 Unknown")
    });
}

#[test]
fn unknown_language_label_falls_back() {
    assert_eq!(
        language_label(Language::default()),
        "🌐 Unknown"
    );
    assert_eq!(language_label(Language::Fra), "🌐 Unknown");
}

#[test]
fn i18n_english_basic_messages() {
    assert_eq!(MsgNote.render(Language::Eng), "Note");
    assert_eq!(
        MsgGenerateButton.render(Language::Eng),
        "Share"
    );
    assert_eq!(MsgBack.render(Language::Eng), "Back");
    assert_eq!(MsgTheme.render(Language::Eng), "Theme");
}

#[test]
fn i18n_spanish_basic_messages() {
    assert_eq!(MsgNote.render(Language::Spa), "Nota");
    assert_eq!(
        MsgGenerateButton.render(Language::Spa),
        "Compartir"
    );
    assert_eq!(MsgBack.render(Language::Spa), "Atrás");
    assert_eq!(MsgTheme.render(Language::Spa), "Tema");
}

#[test]
fn i18n_russian_basic_messages() {
    assert_eq!(MsgNote.render(Language::Rus), "Заметка");
    assert_eq!(
        MsgGenerateButton.render(Language::Rus),
        "Поделиться"
    );
    assert_eq!(MsgBack.render(Language::Rus), "Назад");
    assert_eq!(MsgTheme.render(Language::Rus), "Тема");
}

#[test]
fn i18n_unsupported_falls_back_to_english() {
    let unsupported = Language::Fra;
    assert_eq!(
        MsgNote.render(unsupported),
        MsgNote.render(Language::Eng)
    );
    assert_eq!(
        MsgGenerateButton.render(unsupported),
        MsgGenerateButton.render(Language::Eng)
    );
}

#[test]
fn i18n_all_messages_have_translations() {
    assert!(MsgLicenseText
        .render(Language::Eng)
        .contains("Copyright"));
    assert!(MsgLicenseText
        .render(Language::Spa)
        .contains("Copyright"));
    assert!(MsgLicenseText
        .render(Language::Rus)
        .contains("Copyright"));
}

#[test]
fn i18n_all_supported_languages_render_differently() {
    let eng = MsgNote.render(Language::Eng);
    let spa = MsgNote.render(Language::Spa);
    let rus = MsgNote.render(Language::Rus);
    assert_ne!(eng, spa);
    assert_ne!(eng, rus);
    assert_ne!(spa, rus);
}

#[test]
fn i18n_render_dispatches_correct_language() {
    assert_eq!(MsgHome.render(Language::Eng), "Home");
    assert_eq!(MsgHome.render(Language::Spa), "Inicio");
    assert_eq!(MsgHome.render(Language::Rus), "Главная");
}
