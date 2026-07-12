use cryptonote::messages::*;
use cryptonote::{language_label, Language, I18N, SUPPORTED_LANGUAGES};
use functora_dioxus::Msg as BaseMsg;

#[test]
fn supported_languages_contains_known() {
    assert!(SUPPORTED_LANGUAGES.contains(&Language::Eng));
    assert!(SUPPORTED_LANGUAGES.contains(&Language::Spa));
    assert!(SUPPORTED_LANGUAGES.contains(&Language::Rus));
}

#[test]
fn supported_languages_matches_labels() {
    SUPPORTED_LANGUAGES
        .iter()
        .for_each(|lang| assert_ne!(language_label(*lang), "🌐 Unknown"));
}

#[test]
fn unknown_language_label_falls_back() {
    assert_eq!(language_label(Language::default()), "🌐 Unknown");
    assert_eq!(language_label(Language::Fra), "🌐 Unknown");
}

#[test]
fn i18n_english_basic_messages() {
    assert_eq!(Msg::Note.render(Language::Eng), "Note");
    assert_eq!(Msg::GenerateButton.render(Language::Eng), "Share");
    assert_eq!(Msg::Base(BaseMsg::Back).render(Language::Eng), "Back");
    assert_eq!(Msg::Theme.render(Language::Eng), "Theme");
}

#[test]
fn i18n_spanish_basic_messages() {
    assert_eq!(Msg::Note.render(Language::Spa), "Nota");
    assert_eq!(Msg::GenerateButton.render(Language::Spa), "Compartir");
    assert_eq!(Msg::Base(BaseMsg::Back).render(Language::Spa), "Atrás");
    assert_eq!(Msg::Theme.render(Language::Spa), "Tema");
}

#[test]
fn i18n_russian_basic_messages() {
    assert_eq!(Msg::Note.render(Language::Rus), "Заметка");
    assert_eq!(Msg::GenerateButton.render(Language::Rus), "Поделиться");
    assert_eq!(Msg::Base(BaseMsg::Back).render(Language::Rus), "Назад");
    assert_eq!(Msg::Theme.render(Language::Rus), "Тема");
}

#[test]
fn i18n_unsupported_falls_back_to_english() {
    let unsupported = Language::Fra;
    assert_eq!(Msg::Note.render(unsupported), Msg::Note.render(Language::Eng));
    assert_eq!(
        Msg::GenerateButton.render(unsupported),
        Msg::GenerateButton.render(Language::Eng)
    );
}

#[test]
fn i18n_all_messages_have_translations() {
    assert!(Msg::LicenseText.render(Language::Eng).contains("Copyright"));
    assert!(Msg::LicenseText.render(Language::Spa).contains("Copyright"));
    assert!(Msg::LicenseText.render(Language::Rus).contains("Copyright"));
}

#[test]
fn i18n_all_supported_languages_render_differently() {
    let eng = Msg::Note.render(Language::Eng);
    let spa = Msg::Note.render(Language::Spa);
    let rus = Msg::Note.render(Language::Rus);
    assert_ne!(eng, spa);
    assert_ne!(eng, rus);
    assert_ne!(spa, rus);
}

#[test]
fn i18n_render_dispatches_correct_language() {
    assert_eq!(Msg::Base(BaseMsg::Home).render(Language::Eng), "Home");
    assert_eq!(Msg::Base(BaseMsg::Home).render(Language::Spa), "Inicio");
    assert_eq!(Msg::Base(BaseMsg::Home).render(Language::Rus), "Главная");
}
