use cryptonote::{
    get_translations, language_label, Language,
    SUPPORTED_LANGUAGES,
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
fn get_translations_english_basic_fields() {
    let t = get_translations(Language::Eng);
    assert_eq!(t.note, "Note");
    assert_eq!(t.generate_button, "Share");
    assert_eq!(t.back_button, "Back");
    assert_eq!(t.theme, "Theme");
}

#[test]
fn get_translations_spanish_basic_fields() {
    let t = get_translations(Language::Spa);
    assert_eq!(t.note, "Nota");
    assert_eq!(t.generate_button, "Compartir");
    assert_eq!(t.back_button, "Atrás");
    assert_eq!(t.theme, "Tema");
}

#[test]
fn get_translations_russian_basic_fields() {
    let t = get_translations(Language::Rus);
    assert_eq!(t.note, "Заметка");
    assert_eq!(t.generate_button, "Поделиться");
    assert_eq!(t.back_button, "Назад");
    assert_eq!(t.theme, "Тема");
}

#[test]
fn get_translations_unsupported_falls_back_to_english() {
    let eng = get_translations(Language::Eng);
    let fallback = get_translations(Language::default());
    assert_eq!(fallback.note, eng.note);
    assert_eq!(
        fallback.generate_button,
        eng.generate_button
    );
}

#[test]
fn get_translations_all_locales_have_consistent_field_count(
) {
    let t_en = get_translations(Language::Eng);
    let t_es = get_translations(Language::Spa);
    let t_ru = get_translations(Language::Rus);
    assert!(t_en.license_text.contains("Copyright"));
    assert!(t_es.license_text.contains("Copyright"));
    assert!(t_ru.license_text.contains("Copyright"));
}
