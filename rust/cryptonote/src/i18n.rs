pub use functora_dioxus::i18n::I18N;
pub use functora_dioxus::i18n::{
    detect_browser_language, Language,
};

pub const SUPPORTED_LANGUAGES: &[Language] =
    &[Language::Eng, Language::Spa, Language::Rus];

pub fn language_label(lang: Language) -> &'static str {
    match lang {
        Language::Eng => "🇬🇧 English",
        Language::Spa => "🇪🇸 Español",
        Language::Rus => "🇷🇺 Русский",
        _ => "🌐 Unknown",
    }
}
