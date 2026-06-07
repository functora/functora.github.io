pub use isolang::Language;

pub fn language_from_code(code: &str) -> Language {
    code.split('-')
        .next()
        .map(str::to_ascii_lowercase)
        .as_deref()
        .and_then(Language::from_639_1)
        .unwrap_or_default()
}

#[cfg(target_arch = "wasm32")]
pub fn detect_browser_language() -> Language {
    web_sys::window()
        .and_then(|x| x.navigator().language())
        .as_deref()
        .map_or_else(Language::default, language_from_code)
}

#[cfg(not(target_arch = "wasm32"))]
pub fn detect_browser_language() -> Language {
    sys_locale::get_locale()
        .as_deref()
        .map_or_else(Language::default, language_from_code)
}
