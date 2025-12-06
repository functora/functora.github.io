use crate::i18n::Language;
use dioxus::prelude::*;

#[component]
pub fn Privacy() -> Element {
    let language = use_context::<Signal<Language>>();
    let translations =
        crate::i18n::get_translations(language());

    rsx! {
        section {
            pre { white_space: "pre-wrap", "{translations.privacy_text}" }
        }
    }
}
