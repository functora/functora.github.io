use crate::components::*;
use crate::i18n::Language;
use crate::prelude::*;
use crate::storage::AppCfg;
use functora_dioxus::Nav;

pub fn use_app_nav() -> Signal<Nav<Route>> {
    use_context::<Signal<Nav<Route>>>()
}

pub fn use_lang() -> Language {
    use_context::<Signal<AppCfg>>().read().language
}
