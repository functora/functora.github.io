use crate::i18n::Language;
use crate::prelude::*;
use crate::storage::AppCfg;

pub fn use_lang() -> Language {
    use_context::<Signal<AppCfg>>().read().language
}
