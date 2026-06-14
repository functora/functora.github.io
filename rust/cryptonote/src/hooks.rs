use crate::*;

pub fn use_lang() -> Language {
    use_context::<PersistentSignal<AppCfg>>()
        .read()
        .language
}
