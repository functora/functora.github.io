use crate::*;

pub fn use_lang() -> Language {
    use_context::<PersistentSignal<PersistentState<()>>>()
        .read()
        .language
}
