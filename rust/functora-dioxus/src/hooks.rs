use crate::Language;
use crate::i18n::I18N;
use crate::state::{PersistentState, PersistentStateStoreExt};
use crate::storage::PersistentSignal;
use dioxus::prelude::*;

#[must_use]
pub fn use_lang() -> Language {
    use_context::<PersistentSignal<PersistentState>>().language()()
}

pub fn use_message_markdown<T: I18N + 'static>(msg: T) -> Memo<String> {
    let pst = use_context::<PersistentSignal<PersistentState>>();
    use_memo(move || msg.render_markdown(pst.language()()))
}

#[must_use]
pub fn use_message<T: 'static>() -> Signal<Option<T>> {
    use_signal(|| None)
}
