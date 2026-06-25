use crate::messages::Msg;
use crate::*;

pub fn use_lang() -> Language {
    use_context::<PersistentSignal<PersistentState>>()
        .language()()
}

pub fn use_message() -> Signal<Option<Msg>> {
    use_signal(|| None)
}

pub fn paste_clipboard(
    on_paste: impl FnOnce(String) + 'static,
    mut message: Signal<Option<Msg>>,
    lang: Language,
) {
    spawn(async move {
        match read_clipboard().await {
            Ok(text) => on_paste(text),
            Err(e) => message.set(Some(Msg::Error(
                AppError::Fd(e).render(lang),
            ))),
        }
    });
}
