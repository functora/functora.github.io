use crate::i18n::{I18N, Language};
use crate::js::js_write_clipboard;
use crate::messages::*;
use dioxus::prelude::*;

pub fn write_clipboard(val: String, message: Signal<Option<String>>, lang: Language) {
    write_clipboard_with(val, message, lang, MsgCopied, move |e| e.render(lang));
}

pub fn write_clipboard_with<S: I18N + 'static>(
    val: String,
    mut message: Signal<Option<String>>,
    lang: Language,
    success: S,
    map_error: impl FnOnce(crate::Error) -> String + 'static,
) {
    let _ = spawn(async move {
        match js_write_clipboard(val).await {
            Ok(()) => {
                message.set(Some(success.render(lang)));
            }
            Err(e) => {
                message.set(Some(map_error(e)));
            }
        }
    });
}
