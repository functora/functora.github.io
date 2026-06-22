use crate::i18n::I18N;
use crate::js::js_write_clipboard;
use dioxus::prelude::*;

pub fn write_clipboard<S: I18N + 'static>(
    val: String,
    mut message: impl Writable<Target = Option<S>> + 'static,
    success: S,
    map_error: impl FnOnce(crate::Error) -> S + 'static,
) {
    let _ = spawn(async move {
        match js_write_clipboard(val).await {
            Ok(()) => message.set(Some(success)),
            Err(e) => message.set(Some(map_error(e))),
        }
    });
}
