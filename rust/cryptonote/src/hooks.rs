use crate::error::AppError;
use crate::messages::Msg;
use crate::*;

pub fn use_lang() -> Language {
    use_context::<PersistentSignal<PersistentState>>()
        .language()()
}

pub fn use_message() -> Signal<Option<Msg>> {
    use_signal(|| None)
}

pub fn read_clipboard(
    on_paste: impl FnOnce(String) + 'static,
    mut message: Signal<Option<Msg>>,
) {
    spawn(async move {
        match functora_dioxus::ffi::read_clipboard().await {
            Ok(text) => on_paste(text),
            Err(e) => message
                .set(Some(Msg::Error(AppError::Fd(e)))),
        }
    });
}

pub fn write_clipboard(
    val: String,
    message: Signal<Option<Msg>>,
) {
    functora_dioxus::ffi::write_clipboard(
        val,
        message,
        Msg::Base(BaseMsg::Copied),
        |e| {
            Msg::Base(BaseMsg::ClipboardWriteError(
                format!("{e}"),
            ))
        },
    );
}
