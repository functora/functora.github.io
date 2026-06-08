use crate::messages::*;
use crate::*;
use functora_dioxus::i18n::I18N;
use functora_dioxus::js::js_write_clipboard;

pub fn write_clipboard(
    val: String,
    mut message: Signal<Option<String>>,
    lang: Language,
) {
    spawn(async move {
        match js_write_clipboard(val).await {
            Ok(()) => {
                message.set(Some(MsgCopied.render(lang)));
            }
            Err(e) => {
                message.set(Some(
                    AppError::Fd(e).render(lang),
                ));
            }
        }
    });
}
