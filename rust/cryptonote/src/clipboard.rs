use functora_dioxus::js::js_write_clipboard;

use crate::components::widgets::UiMessage;
use crate::error::AppError;
use dioxus::prelude::*;

pub fn write_clipboard(
    val: String,
    mut message: Signal<Option<UiMessage>>,
) {
    spawn(async move {
        match js_write_clipboard(val).await {
            Ok(()) => message.set(Some(UiMessage::Copied)),
            Err(e) => message.set(Some(UiMessage::Error(
                AppError::JsWriteClipboard(e),
            ))),
        }
    });
}
