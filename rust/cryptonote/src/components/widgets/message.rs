use crate::*;

pub enum UiMessage {
    Copied,
    Error(AppError),
}

#[component]
pub fn Message(
    message: Signal<Option<UiMessage>>,
) -> Element {
    let t = use_translations();

    let text = match &*message.read() {
        Some(UiMessage::Copied) => t.copied.to_string(),
        Some(UiMessage::Error(e)) => e.localized(&t),
        None => return rsx! {},
    };

    rsx! {
        Pre {
            Quote { "{text}" }
        }
    }
}
