use crate::*;

pub enum UiMessage {
    Copied,
    Error(AppError),
}

#[component]
pub fn Message(
    message: Signal<Option<UiMessage>>,
) -> Element {
    let language = use_context::<Signal<Language>>();
    let t = get_translations(language());

    let text = match &*message.read() {
        Some(UiMessage::Copied) => t.copied.to_string(),
        Some(UiMessage::Error(e)) => e.localized(&t),
        None => return rsx! {},
    };

    rsx! {
        pre {
            code { white_space: "pre-wrap", "{text}" }
        }
    }
}
