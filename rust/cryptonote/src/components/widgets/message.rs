use crate::*;

pub enum UiMessage {
    Copied,
    Error(AppError),
}

impl UiMessage {
    pub fn to_text(&self, t: &Translations) -> String {
        match self {
            UiMessage::Copied => t.copied.to_string(),
            UiMessage::Error(e) => e.localized(t),
        }
    }
}

#[component]
pub fn Message(
    message: Signal<Option<UiMessage>>,
) -> Element {
    let t = use_translations();
    let text =
        message.with(|m| m.as_ref().map(|m| m.to_text(&t)));

    match text {
        Some(text) => rsx! {
            Pre { Quote { "{text}" } }
        },
        None => rsx! {},
    }
}
