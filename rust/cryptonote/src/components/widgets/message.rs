use crate::messages::*;
use crate::*;
use functora_dioxus::i18n::I18N;

pub enum UiMessage {
    Copied,
    Error(AppError),
}

impl UiMessage {
    pub fn to_text(&self, lang: Language) -> String {
        match self {
            UiMessage::Copied => MsgCopied.render(lang),
            UiMessage::Error(e) => e.localized(lang),
        }
    }
}

#[component]
pub fn Message(
    message: Signal<Option<UiMessage>>,
) -> Element {
    let lang = use_lang();
    let text = message
        .with(|m| m.as_ref().map(|m| m.to_text(lang)));

    match text {
        Some(text) => rsx! {
            Pre { Quote { "{text}" } }
        },
        None => rsx! {},
    }
}
