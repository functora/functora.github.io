use crate::*;

pub enum UiMessage {
    Copied,
    Error(AppError),
}

#[component]
pub fn Message(
    message: Signal<Option<UiMessage>>,
) -> Element {
    let cfg = use_context::<Signal<AppCfg>>();
    let t = get_translations(cfg.read().language);

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
