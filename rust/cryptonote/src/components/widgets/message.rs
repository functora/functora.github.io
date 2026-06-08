use crate::*;

#[component]
pub fn Message(message: Signal<Option<String>>) -> Element {
    let text = message.with(|m| m.clone());

    match text {
        Some(text) => rsx! {
            Pre { Quote { "{text}" } }
        },
        None => rsx! {},
    }
}
