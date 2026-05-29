use dioxus::prelude::*;

#[component]
pub fn Message(message: Signal<Option<String>>) -> Element {
    message.with(|m| match m {
        Some(text) => rsx! {
            pre {
                code { "{text}" }
            }
        },
        None => rsx! {},
    })
}
