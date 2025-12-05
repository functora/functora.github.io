use crate::views::message::{Message, UiMessage};
use dioxus::prelude::*;

#[component]
pub fn ActionRow(
    children: Element,
    message: Signal<Option<UiMessage>>,
) -> Element {
    rsx! {
        p {
            {children}
            br {}
            Message { message }
        }
    }
}
