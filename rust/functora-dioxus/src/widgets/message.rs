use crate::i18n::{I18N, Language};
use crate::widgets::pre::Pre;
use crate::widgets::quote::Quote;
use dioxus::prelude::*;

#[component]
pub fn Message<M: I18N + Clone + PartialEq + 'static>(message: Signal<Option<M>>, lang: Language) -> Element {
    message.with(|m| match m {
        Some(msg) => rsx! {
            Pre {
                Quote { "{msg.render(lang)}" }
            }
        },
        None => rsx! {},
    })
}
