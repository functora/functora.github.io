use crate::i18n::{I18N, Language};
use crate::widgets::pre::Pre;
use crate::widgets::quote::Quote;
use dioxus::prelude::*;

#[component]
pub fn Banner<S, M>(message: S, lang: Language) -> Element
where
    S: Readable<Target = Option<M>> + Clone + PartialEq + 'static,
    M: I18N + 'static,
{
    message.with(|m| match m {
        Some(msg) => rsx! {
            Pre {
                Quote { "{msg.render(lang)}" }
            }
        },
        None => rsx! {},
    })
}
