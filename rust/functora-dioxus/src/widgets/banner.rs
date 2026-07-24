use crate::i18n::{I18N, Language};
use crate::widgets::par::Align;
use crate::widgets::pre::Pre;
use dioxus::prelude::*;

#[component]
pub fn Banner<S, M>(message: S, lang: Language, #[props(default)] align: Option<Align>) -> Element
where
    S: Readable<Target = Option<M>> + Clone + PartialEq + 'static,
    M: I18N + 'static,
{
    message.with(|m| match m {
        Some(msg) => rsx! {
            Pre { align,
                code { "{msg.render(lang)}" }
            }
        },
        None => rsx! {},
    })
}
