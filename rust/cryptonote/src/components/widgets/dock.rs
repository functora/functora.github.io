use crate::messages::*;
use crate::*;

#[component]
pub fn Dock(
    children: Element,
    message: Option<Signal<Option<Msg>>>,
) -> Element {
    let nav = use_context::<Signal<Nav<Route>>>();
    let lang = use_lang();

    rsx! {
        functora_dioxus::widgets::Dock {
            children,
            message,
            nav,
            lang,
        }
    }
}
