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
        functora_dioxus::widgets::GenDock::<Route, Msg, Msg> {
            children,
            message,
            nav,
            back_button_i18n: Some(Msg::Back),
            back_button_icon: Some(rsx! { Icon { icon: FaArrowLeft } }),
            lang,
        }
    }
}
