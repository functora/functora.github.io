use crate::messages::*;
use crate::*;

#[component]
pub fn Dock(children: Element, #[props(default)] message: Option<Signal<Option<Msg>>>) -> Element {
    let nav = use_context::<Signal<Nav<Route>>>();
    let lang = use_lang();

    functora_dioxus::widgets::GenDock(functora_dioxus::widgets::GenDockProps {
        children,
        message,
        nav,
        lang,
        back_button_i18n: Some(Msg::Back),
        back_button_icon: Some(FaArrowLeft),
    })
}
