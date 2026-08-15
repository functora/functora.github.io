#![allow(clippy::shadow_reuse)]
use crate::messages::Msg;
use crate::*;

#[component]
pub fn Dock(
    children: Element,
    #[props(default)] message: Option<Signal<Option<Msg>>>,
    #[props(default)] back_button_hide: bool,
) -> Element {
    let nav = use_context::<Signal<Nav<Route>>>();
    let lang = use_lang();

    rsx! {
        functora_dioxus::widgets::GenDock::<Route,Msg,Msg,FaArrowLeft,Signal<Nav<Route>>,Signal<Option<Msg>>> {
            children,
            nav,
            message,
            lang,
            back_button_i18n: Some(Msg::Base(BaseMsg::Back)),
            back_button_icon: Some(FaArrowLeft),
            back_button_hide,
        }
    }
}
