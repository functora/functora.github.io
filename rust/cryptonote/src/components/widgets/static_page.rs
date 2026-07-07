use crate::messages::Msg;
use crate::*;

#[component]
pub fn StaticPage(title: Msg, content: Msg) -> Element {
    let nav = use_context::<Signal<Nav<Route>>>();
    let lang = use_lang();
    rsx! {
        functora_dioxus::widgets::StaticPage {
            title,
            content,
            home_label: Msg::Base(BaseMsg::Home),
            home_route: Screen::Home.to_route(None),
            nav,
            lang,
            children: rsx! {
                Dock {}
            }
        }
    }
}
