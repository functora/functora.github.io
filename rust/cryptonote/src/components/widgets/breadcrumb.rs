use crate::messages::Msg;
use crate::*;

#[component]
pub fn Breadcrumb<T: I18N + Clone + PartialEq + 'static>(title: T) -> Element {
    let nav = use_context::<Signal<Nav<Route>>>();
    let lang = use_lang();
    rsx! {
        functora_dioxus::widgets::Breadcrumb {
            title,
            home_label: Msg::Base(BaseMsg::Home),
            home_route: Screen::Home.to_route(None),
            nav,
            lang,
        }
    }
}
