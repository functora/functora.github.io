use crate::messages::MsgHome;
use crate::storage::AppCfg;
use crate::*;

#[component]
pub fn Breadcrumb<T: I18N + Clone + PartialEq + 'static>(
    title: T,
) -> Element {
    let nav = use_context::<Signal<Nav<Route>>>();
    let lang =
        use_context::<Signal<AppCfg>>().read().language;
    rsx! {
        functora_dioxus::widgets::Breadcrumb {
            title,
            home_label: MsgHome,
            home_href: Screen::Home.to_route(None).to_string(),
            nav,
            lang,
        }
    }
}
