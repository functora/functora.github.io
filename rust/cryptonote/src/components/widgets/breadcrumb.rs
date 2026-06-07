use crate::messages::MsgHome;
use crate::storage::AppCfg;
use crate::*;
use functora_dioxus::i18n::I18N;

#[component]
pub fn Breadcrumb<T: I18N + Clone + PartialEq + 'static>(
    title: T,
) -> Element {
    let nav_ctx = use_context::<Signal<AppNav>>();
    let lang =
        use_context::<Signal<AppCfg>>().read().language;
    rsx! {
        functora_dioxus::widgets::Breadcrumb {
            title,
            home_label: MsgHome,
            home_href: Screen::Home.to_route(None).to_string(),
            nav_ctx,
            lang,
        }
    }
}
