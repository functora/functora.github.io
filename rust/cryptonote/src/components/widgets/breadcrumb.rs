use crate::*;

#[component]
pub fn Breadcrumb(title: String) -> Element {
    let t = use_translations();
    let nav_ctx = use_context::<Signal<AppNav>>();
    rsx! {
        functora_dioxus::widgets::Breadcrumb {
            title,
            home_label: t.home.to_string(),
            home_href: Screen::Home.to_route(None).to_string(),
            nav_ctx,
        }
    }
}
