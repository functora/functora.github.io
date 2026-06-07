use crate::dioxus_elements;
use crate::traits::NavCtx;
use dioxus::prelude::*;

#[component]
pub fn Breadcrumb<N: NavCtx + PartialEq>(
    title: String,
    home_label: String,
    home_href: String,
    nav_ctx: Signal<N>,
) -> Element {
    rsx! {
        card { font_size: "larger",
            a {
                href: "#",
                onclick: move |evt| {
                    evt.prevent_default();
                    nav_ctx.write().push_route(home_href.clone());
                },
                "{home_label}"
            }
            " ❭ {title}"
        }
    }
}
