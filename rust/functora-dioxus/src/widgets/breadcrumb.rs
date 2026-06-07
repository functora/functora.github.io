use crate::dioxus_elements;
use crate::i18n::{I18N, Language};
use crate::traits::NavCtx;
use dioxus::prelude::*;

#[component]
pub fn Breadcrumb<
    N: NavCtx + PartialEq,
    T: I18N + Clone + PartialEq + 'static,
    U: I18N + Clone + PartialEq + 'static,
>(
    title: T,
    home_label: U,
    home_href: String,
    nav_ctx: Signal<N>,
    lang: Language,
) -> Element {
    rsx! {
        card { font_size: "larger",
            a {
                href: "#",
                onclick: move |evt| {
                    evt.prevent_default();
                    nav_ctx.write().push_route(home_href.clone());
                },
                "{home_label.render(lang)}"
            }
            " ❭ {title.render(lang)}"
        }
    }
}
