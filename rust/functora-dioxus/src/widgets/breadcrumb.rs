use crate::dioxus_elements;
use crate::i18n::{I18N, Language};
use crate::nav::Nav;
use dioxus::prelude::*;

#[component]
pub fn Breadcrumb<
    R: Routable + Default + PartialEq + 'static,
    T: I18N + Clone + PartialEq + 'static,
    U: I18N + Clone + PartialEq + 'static,
>(
    title: T,
    home_label: U,
    home_href: String,
    nav: Signal<Nav<R>>,
    lang: Language,
) -> Element {
    rsx! {
        card { font_size: "larger",
            a {
                href: "#",
                onclick: move |evt| {
                    evt.prevent_default();
                    nav.write().push_route(&home_href);
                },
                "{home_label.render(lang)}"
            }
            " ❭ {title.render(lang)}"
        }
    }
}
