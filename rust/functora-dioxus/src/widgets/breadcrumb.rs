use crate::dioxus_elements;
use crate::i18n::{I18N, Language};
use crate::nav::Nav;
use dioxus::prelude::*;

#[component]
pub fn Breadcrumb<
    R: Routable + Clone + Default + PartialEq + 'static,
    T: I18N + Clone + PartialEq + 'static,
    U: I18N + Clone + PartialEq + 'static,
    N: Writable<Target = Nav<R>> + Clone + PartialEq + 'static,
>(
    title: T,
    home_label: U,
    home_route: R,
    nav: N,
    lang: Language,
) -> Element {
    rsx! {
        card { font_size: "larger",
            a {
                href: "#",
                onclick: move |evt| {
                    evt.prevent_default();
                    nav.write().push(home_route.clone());
                },
                "{home_label.render(lang)}"
            }
            " ❭ {title.render(lang)}"
        }
    }
}
