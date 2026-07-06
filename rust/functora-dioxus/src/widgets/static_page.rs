use crate::i18n::{I18N, Language};
use crate::nav::Nav;
use crate::widgets::{Breadcrumb, Pre};
use dioxus::prelude::*;

#[component]
pub fn GenStaticPage<
    R: Routable + Clone + Default + PartialEq + 'static,
    T: I18N + Clone + PartialEq + 'static,
    U: I18N + Clone + PartialEq + 'static,
    N: Writable<Target = Nav<R>> + Clone + PartialEq + 'static,
>(
    title: T,
    content: U,
    home_label: U,
    home_route: R,
    nav: N,
    lang: Language,
    children: Element,
) -> Element {
    rsx! {
        Breadcrumb { title, home_label, home_route, nav, lang }
        section {
            Pre { "{content.render(lang)}" }
            {children}
        }
    }
}

#[component]
pub fn StaticPage<
    R: Routable + Clone + Default + PartialEq + 'static,
    T: I18N + Clone + PartialEq + 'static,
    U: I18N + Clone + PartialEq + 'static,
    N: Writable<Target = Nav<R>> + Clone + PartialEq + 'static,
>(
    title: T,
    content: U,
    home_label: U,
    home_route: R,
    nav: N,
    lang: Language,
    children: Element,
) -> Element {
    GenStaticPage(GenStaticPageProps {
        title,
        content,
        home_label,
        home_route,
        nav,
        lang,
        children,
    })
}
