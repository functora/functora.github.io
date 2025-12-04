use crate::i18n::{Language, get_translations};
use crate::prelude::*;

#[derive(Props, Clone, PartialEq)]
pub struct BreadcrumbProps {
    title: String,
}

#[component]
pub fn Breadcrumb(props: BreadcrumbProps) -> Element {
    let language = use_context::<Signal<Language>>();
    let t = get_translations(language());
    let nav = navigator();

    rsx! {
        card { font_size: "larger",
            a {
                href: "#",
                onclick: move |_| {
                    nav.push("/");
                },
                "{t.home}"
            }
            " ❭ {props.title}"
        }
    }
}
