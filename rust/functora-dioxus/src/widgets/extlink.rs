use crate::widgets::bool_attr;
use dioxus::prelude::*;

#[component]
pub fn ExtLink(
    href: String,
    children: Element,
    #[props(default)] button: bool,
    #[props(default)] primary: bool,
    #[props(extends = a, extends = GlobalAttributes)] attributes: Vec<Attribute>,
) -> Element {
    rsx! {
        a {
            href,
            "btn": bool_attr(button),
            "primary": bool_attr(primary),
            target: "_blank",
            rel: "noopener noreferrer",
            ..attributes,
            {children}
        }
    }
}
