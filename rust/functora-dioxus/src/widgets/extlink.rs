use dioxus::prelude::*;

fn bool_attr(val: bool) -> Option<&'static str> {
    val.then_some("")
}

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
