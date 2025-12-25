use crate::*;

#[component]
pub fn ExtLink(
    href: String,
    children: Element,
    #[props(default)] button: bool,
    #[props(default)] primary: bool,
    #[props(extends = button, extends = GlobalAttributes)]
    attributes: Vec<Attribute>,
) -> Element {
    rsx! {
        a {
            href,
            "btn": if button { Some("") } else { None },
            "primary": if primary { Some("") } else { None },
            target: "_blank",
            rel: "noopener noreferrer",
            ..attributes,
            {children}
        }
    }
}
