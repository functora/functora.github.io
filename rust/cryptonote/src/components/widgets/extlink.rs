use crate::*;

#[component]
pub fn ExtLink(
    href: String,
    children: Element,
    #[props(extends = button, extends = GlobalAttributes)]
    attributes: Vec<Attribute>,
) -> Element {
    rsx! {
        a {
            href,
            target: "_blank",
            rel: "noopener noreferrer",
            ..attributes,
            {children}
        }
    }
}
