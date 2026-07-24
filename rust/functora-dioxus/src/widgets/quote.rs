use dioxus::prelude::*;

#[component]
pub fn Quote(
    children: Element,
    #[props(extends = code, extends = GlobalAttributes)] attributes: Vec<Attribute>,
) -> Element {
    rsx! {
        code { ..attributes,{children} }
    }
}
