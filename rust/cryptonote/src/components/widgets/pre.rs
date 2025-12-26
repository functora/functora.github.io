use crate::*;

#[component]
pub fn Pre(
    children: Element,
    #[props(default)] overflow: bool,
    #[props(extends = pre, extends = GlobalAttributes)]
    attributes: Vec<Attribute>,
) -> Element {
    rsx! {
        pre { white_space: if overflow { None } else { Some("pre-wrap") }, ..attributes, {children} }
    }
}
