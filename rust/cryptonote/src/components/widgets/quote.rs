use crate::*;

#[component]
pub fn Quote(
    children: Element,
    #[props(default)] overflow: bool,
    #[props(extends = code, extends = GlobalAttributes)]
    attributes: Vec<Attribute>,
) -> Element {
    rsx! {
        code { white_space: if overflow { None } else { Some("pre-wrap") }, ..attributes, {children} }
    }
}
