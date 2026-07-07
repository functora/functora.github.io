use crate::widgets::overflow_style;
use dioxus::prelude::*;

#[component]
pub fn Pre(
    children: Element,
    #[props(default)] overflow: bool,
    #[props(extends = pre, extends = GlobalAttributes)] attributes: Vec<Attribute>,
) -> Element {
    rsx! {
        pre { white_space: overflow_style(overflow), ..attributes, {children} }
    }
}
