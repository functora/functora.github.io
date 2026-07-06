use crate::widgets::overflow_style;
use dioxus::prelude::*;

#[component]
pub fn Quote(
    children: Element,
    #[props(default)] overflow: bool,
    #[props(extends = code, extends = GlobalAttributes)] attributes: Vec<Attribute>,
) -> Element {
    rsx! {
        code { white_space: overflow_style(overflow), ..attributes, {children} }
    }
}
