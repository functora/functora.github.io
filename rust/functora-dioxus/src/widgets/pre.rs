use crate::widgets::overflow_style;
use crate::widgets::par::Align;
use dioxus::prelude::*;

#[component]
pub fn Pre(
    children: Element,
    #[props(default)] overflow: bool,
    #[props(default)] align: Option<Align>,
    #[props(extends = pre, extends = GlobalAttributes)] attributes: Vec<Attribute>,
) -> Element {
    rsx! {
        pre {
            white_space: overflow_style(overflow),
            "txt": align.map(Align::as_str),
            ..attributes,
            {children}
        }
    }
}
