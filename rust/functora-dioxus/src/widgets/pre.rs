use crate::widgets::par::Align;
use dioxus::prelude::*;

#[component]
pub fn Pre(
    children: Element,
    #[props(default)] align: Option<Align>,
    #[props(extends = pre, extends = GlobalAttributes)] attributes: Vec<Attribute>,
) -> Element {
    rsx! {
        pre { "txt": align.map(Align::as_str), ..attributes, {children} }
    }
}
