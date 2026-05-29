use dioxus::prelude::*;
use dioxus_free_icons::Icon;

#[component]
pub fn Button<T: dioxus_free_icons::IconShape + Clone + PartialEq + 'static>(
    #[props(default)] icon: Option<T>,
    children: Element,
    onclick: Option<EventHandler<MouseEvent>>,
    #[props(default)] primary: bool,
    #[props(extends = button, extends = GlobalAttributes)] attributes: Vec<Attribute>,
) -> Element {
    rsx! {
        button {
            onclick: move |evt| {
                if let Some(handler) = &onclick {
                    handler.call(evt);
                }
            },
            "primary": if primary { Some("") } else { None },
            ..attributes,
            {icon.map(|i| rsx! { Icon { icon: i } })}
            {children}
        }
    }
}
