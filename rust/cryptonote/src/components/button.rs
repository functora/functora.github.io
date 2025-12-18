use crate::*;

#[component]
pub fn Button<T>(
    icon: T,
    children: Element,
    onclick: Option<EventHandler<MouseEvent>>,
    #[props(default)] primary: bool,
    #[props(extends = button, extends = GlobalAttributes)]
    attributes: Vec<Attribute>,
) -> Element
where
    T: IconShape + Clone + PartialEq + 'static,
{
    rsx! {
        button {
            onclick: move |evt| {
                if let Some(handler) = &onclick {
                    handler.call(evt);
                }
            },
            "primary": if primary { Some("") } else { None },
            ..attributes,
            Icon { icon }
            {children}
        }
    }
}
