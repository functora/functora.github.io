use crate::nav::Nav;
use dioxus::prelude::*;

fn bool_attr(val: bool) -> Option<&'static str> {
    val.then_some("")
}

#[component]
pub fn NavLink<R: Routable + Default + PartialEq + 'static>(
    href: String,
    children: Element,
    onclick: Option<EventHandler<MouseEvent>>,
    #[props(default)] button: bool,
    #[props(default)] primary: bool,
    nav: Signal<Nav<R>>,
    #[props(extends = a, extends = GlobalAttributes)] attributes: Vec<Attribute>,
) -> Element {
    rsx! {
        a {
            href: "#",
            "btn": bool_attr(button),
            "primary": bool_attr(primary),
            onclick: move |evt| {
                evt.prevent_default();
                if let Some(f) = &onclick {
                    f.call(evt);
                }
                nav.write().push_route(&href);
            },
            ..attributes,
            {children}
        }
    }
}
