use crate::*;

#[component]
pub fn NavLink(
    route: Route,
    onclick: Option<EventHandler<MouseEvent>>,
    children: Element,
    #[props(default)] button: bool,
    #[props(default)] primary: bool,
    #[props(extends = a, extends = GlobalAttributes)]
    attributes: Vec<Attribute>,
) -> Element {
    let nav = use_app_nav();
    rsx! {
        a {
            href: "#",
            "btn": if button { Some("") } else { None },
            "primary": if primary { Some("") } else { None },
            onclick: move |evt| {
                evt.prevent_default();
                if let Some(f) = &onclick {
                    f.call(evt);
                }
                nav.push(route.clone());
            },
            ..attributes,
            {children}
        }
    }
}
