use crate::*;

#[component]
pub fn NavLink(
    route: Route,
    onclick: Option<EventHandler<MouseEvent>>,
    children: Element,
    #[props(extends = button, extends = GlobalAttributes)]
    attributes: Vec<Attribute>,
) -> Element {
    let nav = use_app_navigator();
    rsx! {
        a {
            href: "#",
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
