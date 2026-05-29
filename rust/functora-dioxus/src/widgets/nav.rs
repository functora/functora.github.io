use crate::traits::NavCtx;
use dioxus::prelude::*;

fn bool_attr(val: bool) -> Option<&'static str> {
    val.then_some("")
}

#[component]
pub fn Breadcrumb<N: NavCtx + PartialEq>(
    title: String,
    home_label: String,
    home_href: String,
    nav_ctx: Signal<N>,
) -> Element {
    rsx! {
        div { font_size: "larger",
            a {
                href: "#",
                onclick: move |evt| {
                    evt.prevent_default();
                    nav_ctx.write().push_route(home_href.clone());
                },
                "{home_label}"
            }
            " ❭ {title}"
        }
    }
}

#[component]
pub fn NavLink<N: NavCtx + PartialEq>(
    href: String,
    children: Element,
    onclick: Option<EventHandler<MouseEvent>>,
    #[props(default)] button: bool,
    #[props(default)] primary: bool,
    nav_ctx: Signal<N>,
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
                nav_ctx.write().push_route(href.clone());
            },
            ..attributes,
            {children}
        }
    }
}
