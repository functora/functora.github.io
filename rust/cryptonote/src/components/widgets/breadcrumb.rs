use crate::*;

#[derive(Props, Clone, PartialEq)]
pub struct BreadcrumbProps {
    title: String,
}

#[component]
pub fn Breadcrumb(props: BreadcrumbProps) -> Element {
    let cfg = use_context::<Signal<AppCfg>>();
    let t = get_translations(cfg.read().language);
    rsx! {
        card { font_size: "larger",
            NavLink { route: Screen::Home.to_route(None), "{t.home}" }
            " ❭ {props.title}"
        }
    }
}
