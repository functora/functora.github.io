use crate::*;

#[derive(Props, Clone, PartialEq)]
pub struct BreadcrumbProps {
    title: String,
}

#[component]
pub fn Breadcrumb(props: BreadcrumbProps) -> Element {
    let app_settings = use_context::<Signal<AppCfg>>();
    let t = get_translations(app_settings.read().language);
    rsx! {
        card { font_size: "larger",
            NavLink { route: Screen::Home.to_route(None), "{t.home}" }
            " ❭ {props.title}"
        }
    }
}
