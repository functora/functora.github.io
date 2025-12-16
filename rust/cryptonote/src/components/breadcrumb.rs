use crate::*;

#[derive(Props, Clone, PartialEq)]
pub struct BreadcrumbProps {
    title: String,
}

#[component]
pub fn Breadcrumb(props: BreadcrumbProps) -> Element {
    let language = use_context::<Signal<Language>>();
    let t = get_translations(language());

    rsx! {
        card { font_size: "larger",
            Link { to: Screen::Home.to_route(None), "{t.home}" }
            " ❭ {props.title}"
        }
    }
}
