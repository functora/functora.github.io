use crate::*;

#[derive(Props, Clone, PartialEq)]
pub struct BreadcrumbProps {
    title: String,
}

#[component]
pub fn Breadcrumb(props: BreadcrumbProps) -> Element {
    let app_settings = use_context::<Signal<AppSettings>>();
    let t = get_translations(app_settings.read().language);
    let nav = use_app_navigator();

    rsx! {
        card { font_size: "larger",
            a {
                onclick: move |evt| {
                    evt.prevent_default();
                    nav.push(Screen::Home.to_route(None));
                },
                "{t.home}"
            }
            " ❭ {props.title}"
        }
    }
}
