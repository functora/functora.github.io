use crate::*;

#[component]
pub fn Dock(
    children: Element,
    message: Signal<Option<UiMessage>>,
) -> Element {
    let app_settings = use_context::<Signal<AppSettings>>();
    let t = get_translations(app_settings.read().language);
    let nav = use_app_navigator();
    let Route::Root { screen, .. } = use_route::<Route>();

    let not_home = screen
        .as_ref()
        .and_then(|s| s.parse::<Screen>().ok())
        .map(|s| s != Screen::Home)
        .unwrap_or(false);
    let show_back = not_home && (nav.has_navigated)();

    rsx! {
        p { "txt": "r",
            Message { message }
            if show_back {
                Button {
                    icon: FaArrowLeft,
                    onclick: move |_| {
                        nav.go_back();
                    },
                    "{t.back_button}"
                }
            }
            {children}
        }
    }
}
