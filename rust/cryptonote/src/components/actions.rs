use crate::*;

#[component]
pub fn ActionRow(
    children: Element,
    message: Signal<Option<UiMessage>>,
) -> Element {
    let language = use_context::<Signal<Language>>();
    let t = get_translations(language());
    let nav = navigator();
    let Route::Root { screen, .. } = use_route::<Route>();
    let nav_state =
        use_context::<Signal<NavigationState>>();

    let not_home = screen
        .as_ref()
        .and_then(|s| s.parse::<Screen>().ok())
        .map(|s| s != Screen::Home)
        .unwrap_or(false);
    let show_back = not_home && nav_state().has_navigated;

    rsx! {
        p { "txt": "r",
            Message { message }
            if show_back {
                button {
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
