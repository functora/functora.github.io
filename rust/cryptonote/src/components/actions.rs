use crate::components::message::{Message, UiMessage};
use crate::i18n::{Language, get_translations};
use dioxus::prelude::*;

#[component]
pub fn ActionRow(
    children: Element,
    message: Signal<Option<UiMessage>>,
) -> Element {
    let language = use_context::<Signal<Language>>();
    let t = get_translations(language());
    let nav = navigator();
    let crate::Route::Root { screen, .. } =
        use_route::<crate::Route>();
    let nav_state =
        use_context::<Signal<crate::NavigationState>>();

    let not_home = screen
        .as_ref()
        .and_then(|s| s.parse::<crate::Screen>().ok())
        .map(|s| s != crate::Screen::Home)
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
