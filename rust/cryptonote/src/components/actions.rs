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
    let route = use_route::<crate::Route>();
    let nav_state =
        use_context::<Signal<crate::NavigationState>>();

    let not_home = !matches!(route, crate::Route::Home {});
    let show_back = not_home && nav_state().has_navigated;

    rsx! {
        p { "txt": "r",
            if show_back {
                button {
                    onclick: move |_| {
                        nav.go_back();
                    },
                    "{t.back_button}"
                }
            }
            {children}
            br {}
            Message { message }
        }
    }
}
