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
    let show_back = !matches!(route, crate::Route::Home {});

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
