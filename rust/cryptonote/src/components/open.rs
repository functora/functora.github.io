use crate::Screen;
use crate::components::Breadcrumb;
use crate::components::actions::ActionRow;
use crate::components::message::UiMessage;
use crate::error::AppError;
use crate::i18n::{Language, get_translations};
use crate::prelude::*;

#[component]
pub fn Open() -> Element {
    let language = use_context::<Signal<Language>>();
    let t = get_translations(language());
    let nav = navigator();

    let mut url_input = use_signal(String::new);
    let mut message =
        use_signal(|| Option::<UiMessage>::None);

    let open_url = move |_| {
        message.set(None);
        let url = url_input.read().trim().to_string();

        if url.is_empty() {
            message.set(Some(UiMessage::Error(
                AppError::NoNoteInUrl,
            )));
            return;
        }

        match extract_note_param(&url) {
            Some(note) => {
                let mut nav_state = use_context::<
                    Signal<crate::NavigationState>,
                >();
                nav_state.write().has_navigated = true;
                nav.push(Screen::View.to_route(Some(note)));
            }
            None => {
                message.set(Some(UiMessage::Error(
                    AppError::NoNoteParam,
                )));
            }
        }
    };

    rsx! {
        Breadcrumb { title: t.open_title.to_string() }
        section {
            fieldset {
                label { "{t.open_url_label}" }
                textarea {
                    placeholder: "{t.open_url_placeholder}",
                    rows: "6",
                    value: "{url_input}",
                    oninput: move |evt| url_input.set(evt.value()),
                }

                br {}
                br {}

                ActionRow { message,
                    button {
                        onclick: move |_| {
                            url_input.set(String::new());
                            message.set(None);
                        },
                        "{t.create_new_note}"
                    }
                    button { "primary": "", onclick: open_url, "{t.open_button}" }
                }
            }
        }
    }
}

fn extract_note_param(url: &str) -> Option<String> {
    url.split('?').nth(1).and_then(|query| {
        query.split('&').find_map(|param| {
            let mut parts = param.split('=');
            match (parts.next(), parts.next()) {
                (Some("note"), Some(value)) => {
                    Some(value.to_string())
                }
                _ => None,
            }
        })
    })
}
