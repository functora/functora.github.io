use crate::Route;
use crate::components::Breadcrumb;
use crate::components::actions::ActionRow;
use crate::components::message::UiMessage;
use crate::crypto::decrypt_symmetric;
use crate::encoding::{NoteData, parse_url};
use crate::i18n::{Language, get_translations};
use crate::prelude::*;
use web_sys::window;

#[component]
pub fn View() -> Element {
    let language = use_context::<Signal<Language>>();
    let t = get_translations(language());
    let nav = navigator();
    let mut note_content =
        use_signal(|| Option::<String>::None);
    let mut encrypted_data = use_signal(|| {
        Option::<crate::crypto::EncryptedData>::None
    });
    let mut password_input = use_signal(String::new);
    let mut error_message =
        use_signal(|| Option::<UiMessage>::None);
    let mut is_encrypted = use_signal(|| false);

    use_effect(move || {
        if let Some(window) = window() {
            if let Ok(href) = window.location().href() {
                if href.contains("#note=") {
                    match parse_url(&href) {
                        Ok(note_data) => match note_data {
                            NoteData::CipherText(enc) => {
                                is_encrypted.set(true);
                                encrypted_data
                                    .set(Some(enc));
                            }
                            NoteData::PlainText(text) => {
                                note_content
                                    .set(Some(text));
                            }
                        },
                        Err(e) => error_message
                            .set(Some(UiMessage::Error(e))),
                    }
                } else {
                    error_message
                        .set(Some(UiMessage::Error(
                        crate::error::AppError::NoNoteInUrl,
                    )));
                }
            }
        }
    });

    let mut app_context =
        use_context::<Signal<crate::AppContext>>();

    let decrypt_note = move |_evt: Event<MouseData>| {
        error_message.set(None);
        if let Some(enc) = encrypted_data.read().as_ref() {
            let pwd = password_input.read().clone();
            if pwd.is_empty() {
                error_message.set(Some(UiMessage::Error(
                    crate::error::AppError::PasswordRequired,
                )));
                return;
            }

            match decrypt_symmetric(enc, &pwd) {
                Ok(plaintext) => {
                    match String::from_utf8(plaintext) {
                        Ok(text) => {
                            note_content
                                .set(Some(text.clone()));
                            is_encrypted.set(false);

                            app_context.set(
                                crate::AppContext {
                                    content: Some(text),
                                    password: pwd,
                                    cipher: Some(
                                        enc.cipher,
                                    ),
                                    share_url: None,
                                    qr_code: None,
                                },
                            );
                        }
                        Err(e) => error_message.set(Some(UiMessage::Error(
                            crate::error::AppError::Utf8(e),
                        ))),
                    }
                }
                Err(e) => error_message
                    .set(Some(UiMessage::Error(e))),
            }
        }
    };

    rsx! {
        if is_encrypted() {
            Breadcrumb { title: t.encrypted_note.to_string() }
            section {
                fieldset {
                    pre {
                        code { white_space: "pre-wrap", "{t.encrypted_note_desc}" }
                    }

                    label { "{t.password}" }
                    input {
                        r#type: "password",
                        placeholder: "{t.password_placeholder}",
                        value: "{password_input}",
                        oninput: move |evt| password_input.set(evt.value()),
                        onkeydown: move |evt| {
                            if evt.key() == Key::Enter {
                                error_message.set(None);
                                if let Some(enc) = encrypted_data.read().as_ref() {
                                    let pwd = password_input.read().clone();
                                    if !pwd.is_empty() {
                                        match decrypt_symmetric(enc, &pwd) {
                                            Ok(plaintext) => {
                                                match String::from_utf8(plaintext) {
                                                    Ok(text) => {
                                                        note_content.set(Some(text.clone()));
                                                        is_encrypted.set(false);
                                                        app_context
                                                            .set(crate::AppContext {
                                                                content: Some(text),
                                                                password: pwd,
                                                                cipher: Some(enc.cipher),
                                                                share_url: None,
                                                                qr_code: None,
                                                            });
                                                    }
                                                    Err(e) => {
                                                        error_message
                                                            .set(
                                                                Some(UiMessage::Error(crate::error::AppError::Utf8(e))),
                                                            );
                                                    }
                                                }
                                            }
                                            Err(e) => {
                                                error_message.set(Some(UiMessage::Error(e)));
                                            }
                                        }
                                    }
                                }
                            }
                        },
                    }

                    br {}
                    br {}

                    ActionRow { message: error_message,
                        button { "primary": "", onclick: decrypt_note, "{t.decrypt_button}" }
                    }
                }
            }
        } else if let Some(content) = note_content() {
            Breadcrumb { title: t.your_note_title.to_string() }
            section {
                fieldset {
                    article {
                        pre { "{content}" }
                    }

                    ActionRow { message: error_message,
                        button {
                            onclick: move |_| {
                                app_context
                                    .set(crate::AppContext {
                                        content: None,
                                        password: String::new(),
                                        cipher: None,
                                        share_url: None,
                                        qr_code: None,
                                    });
                                nav.push(Route::Home {});
                            },
                            "{t.create_new_note}"
                        }
                        button {
                            onclick: move |_| {
                                if app_context.read().cipher.is_none() {
                                    app_context
                                        .set(crate::AppContext {
                                            content: Some(content.clone()),
                                            password: String::new(),
                                            cipher: None,
                                            share_url: None,
                                            qr_code: None,
                                        });
                                }
                                nav.push(Route::Home {});
                            },
                            "{t.edit_note}"
                        }
                    }
                }
            }
        } else if error_message.read().is_some() {
            Breadcrumb { title: t.error_title.to_string() }
            section {
                ActionRow { message: error_message }
            }
        } else {
            section {
                p { "{t.loading}" }
            }
        }
    }
}
