use crate::*;

#[component]
pub fn View(note: Option<String>) -> Element {
    let language = use_context::<Signal<Language>>();
    let t = get_translations(language());
    let nav = navigator();
    let mut note_content =
        use_signal(|| Option::<String>::None);
    let mut encrypted_data =
        use_signal(|| Option::<EncryptedData>::None);
    let mut password_input = use_signal(String::new);
    let mut message =
        use_signal(|| Option::<UiMessage>::None);
    let mut is_encrypted = use_signal(|| false);

    use_effect(move || {
        if let Some(n) = &note {
            if !n.is_empty() {
                match encoding::decode_note(n) {
                    Ok(note_data) => match note_data {
                        NoteData::CipherText(enc) => {
                            is_encrypted.set(true);
                            encrypted_data.set(Some(enc));
                        }
                        NoteData::PlainText(text) => {
                            note_content.set(Some(text));
                        }
                    },
                    Err(e) => message
                        .set(Some(UiMessage::Error(e))),
                }
            } else {
                message.set(Some(UiMessage::Error(
                    AppError::NoNoteInUrl,
                )));
            }
        } else {
            message.set(Some(UiMessage::Error(
                AppError::NoNoteInUrl,
            )));
        }
    });

    let mut app_context =
        use_context::<Signal<AppContext>>();

    let decrypt_note = move |_evt: Event<MouseData>| {
        message.set(None);
        if let Some(enc) = encrypted_data.read().as_ref() {
            let pwd = password_input.read().clone();
            if pwd.is_empty() {
                message.set(Some(UiMessage::Error(
                    AppError::PasswordRequired,
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

                            app_context.set(AppContext {
                                content: Some(text),
                                password: pwd,
                                cipher: Some(enc.cipher),
                                ..Default::default()
                            });
                        }
                        Err(e) => message.set(Some(
                            UiMessage::Error(
                                AppError::Utf8(e),
                            ),
                        )),
                    }
                }
                Err(e) => {
                    message.set(Some(UiMessage::Error(e)))
                }
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
                                message.set(None);
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
                                                            .set(AppContext {
                                                                content: Some(text),
                                                                password: pwd,
                                                                cipher: Some(enc.cipher),
                                                                ..Default::default()
                                                            });
                                                    }
                                                    Err(e) => {
                                                        message.set(Some(UiMessage::Error(AppError::Utf8(e))));
                                                    }
                                                }
                                            }
                                            Err(e) => {
                                                message.set(Some(UiMessage::Error(e)));
                                            }
                                        }
                                    }
                                }
                            }
                        },
                    }

                    br {}

                    ActionRow { message,
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

                    ActionRow { message,
                        button {
                            onclick: move |_| {
                                app_context.set(AppContext::default());
                                nav.push(Screen::Home.to_route(None));
                            },
                            "{t.create_new_note}"
                        }
                        button {
                            "primary": "",
                            onclick: move |_| {
                                if app_context.read().cipher.is_none() {
                                    app_context
                                        .set(AppContext {
                                            content: Some(content.clone()),
                                            ..Default::default()
                                        });
                                }
                                nav.push(Screen::Home.to_route(None));
                            },
                            "{t.edit_note}"
                        }
                    }
                }
            }
        } else if message.read().is_some() {
            Breadcrumb { title: t.error_title.to_string() }
            section {
                ActionRow { message }
            }
        } else {
            section {
                p { "{t.loading}" }
            }
        }
    }
}
