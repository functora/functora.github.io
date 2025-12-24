use crate::*;

#[component]
pub fn View(note: Option<String>) -> Element {
    let nav = use_app_nav();
    let cfg = use_context::<Signal<AppCfg>>();
    let mut ctx = use_context::<Signal<AppCtx>>();
    let t = get_translations(cfg.read().language);
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
                            note_content
                                .set(Some(text.clone()));
                            ctx.set(AppCtx {
                                content: text,
                                cipher: None,
                                ..Default::default()
                            });
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

                            ctx.set(AppCtx {
                                content: text,
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
                                                        ctx.set(AppCtx {
                                                            content: text,
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

                    Dock { message,
                        Button {
                            icon: FaLockOpen,
                            primary: true,
                            onclick: decrypt_note,
                            "{t.decrypt_button}"
                        }
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

                    Dock { message,
                        Button {
                            icon: FaTrash,
                            onclick: move |_| {
                                ctx.set(AppCtx::default());
                                nav.push(Screen::Home.to_route(None));
                            },
                            "{t.create_new_note}"
                        }
                        Button {
                            icon: FaPenToSquare,
                            primary: true,
                            onclick: move |_| {
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
                Dock { message }
            }
        } else {
            section {
                p { "{t.loading}" }
            }
        }
    }
}
