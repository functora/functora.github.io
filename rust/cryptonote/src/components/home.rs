use crate::*;

#[derive(PartialEq, Clone, Copy)]
enum ActionMode {
    Create,
    Open,
}

#[component]
pub fn Home() -> Element {
    let language = use_context::<Signal<Language>>();
    let t = get_translations(language());
    let nav = use_navigator();
    let mut app_context =
        use_context::<Signal<AppContext>>();

    let mut message =
        use_signal(|| Option::<UiMessage>::None);

    let mut action_mode = use_signal(|| ActionMode::Create);
    let mut url_input = use_signal(String::new);

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
                    Signal<NavigationState>,
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

    let generate_note = move |_| {
        message.set(None);

        let (note_content, enc_option, pwd) = {
            let ctx = app_context.read();
            (
                ctx.content.clone().unwrap_or_default(),
                ctx.cipher,
                ctx.password.clone(),
            )
        };

        let note_data = match enc_option {
            None => {
                NoteData::PlainText(note_content.clone())
            }
            Some(cipher) => {
                if pwd.is_empty() {
                    message.set(Some(UiMessage::Error(
                        AppError::PasswordRequired,
                    )));
                    return;
                }
                match encrypt_symmetric(
                    note_content.as_bytes(),
                    &pwd,
                    cipher,
                ) {
                    Ok(encrypted) => {
                        NoteData::CipherText(encrypted)
                    }
                    Err(e) => {
                        message
                            .set(Some(UiMessage::Error(e)));
                        return;
                    }
                }
            }
        };

        let origin = {
            #[cfg(target_arch = "wasm32")]
            {
                web_sys::window().and_then(|w| {
                    let loc = w.location();
                    let protocol = loc.protocol().ok()?;
                    let host = loc.host().ok()?;
                    let pathname = loc.pathname().ok()?;
                    let path =
                        pathname.trim_end_matches('/');
                    Some(format!(
                        "{}//{}{}",
                        protocol, host, path
                    ))
                })
            }
            #[cfg(not(target_arch = "wasm32"))]
            {
                Some(format!(
                    "https://functora.github.io/apps/cryptonote/{}",
                    env!("CARGO_PKG_VERSION")
                ))
            }
        };

        if let Some(origin) = origin {
            let view_url = format!(
                "{}/?screen={}",
                origin,
                Screen::View
            );
            match build_url(&view_url, &note_data) {
                Ok(url) => match generate_qr_code(&url) {
                    Ok(qr) => {
                        app_context.set(AppContext {
                            content: Some(note_content),
                            password: pwd,
                            cipher: enc_option,
                            share_url: Some(url),
                            qr_code: Some(qr),
                        });

                        let mut nav_state = use_context::<
                            Signal<NavigationState>,
                        >(
                        );
                        nav_state.write().has_navigated =
                            true;
                        nav.push(
                            Screen::Share.to_route(None),
                        );
                    }
                    Err(e) => message
                        .set(Some(UiMessage::Error(e))),
                },
                Err(e) => {
                    message.set(Some(UiMessage::Error(e)))
                }
            }
        }
    };

    rsx! {
        section {
            fieldset {

                label { "{t.action_label}" }
                input {
                    r#type: "radio",
                    value: "create",
                    checked: action_mode() == ActionMode::Create,
                    onchange: move |_| {
                        message.set(None);
                        action_mode.set(ActionMode::Create);
                    },
                }
                label {
                    Icon { icon: FaSquarePlus }
                    "{t.action_create}"
                }
                br {}

                input {
                    r#type: "radio",
                    value: "open",
                    checked: action_mode() == ActionMode::Open,
                    onchange: move |_| {
                        message.set(None);
                        action_mode.set(ActionMode::Open);
                    },
                }
                label {
                    Icon { icon: FaFolderOpen }
                    "{t.action_open}"
                }
                br {}
                br {}

                if action_mode() == ActionMode::Create {
                    label { "{t.note}" }
                    textarea {
                        placeholder: "{t.note_placeholder}",
                        rows: "8",
                        value: "{app_context.read().content.clone().unwrap_or_default()}",
                        oninput: move |evt| {
                            let mut ctx = app_context.write();
                            ctx.content = Some(evt.value());
                        },
                    }

                    label { "{t.mode}" }

                    input {
                        r#type: "radio",
                        value: "none",
                        checked: app_context.read().cipher.is_none(),
                        onchange: move |_| {
                            let mut ctx = app_context.write();
                            ctx.cipher = None;
                        },
                    }
                    label {
                        Icon { icon: FaLockOpen }
                        "{t.no_encryption}"
                    }
                    br {}

                    input {
                        r#type: "radio",
                        value: "symmetric",
                        checked: app_context.read().cipher.is_some(),
                        onchange: move |_| {
                            let mut ctx = app_context.write();
                            ctx.cipher = Some(CipherType::ChaCha20Poly1305);
                        },
                    }
                    label {
                        Icon { icon: FaLock }
                        "{t.password_encryption}"
                    }
                    br {}
                    br {}

                    if let Some(cipher) = app_context.read().cipher {
                        label { "{t.algorithm}" }
                        select {
                            value: match cipher {
                                CipherType::ChaCha20Poly1305 => "chacha20",
                                CipherType::Aes256Gcm => "aes",
                            },
                            onchange: move |evt| {
                                let new_cipher = match evt.value().as_str() {
                                    "aes" => CipherType::Aes256Gcm,
                                    _ => CipherType::ChaCha20Poly1305,
                                };
                                let mut ctx = app_context.write();
                                ctx.cipher = Some(new_cipher);
                            },
                            option { value: "chacha20", "ChaCha20-Poly1305" }
                            option { value: "aes", "AES-256-GCM" }
                        }

                        label { "{t.password}" }
                        input {
                            r#type: "password",
                            placeholder: "{t.password_placeholder}",
                            value: "{app_context.read().password}",
                            oninput: move |evt| {
                                let mut ctx = app_context.write();
                                ctx.password = evt.value();
                            },
                        }
                    }
                    br {}

                    ActionRow { message,
                        Button {
                            icon: FaTrash,
                            onclick: move |_| {
                                message.set(None);
                                app_context.set(AppContext::default());
                            },
                            "{t.create_new_note}"
                        }
                        Button {
                            icon: FaShareNodes,
                            primary: true,
                            onclick: generate_note,
                            "{t.generate_button}"
                        }
                    }
                }

                if action_mode() == ActionMode::Open {
                    label { "{t.open_url_label}" }
                    textarea {
                        placeholder: "{t.open_url_placeholder}",
                        rows: "6",
                        value: "{url_input}",
                        oninput: move |evt| url_input.set(evt.value()),
                    }
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
