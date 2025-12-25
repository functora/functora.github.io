use crate::*;

#[component]
pub fn Home() -> Element {
    let nav = use_app_nav();
    let cfg = use_context::<Signal<AppCfg>>();
    let mut ctx = use_context::<Signal<AppCtx>>();
    let t = get_translations(cfg.read().language);

    let mut message =
        use_signal(|| Option::<UiMessage>::None);

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
                nav.push(Screen::View.to_route(Some(note)));
            }
            None => {
                message.set(Some(UiMessage::Error(
                    AppError::NoNoteParam,
                )));
            }
        }
    };

    let mut generate_note = move || {
        message.set(None);

        if ctx.read().cipher.is_some()
            && ctx.read().password.is_empty()
        {
            message.set(Some(UiMessage::Error(
                AppError::PasswordRequired,
            )));
        } else {
            nav.push(Screen::Share.to_route(None));
        }
    };

    let action = ctx.read().action;

    rsx! {
        section {
            fieldset {
                label { "{t.action_label}" }

                input {
                    r#type: "radio",
                    checked: action == ActionMode::Create,
                    onchange: move |_| {
                        message.set(None);
                        ctx.write().action = ActionMode::Create;
                    },
                }
                label {
                    onclick: move |_| {
                        message.set(None);
                        ctx.write().action = ActionMode::Create;
                    },
                    Icon { icon: FaSquarePlus }
                    "{t.action_create}"
                }
                br {}

                input {
                    r#type: "radio",
                    checked: action == ActionMode::Open,
                    onchange: move |_| {
                        message.set(None);
                        ctx.write().action = ActionMode::Open;
                    },
                }
                label {
                    onclick: move |_| {
                        message.set(None);
                        ctx.write().action = ActionMode::Open;
                    },
                    Icon { icon: FaFolderOpen }
                    "{t.action_open}"
                }
                br {}
                br {}

                if action == ActionMode::Create {
                    label { "{t.note}" }
                    textarea {
                        placeholder: "{t.note_placeholder}",
                        rows: "8",
                        value: "{ctx.read().content.clone()}",
                        oninput: move |evt| {
                            ctx.write().content = evt.value();
                        },
                    }

                    label { "{t.mode}" }

                    input {
                        r#type: "radio",
                        checked: ctx.read().cipher.is_some(),
                        onchange: move |_| {
                            ctx.write().cipher = Some(CipherType::ChaCha20Poly1305);
                        },
                    }
                    label {
                        onclick: move |_| {
                            ctx.write().cipher = Some(CipherType::ChaCha20Poly1305);
                        },
                        Icon { icon: FaLock }
                        "{t.password_encryption}"
                    }
                    br {}


                    input {
                        r#type: "radio",
                        checked: ctx.read().cipher.is_none(),
                        onchange: move |_| ctx.write().cipher = None,
                    }
                    label { onclick: move |_| ctx.write().cipher = None,
                        Icon { icon: FaLockOpen }
                        "{t.no_encryption}"
                    }
                    br {}

                    if let Some(cipher) = ctx.read().cipher {
                        br {}
                        label { "{t.cipher}" }

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
                                ctx.write().cipher = Some(new_cipher);
                            },
                            option { value: "aes", "AES-256-GCM" }
                            option { value: "chacha20", "ChaCha20-Poly1305" }
                        }

                        label { "{t.password}" }
                        input {
                            r#type: "password",
                            placeholder: "{t.password_placeholder}",
                            value: "{ctx.read().password}",
                            oninput: move |evt| {
                                ctx.write().password = evt.value();
                            },
                            onkeydown: move |evt| {
                                if evt.key() == Key::Enter {
                                    generate_note()
                                }
                            },
                        }
                    }
                    br {}

                    Dock { message,
                        Button {
                            icon: FaTrash,
                            onclick: move |_| {
                                message.set(None);
                                ctx.set(AppCtx::default());
                            },
                            "{t.create_new_note}"
                        }
                        Button {
                            icon: FaShareNodes,
                            primary: true,
                            onclick: move |_| generate_note(),
                            "{t.generate_button}"
                        }
                    }
                }

                if action == ActionMode::Open {
                    label { "{t.open_url_label}" }
                    textarea {
                        placeholder: "{t.open_url_placeholder}",
                        rows: "6",
                        value: "{url_input}",
                        oninput: move |evt| url_input.set(evt.value()),
                    }
                    br {}

                    Dock { message,
                        Button {
                            icon: FaTrash,
                            onclick: move |_| {
                                message.set(None);
                                ctx.set(AppCtx::default());
                            },
                            "{t.create_new_note}"
                        }
                        Button {
                            icon: FaFolderOpen,
                            primary: true,
                            onclick: open_url,
                            "{t.open_button}"
                        }
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
