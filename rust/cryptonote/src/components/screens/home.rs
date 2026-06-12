use crate::messages::*;
use crate::*;

#[component]
pub fn Home() -> Element {
    let mut nav = use_context::<Signal<Nav<Route>>>();
    let mut ctx = use_context::<Signal<AppCtx>>();
    let lang = use_lang();

    let mut message = use_signal(|| Option::<String>::None);

    let mut url_input = use_signal(String::new);

    let open_url = move |_| {
        message.set(None);
        let url = url_input.read().trim().to_string();

        if url.is_empty() {
            message.set(Some(
                AppError::NoNoteInUrl.render(lang),
            ));
            return;
        }

        match extract_note_param(&url) {
            Ok(note) => {
                nav.write().push(
                    Screen::View.to_route(Some(note)),
                );
            }
            Err(e) => {
                message.set(Some(e.render(lang)));
            }
        }
    };

    let mut generate_note = move || {
        message.set(None);

        if ctx.read().cipher.is_some()
            && ctx.read().password.is_empty()
        {
            message.set(Some(
                AppError::PasswordRequired.render(lang),
            ));
        } else {
            nav.write().push(Screen::Share.to_route(None));
        }
    };

    let set_action = move |mode: ActionMode| {
        message.set(None);
        ctx.write().action = mode;
    };

    let reset_ctx = move |_| {
        message.set(None);
        let action = ctx.read().action;
        ctx.set(AppCtx::default());
        ctx.write().action = action;
        url_input.set(String::new());
    };

    let action = ctx.read().action;

    rsx! {
            section {
                fieldset {
                    label { "{Msg::ActionLabel.render(lang)}" }

                    {
                        rsx! {
                            ActionRadio {
                                action,
                                mode: ActionMode::Create,
                                icon: FaSquarePlus,
                                label: Msg::ActionCreate.render(lang),
                                on_change: set_action,
                            }
                            ActionRadio {
                                action,
                                mode: ActionMode::Open,
                                icon: FaFolderOpen,
                                label: Msg::ActionOpen.render(lang),
                                on_change: set_action,
                            }
                            ActionRadio {
                                action,
                                mode: ActionMode::Scan,
                                icon: FaQrcode,
                                label: Msg::ActionScan.render(lang),
                                on_change: set_action,
                            }
                        }
                    }
                    br {}

                    if action == ActionMode::Create {
                        label { "{Msg::Mode.render(lang)}" }
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
                            "{Msg::PasswordEncryption.render(lang)}"
                        }
                        br {}

                        input {
                            r#type: "radio",
                            checked: ctx.read().cipher.is_none(),
                            onchange: move |_| ctx.write().cipher = None,
                        }
                        label { onclick: move |_| ctx.write().cipher = None,
                            Icon { icon: FaLockOpen }
                            "{Msg::NoEncryption.render(lang)}"
                        }
                        br {}

                        if let Some(cipher) = ctx.read().cipher {
                            br {}
                            label { "{Msg::Cipher.render(lang)}" }

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

                            label { "{Msg::Password.render(lang)}" }
                            input {
                                r#type: "password",
                                placeholder: "{Msg::PasswordPlaceholder.render(lang)}",
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

                        label { "{Msg::Note.render(lang)}" }
                        textarea {
                            placeholder: "{Msg::NotePlaceholder.render(lang)}",
                            rows: "8",
                            value: "{ctx.read().content.clone()}",
                            oninput: move |evt| {
                                ctx.write().content = evt.value();
                            },
                        }

                        Dock { message,
                            Button { icon: FaTrash, onclick: reset_ctx, "{Msg::CreateNewNote.render(lang)}" }
                            Button {
                                icon: FaEye,
                                onclick: move |_| {
                                    nav.write().push(Screen::View.to_route(None));
                                },
                                "{Msg::ViewButton.render(lang)}"
                            }
                            Button {
                                icon: FaPaste,
                                onclick: move |_| {
                                    spawn(async move {
                                        match js_read_clipboard().await {
                                            Ok(text) => ctx.write().content = text,
    Err(e) => {
                        message.set(Some(AppError::Fd(e).render(lang)))
                    }
                                        }
                                    });
                                },
                                "{Msg::PasteButton.render(lang)}"
                            }
                            Button {
                                icon: FaShareNodes,
                                primary: true,
                                onclick: move |_| generate_note(),
                                "{Msg::GenerateButton.render(lang)}"
                            }
                        }
                    }

                    if action == ActionMode::Open {
                        label { "{Msg::OpenUrlLabel.render(lang)}" }
                        textarea {
                            placeholder: "{Msg::OpenUrlPlaceholder.render(lang)}",
                            rows: "6",
                            value: "{url_input}",
                            oninput: move |evt| url_input.set(evt.value()),
                        }
                        br {}

                        Dock { message,
                            Button { icon: FaTrash, onclick: reset_ctx, "{Msg::CreateNewNote.render(lang)}" }
                            Button {
                                icon: FaPaste,
                                onclick: move |_| {
                                    spawn(async move {
                                        match js_read_clipboard().await {
                                            Ok(text) => url_input.set(text),
    Err(e) => {
                        message.set(Some(AppError::Fd(e).render(lang)))
                    }
                                        }
                                    });
                                },
                                "{Msg::PasteButton.render(lang)}"
                            }
                            Button {
                                icon: FaFolderOpen,
                                primary: true,
                                onclick: open_url,
                                "{Msg::OpenButton.render(lang)}"
                            }
                        }
                    }

                    if action == ActionMode::Scan {
                        QrScanner {
                            lang,
                            on_scan: Callback::new(move |url: String| {
                                match extract_note_param(&url) {
                                    Ok(note) => {
                                        nav.write().push(Screen::View.to_route(Some(note)));
                                    }
                                    Err(e) => {
                                        message.set(Some(e.render(lang)));
                                    }
                                }
                            }),
                        }
                    }
                }
            }
        }
}

#[component]
fn ActionRadio<
    T: IconShape + Clone + PartialEq + 'static,
>(
    action: ActionMode,
    mode: ActionMode,
    icon: T,
    label: String,
    on_change: EventHandler<ActionMode>,
) -> Element {
    rsx! {
        input {
            r#type: "radio",
            checked: action == mode,
            onchange: move |_| on_change.call(mode),
        }
        label { onclick: move |_| on_change.call(mode),
            Icon { icon }
            "{label}"
        }
        br {}
    }
}
