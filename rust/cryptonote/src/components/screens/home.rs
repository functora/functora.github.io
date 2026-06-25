use crate::messages::*;
use crate::*;

#[component]
pub fn Home() -> Element {
    let mut nav = use_context::<Signal<Nav<Route>>>();
    let mut tst = use_context::<Store<TemporaryState>>();
    let lang = use_lang();

    let mut message = use_message();

    let open_url = move |_| {
        message.set(None);
        let url = tst.home().url_input()();
        let url = url.trim().to_string();

        if url.is_empty() {
            message.set(Some(Msg::Error(AppError::NoNoteInUrl.render(lang))));
            return;
        }

        match extract_note_param(&url) {
            Ok(note) => {
                nav.write().push(Screen::View.to_route(Some(note)));
            }
            Err(e) => {
                message.set(Some(Msg::Error(e.render(lang))));
            }
        }
    };

    let mut generate_note = move || {
        message.set(None);

        if tst.cipher().is_some() && tst.password()().is_empty() {
            message.set(Some(Msg::PasswordRequired));
        } else {
            nav.write().push(Screen::Share.to_route(None));
        }
    };

    let set_action = move |mode: ActionMode| {
        message.set(None);
        tst.action().set(mode);
    };

    let reset_ctx = move |_| {
        message.set(None);
        let action = tst.action()();
        tst.set(TemporaryState::default());
        tst.action().set(action);
    };

    let action = tst.action()();

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
                        checked: tst.cipher().is_some(),
                        onchange: move |_| {
                            tst.cipher().set(Some(CipherType::ChaCha20Poly1305));
                        },
                    }
                    label {
                        onclick: move |_| {
                            tst.cipher().set(Some(CipherType::ChaCha20Poly1305));
                        },
                        Icon { icon: FaLock }
                        "{Msg::PasswordEncryption.render(lang)}"
                    }
                    br {}

                    input {
                        r#type: "radio",
                        checked: tst.cipher().is_none(),
                        onchange: move |_| tst.cipher().set(None),
                    }
                    label { onclick: move |_| tst.cipher().set(None),
                        Icon { icon: FaLockOpen }
                        "{Msg::NoEncryption.render(lang)}"
                    }
                    br {}

                    if let Some(cipher) = tst.cipher().transpose() {
                        br {}
                        label { "{Msg::Cipher.render(lang)}" }

                        select {
                            value: match *cipher.read() {
                                CipherType::ChaCha20Poly1305 => "chacha20",
                                CipherType::Aes256Gcm => "aes",
                            },
                            onchange: move |evt| {
                                let new_cipher = match evt.value().as_str() {
                                    "aes" => CipherType::Aes256Gcm,
                                    _ => CipherType::ChaCha20Poly1305,
                                };
                                tst.cipher().set(Some(new_cipher));
                            },
                            option { value: "aes", "AES-256-GCM" }
                            option { value: "chacha20", "ChaCha20-Poly1305" }
                        }

                        label { "{Msg::Password.render(lang)}" }
                        input {
                            r#type: "password",
                            placeholder: "{Msg::PasswordPlaceholder.render(lang)}",
                            value: "{tst.password()}",
                            oninput: move |evt| {
                                tst.password().set(evt.value());
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
                        value: "{tst.content()}",
                        oninput: move |evt| {
                            tst.content().set(evt.value());
                        },
                    }

                    Dock { message,
                        Button {
                            icon: Some(FaTrash),
                            onclick: reset_ctx,
                            i18n: Some(Msg::CreateNewNote),
                            lang,
                        }
                        Button {
                            icon: Some(FaEye),
                            onclick: move |_| {
                                nav.write().push(Screen::View.to_route(None));
                            },
                            i18n: Some(Msg::ViewButton),
                            lang,
                        }
                        Button {
                            icon: Some(FaPaste),
                            onclick: move |_| {
                                paste_clipboard(move |text| tst.content().set(text), message, lang);
                            },
                            i18n: Some(Msg::Paste),
                            lang,
                        }
                        Button {
                            icon: Some(FaShareNodes),
                            primary: true,
                            onclick: move |_| generate_note(),
                            i18n: Some(Msg::GenerateButton),
                            lang,
                        }
                    }
                }

                if action == ActionMode::Open {
                    label { "{Msg::OpenUrlLabel.render(lang)}" }
                    textarea {
                        placeholder: "{Msg::OpenUrlPlaceholder.render(lang)}",
                        rows: "6",
                        value: "{tst.home().url_input()}",
                        oninput: move |evt| tst.home().url_input().set(evt.value()),
                    }
                    br {}

                    Dock { message,
                        Button {
                            icon: Some(FaTrash),
                            onclick: reset_ctx,
                            i18n: Some(Msg::CreateNewNote),
                            lang,
                        }
                        Button {
                            icon: Some(FaPaste),
                            onclick: move |_| {
                                paste_clipboard(move |text| tst.home().url_input().set(text), message, lang);
                            },
                            i18n: Some(Msg::Paste),
                            lang,
                        }
                        Button {
                            icon: Some(FaFolderOpen),
                            primary: true,
                            onclick: open_url,
                            i18n: Some(Msg::OpenButton),
                            lang,
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
                                    message.set(Some(Msg::Error(e.render(lang))));
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
fn ActionRadio<T: IconShape + Clone + PartialEq + 'static>(
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
