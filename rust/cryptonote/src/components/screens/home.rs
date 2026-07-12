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
            message.set(Some(Msg::Error(
                AppError::NoNoteInUrl,
            )));
            return;
        }

        match extract_note_param(&url) {
            Ok(note) => nav
                .write()
                .push(Screen::View.to_route(Some(note))),
            Err(e) => message.set(Some(Msg::Error(e))),
        }
    };

    let mut generate_note = move || {
        message.set(None);

        if tst.cipher().is_some()
            && tst.password()().is_empty()
        {
            message.set(Some(Msg::Base(
                BaseMsg::PasswordRequired,
            )));
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
                legend { "{Msg::ActionLabel.render(lang)}" }
                ActionRadio {
                    action,
                    mode: ActionMode::Create,
                    icon: FaSquarePlus,
                    label: Msg::ActionCreate.render(lang),
                    on_change: set_action,
                }
                br {}
                ActionRadio {
                    action,
                    mode: ActionMode::Open,
                    icon: FaFolderOpen,
                    label: Msg::ActionOpen.render(lang),
                    on_change: set_action,
                }
                br {}
                ActionRadio {
                    action,
                    mode: ActionMode::Scan,
                    icon: FaQrcode,
                    label: Msg::ActionScan.render(lang),
                    on_change: set_action,
                }

            }

            if action == ActionMode::Create {
                fieldset {
                    legend { "{Msg::Mode.render(lang)}" }
                    CipherRadio {
                        cipher: tst.cipher()(),
                        value: None,
                        icon: FaLockOpen,
                        label: Msg::NoEncryption.render(lang),
                        on_change: move |_| tst.cipher().set(None),
                    }
                    br {}
                    CipherRadio {
                        cipher: tst.cipher()(),
                        value: Some(CipherType::Aes256Gcm),
                        icon: FaLock,
                        label: format!("AES-256-GCM {}", Msg::EncryptionSuffix.render(lang)),
                        on_change: move |_| tst.cipher().set(Some(CipherType::Aes256Gcm)),
                    }
                    br {}
                    CipherRadio {
                        cipher: tst.cipher()(),
                        value: Some(CipherType::ChaCha20Poly1305),
                        icon: FaLock,
                        label: format!("ChaCha20-Poly1305 {}", Msg::EncryptionSuffix.render(lang)),
                        on_change: move |_| tst.cipher().set(Some(CipherType::ChaCha20Poly1305)),
                    }
                }

                if tst.cipher().is_some() {
                    label { "{Msg::Base(BaseMsg::Password).render(lang)}" }
                    input {
                        r#type: "password",
                        placeholder: "{Msg::Base(BaseMsg::PasswordPlaceholder).render(lang)}",
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
                            read_clipboard(move |text| tst.content().set(text), message);
                        },
                        i18n: Some(Msg::Base(BaseMsg::Paste)),
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
                            read_clipboard(move |text| tst.home().url_input().set(text), message);
                        },
                        i18n: Some(Msg::Base(BaseMsg::Paste)),
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
                            Ok(note) => nav.write().push(Screen::View.to_route(Some(note))),
                            Err(e) => message.set(Some(Msg::Error(e))),
                        }
                    }),
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
    }
}

#[component]
fn CipherRadio<
    T: IconShape + Clone + PartialEq + 'static,
>(
    cipher: Option<CipherType>,
    value: Option<CipherType>,
    icon: T,
    label: String,
    on_change: EventHandler<()>,
) -> Element {
    rsx! {
        input {
            r#type: "radio",
            checked: cipher == value,
            onchange: move |_| on_change.call(()),
        }
        label { onclick: move |_| on_change.call(()),
            Icon { icon }
            "{label}"
        }
    }
}
