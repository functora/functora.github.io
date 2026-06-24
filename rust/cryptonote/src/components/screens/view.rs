use crate::messages::*;
use crate::*;

#[component]
pub fn View(note: Option<String>) -> Element {
    let mut nav = use_context::<Signal<Nav<Route>>>();
    let mut tst = use_context::<Store<TemporaryState>>();
    let lang = use_lang();
    let mut message = use_signal(|| Option::<Msg>::None);
    let rendered = use_memo(move || {
        tst.view().note_content()()
            .as_deref()
            .map(render_markdown)
    });

    use_effect(move || {
        if let Some(n) = &note {
            if !n.is_empty() {
                match encoding::decode_note(n) {
                    Ok(note_data) => match note_data {
                        NoteData::CipherText(enc) => {
                            tst.view()
                                .is_encrypted()
                                .set(true);
                            tst.view()
                                .encrypted_data()
                                .set(Some(enc));
                        }
                        NoteData::PlainText(text) => {
                            tst.view()
                                .note_content()
                                .set(Some(text.clone()));
                            tst.content().set(text);
                            tst.cipher().set(None);
                        }
                    },
                    Err(e) => message.set(Some(
                        Msg::Error(e.render(lang)),
                    )),
                }
                return;
            }
        }
        let content = tst.content()();
        if content.is_empty() {
            message.set(Some(Msg::Error(
                AppError::NoNoteInUrl.render(lang),
            )));
        } else {
            tst.view().note_content().set(Some(content));
        }
    });

    let mut decrypt_note = move || {
        message.set(None);
        let enc_data = tst.view().encrypted_data()();
        if let Some(enc) = enc_data {
            let pwd = tst.view().password_input()();
            if pwd.is_empty() {
                message.set(Some(Msg::PasswordRequired));
                return;
            }

            match decrypt_symmetric(&enc, &pwd) {
                Ok(plaintext) => {
                    match String::from_utf8(plaintext) {
                        Ok(text) => {
                            tst.view()
                                .note_content()
                                .set(Some(text.clone()));
                            tst.view()
                                .is_encrypted()
                                .set(false);
                            tst.content().set(text);
                            tst.password().set(pwd);
                            tst.cipher()
                                .set(Some(enc.cipher));
                        }
                        Err(e) => {
                            message.set(Some(Msg::Error(
                                AppError::Utf8(e)
                                    .render(lang),
                            )))
                        }
                    }
                }
                Err(e) => message
                    .set(Some(Msg::Error(e.render(lang)))),
            }
        }
    };

    rsx! {
            if tst.view().is_encrypted()() {
                Breadcrumb { title: Msg::EncryptedNote }
                section {
                    fieldset {
                        Pre {
                            Quote { "{Msg::EncryptedNoteDesc.render(lang)}" }
                        }

                        label { "{Msg::Password.render(lang)}" }
                        input {
                            r#type: "password",
                            placeholder: "{Msg::PasswordPlaceholder.render(lang)}",
                            value: "{tst.view().password_input()}",
                            oninput: move |evt| tst.view().password_input().set(evt.value()),
                            onkeydown: move |evt| {
                                if evt.key() == Key::Enter {
                                    decrypt_note()
                                }
                            },
                        }

                        br {}

                        Dock { message,
                            Button {
                                icon: FaPaste,
                                onclick: move |_| {
                                    spawn(async move {
                                        match js_read_clipboard().await {
                                            Ok(text) => tst.view().password_input().set(text),
    Err(e) => {
                        message.set(Some(Msg::Error(AppError::Fd(e).render(lang))))
                    }
                                        }
                                    });
                                },
                                "{Msg::Paste.render(lang)}"
                            }
                            Button {
                                icon: FaLockOpen,
                                primary: true,
                                onclick: move |_| decrypt_note(),
                                "{Msg::DecryptButton.render(lang)}"
                            }
                        }
                    }
                }
            } else if let Some(content) = tst.view().note_content()() {
                Breadcrumb { title: Msg::YourNoteTitle }
                section {
                    fieldset {
                        article {
                            div {
                                overflow_wrap: "anywhere",
                                word_break: "break-word",
                                dangerous_inner_html: "{rendered().unwrap_or_default()}",
                            }
                        }

                        Dock { message,
                            Button {
                                icon: FaTrash,
                                onclick: move |_| {
                                    tst.set(TemporaryState::default());
                                    nav.write().push(Screen::Home.to_route(None));
                                },
                                "{Msg::CreateNewNote.render(lang)}"
                            }
                            Button {
                                icon: FaPenToSquare,
                                onclick: move |_| {
                                    tst.action().set(ActionMode::Create);
                                    nav.write().push(Screen::Home.to_route(None));
                                },
                                "{Msg::EditNote.render(lang)}"
                            }
                            Button {
                                icon: FaCopy,
                                primary: true,
                                onclick: move |_| {
                                    write_clipboard(content.clone(), message, Msg::Copied, |_e| Msg::ClipboardWriteError);
                                },
                                "{Msg::Copy.render(lang)}"
                            }
                        }
                    }
                }
            } else if message.read().is_some() {
                Breadcrumb { title: Msg::ErrorTitle }
                section {
                    Dock { message }
                }
            } else {
                section {
                    p { "{Msg::Loading.render(lang)}" }
                }
            }
        }
}
