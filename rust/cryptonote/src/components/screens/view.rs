use crate::messages::*;
use crate::*;

#[component]
pub fn View(note: Option<String>) -> Element {
    let mut nav = use_context::<Signal<Nav<Route>>>();
    let mut tst = use_context::<Store<TemporaryState>>();
    let lang = use_lang();
    let mut message = use_message();
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
                            icon: Some(FaPaste),
                            onclick: move |_| {
                                paste_clipboard(
                                    move |text| tst.view().password_input().set(text),
                                    message,
                                    lang,
                                );
                            },
                            i18n: Some(Msg::Paste),
                            lang,
                        }
                        Button {
                            icon: Some(FaLockOpen),
                            primary: true,
                            onclick: move |_| decrypt_note(),
                            i18n: Some(Msg::DecryptButton),
                            lang,
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
                            icon: Some(FaTrash),
                            onclick: move |_| {
                                tst.set(TemporaryState::default());
                                nav.write().push(Screen::Home.to_route(None));
                            },
                            i18n: Some(Msg::CreateNewNote),
                            lang,
                        }
                        Button {
                            icon: Some(FaPenToSquare),
                            onclick: move |_| {
                                tst.action().set(ActionMode::Create);
                                nav.write().push(Screen::Home.to_route(None));
                            },
                            i18n: Some(Msg::EditNote),
                            lang,
                        }
                        Button {
                            icon: Some(FaCopy),
                            primary: true,
                            onclick: move |_| {
                                write_clipboard(
                                    content.clone(),
                                    message,
                                    Msg::Copied,
                                    |_e| Msg::ClipboardWriteError,
                                );
                            },
                            i18n: Some(Msg::Copy),
                            lang,
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
