use crate::messages::*;
use crate::*;

#[component]
pub fn View(note: Option<String>) -> Element {
    let mut nav = use_app_nav();
    let mut ctx = use_context::<Signal<AppCtx>>();
    let lang = use_lang();
    let mut note_content =
        use_signal(|| Option::<String>::None);
    let mut encrypted_data =
        use_signal(|| Option::<EncryptedData>::None);
    let mut password_input = use_signal(String::new);
    let mut message =
        use_signal(|| Option::<UiMessage>::None);
    let mut is_encrypted = use_signal(|| false);
    let rendered = use_memo(move || {
        note_content
            .read()
            .as_ref()
            .map(String::as_str)
            .map(render_markdown)
    });

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
                            ctx.write().content = text;
                            ctx.write().cipher = None;
                        }
                    },
                    Err(e) => message
                        .set(Some(UiMessage::Error(e))),
                }
                return;
            }
        }
        let content = ctx.read().content.clone();
        if content.is_empty() {
            message.set(Some(UiMessage::Error(
                AppError::NoNoteInUrl,
            )));
        } else {
            note_content.set(Some(content));
        }
    });

    let mut decrypt_note = move || {
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
                            ctx.write().content = text;
                            ctx.write().password = pwd;
                            ctx.write().cipher =
                                Some(enc.cipher);
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
                Breadcrumb { title: MsgEncryptedNote }
                section {
                    fieldset {
                        Pre {
                            Quote { "{MsgEncryptedNoteDesc.render(lang)}" }
                        }

                        label { "{MsgPassword.render(lang)}" }
                        input {
                            r#type: "password",
                            placeholder: "{MsgPasswordPlaceholder.render(lang)}",
                            value: "{password_input}",
                            oninput: move |evt| password_input.set(evt.value()),
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
                                            Ok(text) => password_input.set(text),
    Err(e) => {
                        message.set(Some(UiMessage::Error(e.into())))
                    }
                                        }
                                    });
                                },
                                "{MsgPasteButton.render(lang)}"
                            }
                            Button {
                                icon: FaLockOpen,
                                primary: true,
                                onclick: move |_| decrypt_note(),
                                "{MsgDecryptButton.render(lang)}"
                            }
                        }
                    }
                }
            } else if let Some(content) = note_content() {
                Breadcrumb { title: MsgYourNoteTitle }
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
                                    ctx.set(AppCtx::default());
                                    nav.write().push(Screen::Home.to_route(None));
                                },
                                "{MsgCreateNewNote.render(lang)}"
                            }
                            Button {
                                icon: FaPenToSquare,
                                onclick: move |_| {
                                    ctx.write().action = ActionMode::Create;
                                    nav.write().push(Screen::Home.to_route(None));
                                },
                                "{MsgEditNote.render(lang)}"
                            }
                            Button {
                                icon: FaCopy,
                                primary: true,
                                onclick: move |_| {
                                    write_clipboard(content.clone(), message);
                                },
                                "{MsgCopyButton.render(lang)}"
                            }
                        }
                    }
                }
            } else if message.read().is_some() {
                Breadcrumb { title: MsgErrorTitle }
                section {
                    Dock { message }
                }
            } else {
                section {
                    p { "{MsgLoading.render(lang)}" }
                }
            }
        }
}
