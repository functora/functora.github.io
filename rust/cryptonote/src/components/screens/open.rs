use crate::messages::*;
use crate::*;

#[component]
pub fn Open(note: Option<String>) -> Element {
    let tst = use_context::<Store<TemporaryState>>();
    let nav = use_context::<Signal<Nav<Route>>>();
    let lang = use_lang();
    let mut message = use_message();
    let external = tst.external()();
    let is_encrypted = match external {
        External::Note(n) => matches!(n.data, NoteData::CipherText(_)),
        External::Archive(_) => true,
        External::Nothing => false,
    };

    use_effect(move || {
        if let Some(n) = &note {
            if !n.is_empty() {
                match encoding::decode_note(n) {
                    Ok(note_data) => match note_data {
                        NoteData::CipherText(enc) => {
                            tst.external().set(External::Note(ExternalNote {
                                data: NoteData::CipherText(enc),
                                url: String::new(),
                                qr: String::new(),
                            }));
                        }
                        NoteData::PlainText(text) => {
                            tst.note().set(text);
                            tst.cipher().set(None);
                            tst.external().set(External::Nothing);
                        }
                    },
                    Err(e) => message.set(Some(Msg::Error(e))),
                }
                return;
            }
        }
        if !matches!(tst.external()(), External::Nothing) {
            return;
        }
        if tst.note()().is_empty() {
            message.set(Some(Msg::Error(AppError::NoNoteInUrl)));
        }
    });

    let mut decrypt_note = move || {
        message.set(None);
        let pwd = tst.password()();
        if pwd.is_empty() {
            message.set(Some(Msg::Base(BaseMsg::PasswordRequired)));
            return;
        }
        match tst.external()() {
            External::Note(p) => {
                if let NoteData::CipherText(enc) = p.data {
                    let cipher = enc.cipher;
                    let nav = nav;
                    spawn(async move {
                        let mut nav = nav;
                        let mut message = message;
                        match crate::worker::run(
                            (enc, pwd.clone()),
                            tst.progress(),
                            |(enc, pwd), mut report| async move {
                                report(Job {
                                    stage: Stage::Decrypt,
                                    done: 0,
                                    total: 1,
                                    name: None,
                                });
                                decrypt_symmetric(&enc, &pwd)
                            },
                        )
                        .await
                        {
                            Ok(plaintext) => match String::from_utf8(plaintext) {
                                Ok(text) => {
                                    tst.progress().set(None);
                                    tst.note().set(text);
                                    tst.password().set(pwd);
                                    tst.cipher().set(Some(cipher));
                                    tst.external().set(External::Nothing);
                                    nav.write().push(Screen::View.to_route(None));
                                }
                                Err(e) => {
                                    tst.progress().set(None);
                                    message.set(Some(Msg::Error(AppError::Utf8(e))));
                                }
                            },
                            Err(e) => {
                                tst.progress().set(None);
                                message.set(Some(Msg::Error(e)));
                            }
                        }
                    });
                }
            }
            External::Archive(archive) => {
                let pwd = pwd.clone();
                let nav = nav;
                spawn(async move {
                    let mut nav = nav;
                    let mut message = message;
                    match extract_archive_package_async(&archive, &pwd, tst.progress()).await {
                        Ok((text, files)) => {
                            clear_progress(tst.progress());
                            tst.note().set(text);
                            tst.attachments().set(files);
                            tst.external().set(External::Nothing);
                            nav.write().push(Screen::View.to_route(None));
                        }
                        Err(e) => {
                            clear_progress(tst.progress());
                            message.set(Some(Msg::Error(e)));
                        }
                    }
                });
            }
            External::Nothing => {}
        }
    };

    rsx! {
        if is_encrypted {
            Breadcrumb { title: Msg::EncryptedNote }
            section {
                Pre {
                    code { "{Msg::EncryptedNoteDesc.render(lang)}" }
                }

                label { "{Msg::Base(BaseMsg::Password).render(lang)}" }
                input {
                    r#type: "password",
                    autocomplete: "off",
                    placeholder: "{Msg::Base(BaseMsg::PasswordPlaceholder).render(lang)}",
                    value: "{tst.password()}",
                    oninput: move |evt| tst.password().set(evt.value()),
                    onkeydown: move |evt| {
                        if evt.key() == Key::Enter {
                            decrypt_note()
                        }
                    },
                }

                Dock { message,
                    Button {
                        icon: Some(FaPaste),
                        onclick: move |_| read_clipboard(move |text| tst.password().set(text), message),
                        i18n: Some(Msg::Base(BaseMsg::Paste)),
                        lang,
                    }
                    Button {
                        icon: Some(FaLockOpen),
                        primary: true,
                        onclick: move |_| decrypt_note(),
                        i18n: Some(Msg::DecryptButton),
                        lang,
                    }
                    Button {
                        icon: Some(FaXmark),
                        onclick: move |_| tst.password().set(String::new()),
                        i18n: Some(Msg::Clear),
                        lang,
                    }
                }
            }
        } else {
            Breadcrumb { title: Msg::Note }
            NoteDisplay {}
        }
    }
}
