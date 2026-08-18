#![allow(clippy::shadow_reuse)]
use crate::messages::*;
use crate::*;
use zeroize::Zeroizing;

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

    let _ = use_effect(move || {
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
                    Err(e) => message.set(Some(Msg::Error(e.into()))),
                }
                return;
            }
        }
        if !matches!(tst.external()(), External::Nothing) {
            return;
        }
        if tst.note()().is_empty() {
            message.set(Some(Msg::Error(AppError::NoNoteInUrl.into())));
        }
    });

    let mut decrypt_note = move || {
        message.set(None);
        let pwd = Zeroizing::new(tst.password()());
        let pwd_required = pwd.is_empty();
        if pwd_required {
            message.set(Some(Msg::Base(BaseMsg::PasswordRequired)));
            return;
        }
        let Some(guard) = claim_job(tst.progress(), Stage::Decrypt) else {
            return;
        };
        match tst.external()() {
            External::Note(p) => {
                if let NoteData::CipherText(enc) = p.data {
                    let cipher = enc.cipher;
                    let _ = spawn_guarded(guard, async move {
                        let mut nav_out = nav;
                        let mut message_out = message;
                        match crate::worker::run(
                            (enc, pwd.clone()),
                            tst.progress(),
                            |(enc_in, pwd_in), mut report| async move {
                                report(Job {
                                    stage: Stage::Decrypt,
                                    done: 0,
                                    total: 1,
                                    name: None,
                                });
                                decrypt_symmetric(&enc_in, &pwd_in)
                            },
                        )
                        .await
                        {
                            Ok(plaintext) => match String::from_utf8(plaintext) {
                                Ok(text) => {
                                    tst.progress().set(None);
                                    tst.note().set(text);
                                    tst.password().set(pwd.to_string());
                                    tst.cipher().set(Some(cipher));
                                    tst.external().set(External::Nothing);
                                    nav_out.write().push(Screen::View.to_route(None));
                                }
                                Err(e) => {
                                    tst.progress().set(None);
                                    message_out.set(Some(Msg::Error(AppError::Utf8(e).into())));
                                }
                            },
                            Err(e) => {
                                tst.progress().set(None);
                                message_out.set(Some(Msg::Error(e.into())));
                            }
                        }
                    });
                }
            }
            External::Archive(src_archive) => {
                let _ = spawn_guarded(guard, async move {
                    let mut nav_out = nav;
                    let mut message_out = message;
                    let archive_bytes = src_archive.untag();
                    match extract_archive_package_async(ArchiveSource::Bytes(archive_bytes), &pwd, tst.progress()).await
                    {
                        Ok((text, files)) => {
                            clear_progress(tst.progress());
                            tst.note().set(text);
                            tst.attachments().set(files);
                            tst.external().set(External::Nothing);
                            nav_out.write().push(Screen::View.to_route(None));
                        }
                        Err(e) => {
                            clear_progress(tst.progress());
                            message_out.set(Some(Msg::Error(e.into())));
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
                        icon: Some(FaLockOpen),
                        primary: true,
                        onclick: move |_| decrypt_note(),
                        i18n: Some(Msg::DecryptButton),
                        lang,
                    }
                    Button {
                        icon: Some(FaPaste),
                        onclick: move |_| read_clipboard(move |text| tst.password().set(text), message),
                        i18n: Some(Msg::Base(BaseMsg::Paste)),
                        lang,
                    }
                    Button {
                        icon: Some(FaXmark),
                        onclick: move |_| tst.password().set(String::new()),
                        i18n: Some(Msg::Clear),
                        lang,
                    }
                    Button {
                        icon: Some(FaTrash),
                        onclick: reset_handler(tst, nav),
                        i18n: Some(Msg::CreateNewNote),
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
