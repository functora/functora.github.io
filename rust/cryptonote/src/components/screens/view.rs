use crate::messages::*;
use crate::*;

#[component]
pub fn View(note: Option<String>) -> Element {
    let nav = use_context::<Signal<Nav<Route>>>();
    let tst = use_context::<Store<TemporaryState>>();
    let lang = use_lang();
    let mut message = use_message();
    let rendered = use_memo(move || render_markdown(&tst.note()()));
    let atts = tst.attachments()();
    let has_attachments = !atts.is_empty();
    let is_encrypted = tst.encrypted_note()().is_some() || tst.encrypted_archive()().is_some();

    use_effect(move || {
        if let Some(n) = &note {
            if !n.is_empty() {
                match encoding::decode_note(n) {
                    Ok(note_data) => match note_data {
                        NoteData::CipherText(enc) => {
                            tst.encrypted_note().set(Some(enc));
                        }
                        NoteData::PlainText(text) => {
                            tst.note().set(text);
                            tst.cipher().set(None);
                        }
                    },
                    Err(e) => message.set(Some(Msg::Error(e))),
                }
                return;
            }
        }
        if tst.encrypted_archive()().is_some() {
            return;
        }
        if tst.note()().is_empty() {
            message.set(Some(Msg::Error(AppError::NoNoteInUrl)));
        }
    });

    let mut download_all = move || {
        let files = tst.attachments()();
        if let Ok(zip) = create_zip(&files) {
            match download_package(zip, "cryptonote-unlocked.zip") {
                Ok(loc) => message.set(Some(Msg::Downloaded(loc))),
                Err(e) => message.set(Some(Msg::Error(AppError::FunctoraDioxus(functora_dioxus::Error::IO(
                    e,
                ))))),
            }
        }
    };

    let mut decrypt_note = move || {
        message.set(None);
        let pwd = tst.password()();
        if pwd.is_empty() {
            message.set(Some(Msg::Base(BaseMsg::PasswordRequired)));
            return;
        }
        let enc_data = tst.encrypted_note()();
        if let Some(enc) = enc_data {
            match decrypt_symmetric(&enc, &pwd) {
                Ok(plaintext) => match String::from_utf8(plaintext) {
                    Ok(text) => {
                        tst.note().set(text);
                        tst.password().set(pwd);
                        tst.cipher().set(Some(enc.cipher));
                        tst.encrypted_note().set(None);
                    }
                    Err(e) => message.set(Some(Msg::Error(AppError::Utf8(e)))),
                },
                Err(e) => message.set(Some(Msg::Error(e))),
            }
        } else if let Some(archive) = tst.encrypted_archive()() {
            match extract_archive_package(&archive, &pwd) {
                Ok((text, files)) => {
                    tst.note().set(text);
                    tst.attachments().set(files);
                    tst.encrypted_archive().set(None);
                }
                Err(e) => message.set(Some(Msg::Error(e))),
            }
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
            section {
                card {
                    overflow_wrap: "anywhere",
                    word_break: "break-word",
                    dangerous_inner_html: "{rendered()}",
                }

                if has_attachments {
                    table {
                        thead {
                            tr {
                                th { colspan: "3", "{Msg::Attachments.render(lang)}" }
                            }
                        }
                        tbody {
                            for f in &atts {
                                tr { key: "{f.name}",
                                    td { "{f.name}" }
                                    td { "txt": "r", "{format_size(f.data.len() as u64)}" }
                                    td {
                                        button {
                                            onclick: {
                                                let data = f.data.clone();
                                                let name = f.name.clone();
                                                move |_| {
                                                    match download_package(data.clone(), &name) {
                                                        Ok(loc) => message.set(Some(Msg::Downloaded(loc))),
                                                        Err(e) => {
                                                            message
                                                                .set(
                                                                    Some(
                                                                        Msg::Error(
                                                                            AppError::FunctoraDioxus(functora_dioxus::Error::IO(e)),
                                                                        ),
                                                                    ),
                                                                )
                                                        }
                                                    }
                                                }
                                            },
                                            Icon { icon: FaDownload }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                Dock { message,
                    Button {
                        icon: Some(FaCopy),
                        primary: true,
                        onclick: move |_| write_clipboard(tst.note()(), message),
                        i18n: Some(Msg::Base(BaseMsg::Copy)),
                        lang,
                    }
                    if has_attachments {
                        Button {
                            icon: Some(FaDownload),
                            primary: true,
                            onclick: move |_| download_all(),
                            i18n: Some(Msg::DownloadAll),
                            lang,
                        }
                    }
                    Button {
                        icon: Some(FaPenToSquare),
                        onclick: edit_handler(tst, nav),
                        i18n: Some(Msg::EditNote),
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
        }
    }
}
