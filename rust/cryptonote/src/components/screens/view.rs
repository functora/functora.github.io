use crate::messages::*;
use crate::*;

#[component]
pub fn View(note: Option<String>) -> Element {
    let nav = use_context::<Signal<Nav<Route>>>();
    let tst = use_context::<Store<TemporaryState>>();
    let lang = use_lang();
    let mut message = use_message();
    let rendered = use_memo(move || tst.view().note_content()().as_deref().map(render_markdown));
    let extracted = tst.extracted_files()();
    let has_attachments = extracted.iter().any(|f| f.name != "note.txt");

    use_effect(move || {
        if let Some(n) = &note {
            if !n.is_empty() {
                match encoding::decode_note(n) {
                    Ok(note_data) => match note_data {
                        NoteData::CipherText(enc) => {
                            tst.view().is_encrypted().set(true);
                            tst.view().encrypted_data().set(Some(enc));
                        }
                        NoteData::PlainText(text) => {
                            tst.view().note_content().set(Some(text.clone()));
                            tst.content().set(text);
                            tst.cipher().set(None);
                        }
                    },
                    Err(e) => message.set(Some(Msg::Error(e))),
                }
                return;
            }
        }
        if tst.archive_meta()().is_some() {
            return;
        }
        let content = tst.content()();
        if content.is_empty() {
            message.set(Some(Msg::Error(AppError::NoNoteInUrl)));
        } else {
            tst.view().note_content().set(Some(content));
        }
    });

    let mut download_all = move || {
        let files = tst.extracted_files()();
        if let Ok(zip) = create_zip(&files) {
            match download_package(zip, "cryptonote-unlocked.zip") {
                Ok(loc) => message.set(Some(Msg::Downloaded(loc))),
                Err(e) => message.set(Some(Msg::Error(AppError::FunctoraDioxus(functora_dioxus::Error::IO(e))))),
            }
        }
    };

    let mut decrypt_note = move || {
        message.set(None);
        let pwd = tst.view().password_input()();
        if pwd.is_empty() {
            message.set(Some(Msg::Base(BaseMsg::PasswordRequired)));
            return;
        }

        let enc_data = tst.view().encrypted_data()();
        if let Some(enc) = enc_data {
            match decrypt_symmetric(&enc, &pwd) {
                Ok(plaintext) => match String::from_utf8(plaintext) {
                    Ok(text) => {
                        tst.view().note_content().set(Some(text.clone()));
                        tst.view().is_encrypted().set(false);
                        tst.content().set(text);
                        tst.password().set(pwd);
                        tst.cipher().set(Some(enc.cipher));
                    }
                    Err(e) => message.set(Some(Msg::Error(AppError::Utf8(e)))),
                },
                Err(e) => message.set(Some(Msg::Error(e))),
            }
        } else if let Some(bytes) = tst.archive_bytes()() {
            match extract_archive_package(&bytes, &pwd) {
                Ok(files) => {
                    if let Some(note_file) = files.iter().find(|f| f.name == "note.txt") {
                        if let Ok(text) = String::from_utf8(note_file.data.clone()) {
                            tst.view().note_content().set(Some(text.clone()));
                            tst.content().set(text);
                        }
                    }
                    tst.extracted_files().set(files);
                    tst.view().is_encrypted().set(false);
                }
                Err(e) => message.set(Some(Msg::Error(e))),
            }
        }
    };

    rsx! {
        if tst.view().is_encrypted()() {
            Breadcrumb { title: Msg::EncryptedNote }
            section {
                Pre {
                    code { "{Msg::EncryptedNoteDesc.render(lang)}" }
                }

                label { "{Msg::Base(BaseMsg::Password).render(lang)}" }
                input {
                    r#type: "password",
                    placeholder: "{Msg::Base(BaseMsg::PasswordPlaceholder).render(lang)}",
                    value: "{tst.view().password_input()}",
                    oninput: move |evt| tst.view().password_input().set(evt.value()),
                    onkeydown: move |evt| {
                        if evt.key() == Key::Enter {
                            decrypt_note()
                        }
                    },
                }

                Dock { message,
                    Button {
                        icon: Some(FaPaste),
                        onclick: move |_| read_clipboard(move |text| tst.view().password_input().set(text), message),
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
                        onclick: move |_| tst.view().password_input().set(String::new()),
                        i18n: Some(Msg::Clear),
                        lang,
                    }
                }
            }
        } else if let Some(content) = tst.view().note_content()() {
            Breadcrumb { title: Msg::Note }
            section {
                card {
                    overflow_wrap: "anywhere",
                    word_break: "break-word",
                    dangerous_inner_html: "{rendered().unwrap_or_default()}",
                }

                for f in &extracted {
                    if f.name != "note.txt" {
                        div { key: "{f.name}",
                            {format!("{} ({}) ", f.name.strip_prefix("attachments/").unwrap_or(&f.name), format_size(f.data.len() as u64))}
                            button {
                                onclick: {
                                    let data = f.data.clone();
                                    let name = f.name.strip_prefix("attachments/").unwrap_or(&f.name).to_string();
                                    move |_| {
                                        match download_package(data.clone(), &name) {
                                            Ok(loc) => message.set(Some(Msg::Downloaded(loc))),
                                            Err(e) => message.set(Some(Msg::Error(AppError::FunctoraDioxus(functora_dioxus::Error::IO(e))))),
                                        }
                                    }
                                },
                                Icon { icon: FaDownload }
                            }
                        }
                    }
                }

                Dock { message,
                    Button {
                        icon: Some(FaCopy),
                        primary: true,
                        onclick: move |_| write_clipboard(content.clone(), message),
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
        } else if message.read().is_some() {
            Breadcrumb { title: Msg::Base(BaseMsg::ErrorTitleLabel) }
            section {
                Dock { message }
            }
        } else {
            section {
                p { "{Msg::Base(BaseMsg::Loading).render(lang)}" }
            }
        }
    }
}
