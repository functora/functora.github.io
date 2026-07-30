use crate::messages::*;
use crate::*;

#[component]
pub fn Share() -> Element {
    let mut nav = use_context::<Signal<Nav<Route>>>();
    let tst = use_context::<Store<TemporaryState>>();
    let lang = use_lang();

    let mut url = use_signal(String::new);
    let mut qr_code = use_signal(String::new);
    let mut pkg_ready = use_signal(|| false);
    let mut pkg_bytes = use_signal(Vec::<u8>::new);
    let mut message = use_message();

    use_effect(move || {
        let content = tst.content()();
        let password = tst.password()();
        let cipher = tst.cipher()();
        let atts = tst.attachments()();

        let has_atts = !atts.is_empty();

        let res: Result<(String, String, bool), AppError> = (|| {
            if has_atts {
                let cipher = cipher.ok_or(AppError::PasswordRequired)?;
                if password.is_empty() {
                    return Err(AppError::PasswordRequired);
                }
                let pkg = create_archive_package(&content, &atts, &password, cipher)?;
                pkg_bytes.set(pkg);
                Ok((String::new(), String::new(), true))
            } else {
                let note_data = match cipher {
                    Some(cipher) => {
                        if password.is_empty() {
                            return Err(AppError::PasswordRequired);
                        }
                        NoteData::CipherText(encrypt_symmetric(content.as_bytes(), &password, cipher)?)
                    }
                    None => NoteData::PlainText(content),
                };

                let origin = {
                    #[cfg(target_arch = "wasm32")]
                    {
                        web_sys::window().and_then(|w| {
                            let loc = w.location();
                            let protocol = loc.protocol().ok()?;
                            let host = loc.host().ok()?;
                            let pathname = loc.pathname().ok()?;
                            let path = pathname.trim_end_matches('/');
                            Some(format!("{}//{}{}", protocol, host, path))
                        })
                    }
                    #[cfg(not(target_arch = "wasm32"))]
                    {
                        Some(WEB_APP_URL)
                    }
                }
                .ok_or(AppError::NoNoteInUrl)?;

                let view_url = format!("{}/?screen={}", origin, Screen::View);
                let u = build_url(&view_url, &note_data)?;
                let q = generate_qr_code(&u)?;

                Ok((u, q, false))
            }
        })();

        match res {
            Ok((u, q, pkg)) => {
                url.set(u);
                qr_code.set(q);
                pkg_ready.set(pkg);
                message.set(None);
            }
            Err(e) => message.set(Some(Msg::Error(e))),
        }
    });

    rsx! {
        Breadcrumb { title: Msg::Share }
        section {
            if pkg_ready() {
                p { "{Msg::ArchiveReady.render(lang)}" }
            } else if !url().is_empty() {
                if !qr_code().is_empty() {
                    div { dangerous_inner_html: "{qr_code}" }
                }

                textarea {
                    readonly: true,
                    value: "{url}",
                    onclick: move |_| {
                        write_clipboard(url(), message);
                    },
                }
            } else if message.read().is_some() {
                Dock { message }
            } else {
                p { "{Msg::Base(BaseMsg::Loading).render(lang)}" }
            }

            if pkg_ready() || !url().is_empty() {
                Dock { message,
                    if !url().is_empty() {
                        Button {
                            icon: Some(FaCopy),
                            primary: true,
                            onclick: move |_| {
                                write_clipboard(url(), message);
                            },
                            i18n: Some(Msg::Base(BaseMsg::Copy)),
                            lang,
                        }
                        Button {
                            icon: Some(FaPrint),
                            primary: true,
                            onclick: move |_| {
                                let mut msg = message;
                                spawn(async move {
                                    if let Err(e) = print_page().await {
                                        msg.set(Some(Msg::Error(AppError::FunctoraDioxus(e))));
                                    }
                                });
                            },
                            i18n: Some(Msg::Print),
                            lang,
                        }
                        Button {
                            icon: Some(FaShareNodes),
                            primary: true,
                            onclick: move |_| {
                                let u = url();
                                let mut msg = message;
                                let text = Msg::SharedNoteText.render(lang);
                                spawn(async move {
                                    let data = ShareData {
                                        title: "Cryptonote".into(),
                                        text,
                                        url: u,
                                    };
                                    match web_share(data).await {
                                        Ok(()) => msg.set(Some(Msg::Sent)),
                                        Err(e) => msg.set(Some(Msg::Error(AppError::FunctoraDioxus(e)))),
                                    }
                                });
                            },
                            i18n: Some(Msg::Share),
                            lang,
                        }
                    }
                    if pkg_ready() {
                        Button {
                            icon: Some(FaDownload),
                            primary: true,
                            onclick: move |_| {
                                let bytes = pkg_bytes();
                                if !bytes.is_empty() {
                                    match download_package(bytes, "cryptonote_archive.cryptonote") {
                                        Ok(()) => message.set(Some(Msg::Sent)),
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
                            i18n: Some(Msg::DownloadAll),
                            lang,
                        }
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
