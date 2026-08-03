use crate::messages::*;
use crate::*;

#[component]
pub fn Share() -> Element {
    let mut nav = use_context::<Signal<Nav<Route>>>();
    let tst = use_context::<Store<TemporaryState>>();
    let lang = use_lang();
    let message = use_message();

    let external = tst.external()();
    let pkg_ready = matches!(external, External::Archive(_));
    let (url, qr_code) = match external {
        External::Note(n) => (n.url, n.qr),
        _ => Default::default(),
    };

    rsx! {
        Breadcrumb { title: Msg::Share }
        section {
            if pkg_ready {
                p { "fs": "l", "txt": "c", "{Msg::ArchiveReady.render(lang)}" }
            } else if !url.is_empty() {
                if !qr_code.is_empty() {
                    div { dangerous_inner_html: "{qr_code}" }
                }

                textarea {
                    readonly: true,
                    value: "{url}",
                    onclick: move |_| {
                        write_clipboard(tst.external()().note_url(), message);
                    },
                }
            } else if message.read().is_some() {
                Dock { message }
            } else {
                p { "{Msg::Base(BaseMsg::Loading).render(lang)}" }
            }

            if pkg_ready || !url.is_empty() {
                Dock { message,
                    if !url.is_empty() {
                        Button {
                            icon: Some(FaCopy),
                            primary: true,
                            onclick: move |_| {
                                write_clipboard(tst.external()().note_url(), message);
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
                                let u = tst.external()().note_url();
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
                    if pkg_ready {
                        Button {
                            icon: Some(FaDownload),
                            primary: true,
                            onclick: move |_| {
                                let bytes = tst.external()().archive_bytes();
                                if !bytes.is_empty() {
                                    let progress = tst.progress();
                                    let message = message;
                                    spawn(async move {
                                        let mut progress = progress;
                                        let mut message = message;
                                        match download_package(bytes, "archive.cryptonote", progress).await {
                                            Ok(loc) => {
                                                progress.set(None);
                                                message.set(Some(Msg::Downloaded(loc)));
                                            }
                                            Err(e) => {
                                                progress.set(None);
                                                message
                                                    .set(
                                                        Some(
                                                            Msg::Error(
                                                                AppError::FunctoraDioxus(functora_dioxus::Error::IO(e)),
                                                            ),
                                                        ),
                                                    );
                                            }
                                        }
                                    });
                                }
                            },
                            i18n: Some(Msg::Download),
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
