use crate::messages::*;
use crate::*;

#[component]
pub fn NoteDisplay() -> Element {
    let nav = use_context::<Signal<Nav<Route>>>();
    let tst = use_context::<Store<TemporaryState>>();
    let lang = use_lang();
    let mut message = use_message();
    let rendered = use_memo(move || render_markdown(&tst.note()()));
    let atts = tst.attachments()();
    let has_attachments = !atts.is_empty();

    let download_all = move || {
        let files = tst.attachments()();
        let progress = tst.progress();
        let message = message;
        spawn(async move {
            let mut progress = progress;
            let mut message = message;
            match create_zip_async(&files, progress).await {
                Ok(zip) => match download_package(zip, "cryptonote-unlocked.zip", progress).await {
                    Ok(loc) => {
                        progress.set(None);
                        message.set(Some(Msg::Downloaded(loc)));
                    }
                    Err(e) => {
                        progress.set(None);
                        message.set(Some(Msg::Error(AppError::FunctoraDioxus(functora_dioxus::Error::IO(
                            e,
                        )))));
                    }
                },
                Err(e) => {
                    progress.set(None);
                    message.set(Some(Msg::Error(e)));
                }
            }
        });
    };

    let share_note = move |_| {
        message.set(None);
        if let Some(msg) = share_error(tst.cipher()(), &tst.password()()) {
            message.set(Some(msg));
            return;
        }
        let nav = nav;
        let message = message;
        spawn(async move {
            let mut nav = nav;
            let mut message = message;
            if matches!(tst.external()(), External::Nothing) {
                match generate_share_async(tst).await {
                    Ok(()) => nav.write().push(Screen::Share.to_route(None)),
                    Err(e) => {
                        tst.progress().set(None);
                        message.set(Some(Msg::Error(e)));
                    }
                }
            } else {
                nav.write().push(Screen::Share.to_route(None));
            }
        });
    };

    rsx! {
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
                            th { "{Msg::FileName.render(lang)}" }
                            th { colspan: 2, "{Msg::FileSize.render(lang)}" }
                        }
                    }
                    tbody {
                        for (i, f) in atts.iter().enumerate() {
                            tr { key: "{f.name}",
                                td { "{f.name}" }
                                td { "{format_size(f.data.len() as u64)}" }
                                td { "txt": "r",
                                    button {
                                        onclick: move |_| {
                                            let att = tst.attachments()()[i].clone();
                                            let progress = tst.progress();
                                            let message = message;
                                            spawn(async move {
                                                let mut progress = progress;
                                                let mut message = message;
                                                match download_package(att.data, &att.name, progress).await {
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
                Button {
                    icon: Some(FaShareNodes),
                    primary: true,
                    onclick: share_note,
                    i18n: Some(Msg::Share),
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
