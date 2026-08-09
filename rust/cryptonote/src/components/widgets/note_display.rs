use crate::messages::*;
use crate::*;
use functora_dioxus::widgets::AttachmentPreview;

#[component]
pub fn NoteDisplay() -> Element {
    let mut nav = use_context::<Signal<Nav<Route>>>();
    let tst = use_context::<Store<TemporaryState>>();
    let lang = use_lang();
    let mut message = use_message();
    let rendered = use_memo(move || render_markdown(&tst.note()()));
    let atts = tst.attachments()();
    let has_attachments = !atts.is_empty();
    let previews = use_memo(move || {
        tst.attachments()()
            .iter()
            .map(|a| functora_dioxus::files::preview(&a.name, &a.data))
            .collect::<Vec<_>>()
    });

    let download_all = move || {
        let files = tst.attachments()();
        let progress = tst.progress();
        let _ = spawn(async move {
            let mut progress_out = progress;
            let mut message_out = message;
            match create_zip_async(&files, progress_out).await {
                Ok(zip) => match download_package(zip, "cryptonote-unlocked.zip", progress_out).await {
                    Ok(loc) => {
                        progress_out.set(None);
                        message_out.set(Some(Msg::Downloaded(loc)));
                    }
                    Err(e) => {
                        progress_out.set(None);
                        message_out.set(Some(Msg::Error(AppError::FunctoraDioxus(e).into())));
                    }
                },
                Err(e) => {
                    progress_out.set(None);
                    message_out.set(Some(Msg::Error(e.into())));
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
        let _ = spawn(async move {
            let mut nav_out = nav;
            let mut message_out = message;
            if matches!(tst.external()(), External::Nothing) {
                match generate_share_async(tst).await {
                    Ok(()) => nav_out.write().push(Screen::Share.to_route(None)),
                    Err(e) => {
                        tst.progress().set(None);
                        message_out.set(Some(Msg::Error(e.into())));
                    }
                }
            } else {
                nav_out.write().push(Screen::Share.to_route(None));
            }
        });
    };

    let print_note = move |_| {
        let mut msg = message;
        let _ = spawn(async move {
            if let Err(e) = print_page().await {
                msg.set(Some(Msg::Error(AppError::FunctoraDioxus(e).into())));
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
                    tbody {
                        for (i, (f, p)) in atts.iter().zip(previews().iter()).enumerate() {
                            tr { key: "{f.name}",
                                td {
                                    a {
                                        onclick: move |_| {
                                            tst.attachment().set(Some(i));
                                            nav.write().push(Screen::File.to_route(None));
                                        },
                                        "{f.name} ({format_size(f.data.len() as u64)})"
                                    }
                                }
                                td {
                                    AttachmentPreview {
                                        name: f.name.clone(),
                                        preview: p.clone(),
                                    }
                                }
                                td { "txt": "r",
                                    button {
                                        onclick: move |_| {
                                            if let Some(att) = tst.attachments()().get(i).cloned() {
                                                download_attachment(att, tst.progress(), message);
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
                Button {
                    icon: Some(FaShareNodes),
                    primary: true,
                    onclick: share_note,
                    i18n: Some(Msg::Share),
                    lang,
                }
                Button {
                    icon: Some(FaPrint),
                    primary: true,
                    onclick: print_note,
                    i18n: Some(Msg::Print),
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
