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
                            th { "{Msg::FileSize.render(lang)}" }
                            th {}
                        }
                    }
                    tbody {
                        for f in &atts {
                            tr { key: "{f.name}",
                                td { "{f.name}" }
                                td { "{format_size(f.data.len() as u64)}" }
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
