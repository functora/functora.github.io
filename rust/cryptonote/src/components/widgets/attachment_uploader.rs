use crate::messages::Msg;
use crate::*;

#[component]
pub fn AttachmentUploader(tst: Store<TemporaryState>, lang: Language) -> Element {
    let mut nav = use_context::<Signal<Nav<Route>>>();
    let attachments = tst.attachments()();
    let has_attachments = !attachments.is_empty();

    if !has_attachments {
        return rsx! {};
    }
    rsx! {
        table {
            thead {
                tr {
                    th { "{Msg::FileName.render(lang)}" }
                    th { colspan: 2, "{Msg::FileSize.render(lang)}" }
                }
            }
            tbody {
                for (i, att) in attachments.iter().enumerate() {
                    tr { key: "{i}",
                        td {
                            a {
                                onclick: move |_| {
                                    tst.attachment().set(Some(i));
                                    nav.write().push(Screen::File.to_route(None));
                                },
                                "{att.name}"
                            }
                        }
                        td { "{format_size(att.data.len() as u64)}" }
                        td { "txt": "r",
                            button {
                                onclick: move |_| remove_attachment(tst, i),
                                aria_label: "{Msg::RemoveFile.render(lang)}",
                                Icon { icon: FaXmark }
                            }
                        }
                    }
                }
            }
        }
    }
}
