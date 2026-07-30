use crate::messages::Msg;
use crate::*;

#[component]
pub fn AttachmentUploader(tst: Store<TemporaryState>, lang: Language) -> Element {
    let attachments = tst.attachments()();
    let has_attachments = !attachments.is_empty();

    if !has_attachments {
        return rsx! {};
    }
    rsx! {
        table {
            thead {
                tr {
                    th { colspan: "3", "{Msg::Attachments.render(lang)}" }
                }
            }
            tbody {
                for (i, att) in attachments.iter().enumerate() {
                    tr { key: "{i}",
                        td { " {att.name}" }
                        td { "txt": "r", "{format_size(att.data.len() as u64)}" }
                        td {
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
