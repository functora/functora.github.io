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
        fieldset { margin_bottom: "1rem",
            legend { "{Msg::Attachments.render(lang)}" }
            ul {
                for (i, att) in attachments.iter().enumerate() {
                    li { key: "{i}",
                        Icon { icon: FaFile }
                        " {att.name} ({format_size(att.data.len() as u64)}) "
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
