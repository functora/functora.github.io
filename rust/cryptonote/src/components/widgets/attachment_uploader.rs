use crate::messages::Msg;
use crate::*;

#[component]
pub fn AttachmentUploader(tst: Store<TemporaryState>, lang: Language) -> Element {
    let attachments = tst.attachments()();
    let has_attachments = !attachments.is_empty();

    #[cfg(target_arch = "wasm32")]
    let picker = rsx! {
        label { "btn": true,
            input {
                r#type: "file",
                multiple: true,
                onchange: move |evt| handle_file_input(evt, tst),
            }
            Icon { icon: FaPaperclip }
            " {Msg::AttachFiles.render(lang)}"
        }
    };

    #[cfg(not(target_arch = "wasm32"))]
    let picker = rsx! {
        button { "btn": true, onclick: move |_| handle_file_input_native(tst),
            Icon { icon: FaPaperclip }
            " {Msg::AttachFiles.render(lang)}"
        }
    };

    rsx! {
        fieldset {
            legend { "{Msg::AttachFiles.render(lang)}" }
            {picker}
            if has_attachments {
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
}
