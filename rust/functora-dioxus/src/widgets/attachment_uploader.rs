#![allow(clippy::shadow_reuse)]
use crate::files::{Attachment, Preview, format_size};
use crate::widgets::AttachmentPreview;
use dioxus::prelude::*;
use dioxus_free_icons::Icon;
use dioxus_free_icons::icons::fa_solid_icons::FaXmark;

#[component]
pub fn AttachmentUploader(
    attachments: Vec<Attachment>,
    previews: Vec<Preview>,
    remove_file: String,
    on_open: EventHandler<usize>,
    on_remove: EventHandler<usize>,
) -> Element {
    if attachments.is_empty() {
        rsx! {}
    } else {
        rsx! {
        table {
            tbody {
                for (i, (att, prev)) in attachments.iter().zip(previews.iter()).enumerate() {
                    tr { key: "{i}",
                        td {
                            a {
                                onclick: move |_| on_open.call(i),
                                "{att.name} ({format_size(att.data.len() as u64)})",
                            }
                        }
                        td {
                            AttachmentPreview {
                                name: att.name.clone(),
                                preview: prev.clone(),
                                onclick: move |_| on_open.call(i),
                            }
                        }
                        td { "txt": "r",
                            button {
                                onclick: move |_| on_remove.call(i),
                                aria_label: "{remove_file}",
                                Icon { icon: FaXmark }
                            }
                        }
                    }
                }
            }
        }
        }
    }
}
